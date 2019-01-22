package provingground.learning
import provingground.{FiniteDistribution => FD, _}
import shapeless.HList._
import shapeless._

import scala.language.higherKinds
import monix.eval._

import GeneratorVariables._

import scala.concurrent._, duration._

import MonixFiniteDistributionEq._

object MonixFiniteDistributionEq {
  def finalProb[Y](y: Y, rv: RandomVar[Y]) = FinalVal(Elem(y, rv))

  def initProb[Y](y: Y, rv: RandomVar[Y]) = InitialVal(Elem(y, rv))

}

abstract class GenMonixFiniteDistributionEq[State, Boat](
    nodeCoeffSeq: NodeCoeffSeq[State, Boat, Double])(
    implicit sd: StateDistribution[State, FD]) {
  import NodeCoeffs._
  import nodeCoeffSeq.find

  /**
    * finite distribution for a random variable
    * @param initState initial state
    * @param randomVar random variable whose distribution is returned
    * @param epsilon cutoff
    * @tparam Y values of the random variable
    * @return finite distribution for the given random variable
    */
  def varDist[Y](initState: State)(
      randomVar: RandomVar[Y],
      epsilon: Double,
      limit: FiniteDuration = 3.minutes): Task[(FD[Y], Set[EquationTerm])] =
    if (epsilon > 1) Task.now(FD.empty[Y] -> Set())
    else
      randomVar match {
        case RandomVar.AtCoord(randomVarFmly, fullArg) =>
//          varFamilyDist(initState)(randomVarFmly, epsilon)
//            .map(_.getOrElse(fullArg, FD.empty[Y]))
          varFamilyDistFunc(initState)(randomVarFmly, epsilon)(fullArg)
            .map{
              case (fd, eqs) => fd.flatten.safeNormalized -> eqs
            }
            .timeout(limit)
        case _ =>
          find(randomVar)
            .map { nc =>
              nodeCoeffDist(initState)(nc, epsilon).map{
                case (fd, eqs) => fd.flatten.safeNormalized -> eqs
              }
            }
            .getOrElse(Task.now(FD.empty[Y] -> Set.empty[EquationTerm]))
            .timeout(limit)
      }

  /**
    * finite distribution for a list of random variables
    * @param initState initial state
    * @param vl list of random variables
    * @param epsilon cutoff
    * @tparam Dom the `HList` giving the type of the variable list
    * @return finite distribution of `Dom`
    */
  def varListDist[Dom <: HList](initState: State)(
      vl: RandomVarList[Dom],
      epsilon: Double): Task[(FD[Dom], Set[EquationTerm])] =
    vl match {
      case RandomVarList.Nil => Task(FD.unif(HNil.asInstanceOf[Dom]) -> Set.empty[EquationTerm])
      case RandomVarList.Cons(head, tail) =>
        varDist(initState)(head, epsilon)
          .zip(varListDist(initState)(tail, epsilon))
          .map {
            case ((xfd, eqx), (yfd, eqy )) =>
              (for {
                x <- xfd
                y <- yfd
              } yield x :: y) -> (eqx union eqy)
          }
    }

  def nodeCoeffDist[Y](initState: State)(
      nodeCoeffs: NodeCoeffs[State, Boat, Double, HNil, Y],
      epsilon: Double): Task[(FD[Y], Set[EquationTerm])] =
    if (epsilon > 1) Task.now(FD.empty[Y] -> Set.empty[EquationTerm])
    else
      nodeCoeffs match {
        case Target(_) => Task.now(FD.empty[Y] -> Set.empty[EquationTerm])
        case bc: Cons[State, Boat, Double, HNil, Y] =>
          val p: Double = bc.headCoeff
          val d: Task[(FD[Y], Set[EquationTerm])] =
            bc.headGen match {
              case gen: GeneratorNode[Y] =>
                nodeDist(initState)(gen, epsilon / p) map {case (fd, eqs) => (fd *p, eqs)}
              case _ =>
                throw new IllegalArgumentException(
                  "found node family for generating a simple random variable")
            }
          for {
            pa <- d
            (a, eqa) = pa
            pb <- nodeCoeffDist(initState)(bc.tail, epsilon)
            (b, eqb) = pb
          } yield (a ++ b, eqa union eqb)
      }

  def mapsSum[X, Y](first: Map[X, (FD[Y], Set[EquationTerm])],
                    second: Map[X, (FD[Y], Set[EquationTerm])]): Map[X, (FD[Y], Set[EquationTerm])] =
    (for {
      k <- first.keySet union second.keySet
      v1 = first.getOrElse(k, FD.empty[Y] -> Set.empty[EquationTerm]) 
      v2 = second.getOrElse(k, FD.empty[Y] -> Set.empty[EquationTerm])
    } yield (k, (v1._1 ++ v2._1, v1._2 union v2._2))).toMap

  def nodeCoeffFamilyMap[Dom <: HList, Y](initState: State)(
      nodeCoeffs: NodeCoeffs[State, Boat, Double, Dom, Y],
      baseDist: Task[FD[Dom]],
      epsilon: Double): Task[Map[Dom, (FD[Y], Set[EquationTerm])]] =
    if (epsilon > 1) Task(Map())
    else
      nodeCoeffs match {
        case Target(_) => Task(Map())
        case bc: Cons[State, Boat, Double, Dom, Y] =>
          val p = bc.headCoeff
          for {
            a <- nodeFamilyDist(initState)(bc.headGen, baseDist, epsilon)
            b <- nodeCoeffFamilyMap(initState)(bc.tail,
                                               baseDist,
                                               epsilon / (1.0 - p))
          } yield mapsSum(a, b)
      }

  def nodeCoeffFamilyDist[Dom <: HList, Y](initState: State)(
      nodeCoeffs: NodeCoeffs[State, Boat, Double, Dom, Y],
      epsilon: Double)(arg: Dom): Task[(FD[Y], Set[EquationTerm])] =
    if (epsilon > 1) Task.now(FD.empty[Y] -> Set.empty[EquationTerm])
    else
      nodeCoeffs match {
        case Target(_) => Task.now(FD.empty[Y] -> Set.empty[EquationTerm])
        case bc: Cons[State, Boat, Double, Dom, Y] =>
          val p = bc.headCoeff
          for {
            pa <- nodeFamilyDistFunc(initState)(bc.headGen, epsilon)(arg)
            pb <- nodeCoeffFamilyDist(initState)(bc.tail, epsilon / (1.0 - p))(
              arg)
          } yield (pa._1 ++ pb._1, pa._2 union pb._2)
      }

  def varFamilyDist[RDom <: HList, Y](initState: State)(
      randomVarFmly: RandomVarFamily[RDom, Y],
      epsilon: Double): Task[Map[RDom, (FD[Y], Set[EquationTerm])]] =
    if (epsilon > 1) Task(Map())
    else
      find(randomVarFmly)
        .map { nc =>
          val base = varListDist(initState)(nc.output.polyDomain, epsilon)
          nodeCoeffFamilyMap(initState)(nc, base.map(_._1), epsilon)
        }
        .getOrElse(Task(Map()))

  def varFamilyDistFunc[RDom <: HList, Y](initState: State)(
      randomVarFmly: RandomVarFamily[RDom, Y],
      epsilon: Double)(arg: RDom): Task[(FD[Y], Set[EquationTerm])] =
    if (epsilon > 1) Task.now(FD.empty[Y] -> Set.empty[EquationTerm])
    else
      find(randomVarFmly)
        .map { nc =>
          nodeCoeffFamilyDist(initState)(nc, epsilon)(arg)
        }
        .getOrElse(Task.now(FD.empty[Y] -> Set.empty[EquationTerm]))

  def nodeFamilyDist[Dom <: HList, Y](initState: State)(
      generatorNodeFamily: GeneratorNodeFamily[Dom, Y],
      baseDist: Task[FD[Dom]],
      epsilon: Double): Task[Map[Dom, (FD[Y], Set[EquationTerm])]] =
    generatorNodeFamily match {
      case node: GeneratorNode[Y] =>
        nodeDist(initState)(node, epsilon)
          .map((t) => Map(HNil -> t))
      case f: GeneratorNodeFamily.Pi[Dom, Y] =>
        baseDist.flatMap { (bd) =>
          val kvs =
            for {
              Weighted(arg, p) <- bd.pmf
              dt = nodeDist(initState)(f.nodes(arg), epsilon / p) // actually a task
            } yield dt.map(d => arg -> d)
          Task.gather(kvs).map(_.toMap)
        }
      case f: GeneratorNodeFamily.PiOpt[Dom, Y] =>
        baseDist.flatMap { (bd) =>
          val kvs =
            for {
              Weighted(arg, p) <- bd.pmf
              node             <- f.nodesOpt(arg)
              dt = nodeDist(initState)(node, epsilon / p) // actually a task
            } yield dt.map(d => arg -> d)
          Task.gather(kvs).map(_.toMap)
        }
    }

  def nodeFamilyDistFunc[Dom <: HList, Y](initState: State)(
      generatorNodeFamily: GeneratorNodeFamily[Dom, Y],
      epsilon: Double)(arg: Dom): Task[(FD[Y], Set[EquationTerm])] =
    generatorNodeFamily match {
      case node: GeneratorNode[Y] =>
        assert(arg == HNil,
               s"looking for coordinate $arg in $node which is not a family")
        nodeDist(initState)(node, epsilon)
      case f: GeneratorNodeFamily.Pi[Dom, Y] =>
        nodeDist(initState)(f.nodes(arg), epsilon) // actually a task
      case f: GeneratorNodeFamily.PiOpt[Dom, Y] =>
        f.nodesOpt(arg)
          .map((node) => nodeDist(initState)(node, epsilon))
          .getOrElse(Task.pure(FD.empty[Y] -> Set.empty[EquationTerm]))
    }

  def nodeDist[Y](initState: State)(generatorNode: GeneratorNode[Y],
                                    epsilon: Double): Task[(FD[Y], Set[EquationTerm])]

}

/**
  * resolving a general specification of a recursive generative model as finite distributions, depending on truncation;
  * the coefficients of the various generator nodes should be `Double`
  *
  * @param nodeCoeffSeq the various generator nodes with coefficients for various random variables and families
  * @param sd finite distributions from the initial state corresponding to random variables and families
  * @tparam State scala type of the initial state
  */
case class MonixFiniteDistributionEq[State, Boat](
    nodeCoeffSeq: NodeCoeffSeq[State, Boat, Double])(
    implicit sd: StateDistribution[State, FD])
    extends GenMonixFiniteDistributionEq[State, Boat](nodeCoeffSeq) {

  /**
    * update coefficients, to be used in complex islands
    * @param dataSeq the new coefficients
    * @return [[MonixFiniteDistribution]] with updated coefficients
    */
  def updateAll(
      dataSeq: Seq[GeneratorNodeFamily.Value[_ <: HList, _, Double]]) =
    MonixFiniteDistributionEq(nodeCoeffSeq.updateAll(dataSeq))

  /**
    * recursively determines the finite distribution given a generator node;
    * the main work is done here
    *
    * @param initState  initial state
    * @param generatorNode generator node to resolve
    * @param epsilon cutoff
    * @tparam Y values of the corresponding random variable
    * @return distribution corresponding to the `output` random variable
    */
  def nodeDist[Y](initState: State)(generatorNode: GeneratorNode[Y],
                                    epsilon: Double): Task[(FD[Y], Set[EquationTerm])] =
    if (epsilon > 1) Task.now(FD.empty[Y] -> Set.empty[EquationTerm])
    else {
      import GeneratorNode._
      generatorNode match {
        case Atom(x, _) =>
          Task(FD.unif(x), Set.empty[EquationTerm])
        case Init(input) =>
          val initDist = sd.value(initState)(input)
          val eqs = initDist.support.map{x => EquationTerm(initProb(x, input), finalProb(x, input))}
          Task(initDist, eqs)
        case Map(f, input, _) =>
          varDist(initState)(input, epsilon).map{case (fd, eqs) => fd.map(f).purge(epsilon) -> eqs}
        case MapOpt(f, input, _) =>
          varDist(initState)(input, epsilon).map{case (fd, eqs) => fd.condMap(f).purge(epsilon) -> eqs}
        case ZipMap(f, input1, input2, _) =>
          val d1 = varDist(initState)(input1, epsilon).map{case (fd, eqs) => fd.flatten -> eqs}
          val d2 = varDist(initState)(input2, epsilon).map{case (fd, eqs) => fd.flatten -> eqs}
          d1.zip(d2).map {
            case ((xd, eqx), (yd, eqy)) =>
              xd.zip(yd).map { case (x, y) => f(x, y) }.purge(epsilon) -> (eqx union eqy)
          }
        case ZipMapOpt(f, input1, input2, _) =>
          val d1 = varDist(initState)(input1, epsilon).map{case (fd, eqs) => fd.flatten -> eqs}
          val d2 = varDist(initState)(input2, epsilon).map{case (fd, eqs) => fd.flatten -> eqs}
          d1.zip(d2).map {
            case ((xd, eqx), (yd, eqy)) =>
              xd.zip(yd).condMap { case (x, y) => f(x, y) }.purge(epsilon) -> (eqx union eqy)
          }
        case ZipFlatMap(baseInput, fiberVar, f, _) =>
          val baseDistT = varDist(initState)(baseInput, epsilon).map{case (fd, eqs) => fd.flatten -> eqs}
          baseDistT.flatMap {case (baseDist, baseEqs) =>
            val pmfT =
              baseDist.pmf
                .map {
                  case Weighted(x1, p1) =>
                    val fiberDistT =
                      varDist(initState)(fiberVar(x1), epsilon / p1)
                        .map{case (fd, eqs) => fd.flatten -> eqs}
                    val tv =
                      fiberDistT
                        .map { case (fiberDist, fiberEqs) =>
                          for {
                            Weighted(x2, p2) <- fiberDist.pmf
                          } yield Weighted(f(x1, x2), p1 * p2)
                        }
                    tv
                }
            Task.gather(pmfT).map((vv) => FD(vv.flatten) -> ???)
          }
        case FlatMap(baseInput, fiberNode, _) =>
          val baseDistT = varDist(initState)(baseInput, epsilon).map{case (fd, eqs) => fd.flatten -> eqs}
          baseDistT.flatMap {case (baseDist, baseEqs) =>
            val pmfT =
              baseDist.pmf
                .map {
                  case Weighted(x1, p1) =>
                    val node = fiberNode(x1)
                    val fiberDistT =
                      nodeDist(initState)(node, epsilon / p1)
                        .map{case (fd, eqs) => fd.flatten -> eqs}
                    fiberDistT
                      .map { case (fiberDist, fiberEqs) =>
                        fiberDist.pmf.map {
                          case Weighted(x2, p2) => Weighted(x2, p1 * p2)
                        }
                      }
                }
            Task.gather(pmfT).map((vv) => FD(vv.flatten) -> ???)
          }
        case FlatMapOpt(baseInput, fiberNodeOpt, _) =>
          val baseDistT = varDist(initState)(baseInput, epsilon).map{case (fd, eqs) => fd.flatten -> eqs}
          baseDistT.flatMap { case (baseDist, baseEqs) =>
            val pmfT =
              baseDist.pmf
                .flatMap {
                  case wt @ Weighted(x, _) => fiberNodeOpt(x).map(wt -> _)
                }
                .map {
                  case (Weighted(_, p1), node) =>
                    val fiberDistT =
                      nodeDist(initState)(node, epsilon / p1)
                        .map{case (fd, eqs) => fd.flatten -> eqs}
                    fiberDistT
                      .map { case (fiberDist: FD[Y], fiberEqs) =>
                        fiberDist.pmf.map {
                          case Weighted(x2, p2) => Weighted(x2, p1 * p2)
                        }
                      }
                }
            Task.gather(pmfT).map((vv) => FD(vv.flatten) -> ???)
          }
        case FiberProductMap(quot, fiberVar, f, baseInput, _) =>
          val d1T = varDist(initState)(baseInput, epsilon).map{case (fd, eqs) => fd.flatten -> eqs}
          d1T.flatMap { case (d1, d1E) =>
            val byBase      = d1.pmf.groupBy { case Weighted(x, _) => quot(x) } // pmfs grouped by terms in quotient
            val baseWeights = byBase.mapValues(v => v.map(_.weight).sum) // weights of terms in the quotient
            val pmfT =
              byBase.map {
                case (z, pmf1) => // `z` is in the base, `pmf1` is all terms above `z`
                  val d2T =
                    varDist(initState)(fiberVar(z), epsilon / baseWeights(z))
                      .map{case (fd, eqs) => fd.flatten -> eqs} // distribution of the fiber at `z`
                  d2T.map { case (d2, d2E) =>
                    val d = FD(pmf1)
                      .zip(d2)
                      .map { case (x1, x2) => f(x1, x2) }
                      .flatten // mapped distribution over `z`
                    d.pmf // terms in pmf over z, should be flatMapped over `z`
                  }
              }
            Task.gather(pmfT).map((vv) => FD(vv.flatten) -> ???)
          }
        case tc: ThenCondition[o, Y] =>
          import tc._
          val base = nodeDist(initState)(gen, epsilon)
          import Sort._
          condition match {
            case _: All[_]    => base
            case c: Filter[_] => base.map{case (fd, eqs) =>  fd.conditioned(c.pred).purge(epsilon) -> eqs}
            case Restrict(f)  => base.map{case (fd, eqs) => fd.condMap(f).purge(epsilon) -> eqs}
          }
        case isle: Island[Y, State, o, b] =>
          import isle._
          val (isleInit, boat) = initMap(initState)                             // initial condition for island, boat to row back
          val isleOut          = varDist(isleInit)(islandOutput(boat), epsilon) //result for the island
          isleOut
            .map{case (fd, eqs) => fd.map(export(boat, _)).purge(epsilon) -> eqs} // exported result seen outside
        case isle: ComplexIsland[o, Y, State, b, Double] =>
          import isle._
          val (isleInit, boat, isleCoeffs) = initMap(initState)
          val isleOut =
            updateAll(isleCoeffs.toSeq) // coefficients changed to those for the island
              .varDist(isleInit)(islandOutput(boat), epsilon)
          isleOut
          .map{case (fd, eqs) => fd.map(export(boat, _)).purge(epsilon) -> eqs} // exported result seen outside
      }
    }
}
