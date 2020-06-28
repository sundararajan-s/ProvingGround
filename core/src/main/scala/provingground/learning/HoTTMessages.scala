package provingground.learning
import provingground._, HoTT._
import provingground.induction.ExstInducDefn
import provingground.learning.HoTTMessages.Proved
import provingground.learning.HoTTMessages.Contradicted
import shapeless._, HList._

/**
  * Messages to be posted for autonomous/interactive running.
  * These can be posted by a user, rule-based bot or a deep-learning system;
  * from the point of view of deep learning, these are moves.
  *
  * Some messages are instructions for what to do next; some report results
  * and some provide data for use in the future.
  *
  *
  * Some types like `LocalProver`, `ExpressionEval`, `TermGenParams` etc can be posted in their raw form.
  */
object HoTTMessages {

  /**
    * goals to seek, through whatever means
    *
    * @param goals the goals
    */
  case class SeekGoals(goals: FiniteDistribution[Typ[Term]])

  /**
    * A goal to seek, often derived from others; if the others are solved or negated, we stop seeking
    *
    * @param goal the goal
    * @param forConsequences the consequences for which we seek this, if any; if empty these have no effect
    */
  case class SeekGoal(
      goal: Typ[Term],
      context: Context,
      forConsequences: Set[Typ[Term]] = Set()
  ) {
    def relevantGiven(terms: Set[Term]) =
      terms
        .map(_.typ)
        .intersect(
          forConsequences union forConsequences
            .map(negate) union Set(goal, negate(goal))
        )
        .isEmpty

    def inContext: Option[SeekGoal] =
      Some(SeekGoal.inContext(this)).filter(nxt => nxt != this)

  }

  object SeekGoal {
    def inContext(sk: SeekGoal): SeekGoal = sk.goal match {
      case PiDefn(variable: Term, value: Typ[u]) =>
        inContext(SeekGoal(value, sk.context.addVariable(variable)))
      case FuncTyp(dom: Typ[v], codom: Typ[u]) =>
        val x = dom.Var
        inContext(
          SeekGoal(codom, sk.context.addVariable(x), sk.forConsequences)
        )
      case _ => sk
    }
  }

  /**
    * an initial term state from which to evolve, perhaps just to generate types
    *
    * @param ts the term-state
    */
  case class InitState(
      ts: TermState,
      weight: Double
  )

  case class TangentBaseState(
      ts: TermState,
      cutoffScale: Double = 1.0,
      tgOpt: Option[TermGenParams] = None,
      depthOpt: Option[Int] = None,
      evOpt: Option[ExpressionEval] = None
  )

  case object TangentBaseCompleted

  case class SpecialInitState(
      ts: TermState,
      baseCutoff : Double = math.pow(10, -4),
      lemmaMix: Double = 0.5,
      cutoffScale: Double = 1.0, // lower the cutoff by this
      tgOpt: Option[TermGenParams] = None,
      depthOpt: Option[Int] = None
  )

  case class TautologyInitState(
      tautGen: TermState
  )

  /**
    * terms to use for proving, possibly the axioms etc not used in generating types.
    *
    * @param terms the term distribution to use
    */
  case class ConsiderTerms(terms: FiniteDistribution[Term])

  /**
    * introduce inductive types
    *
    * @param inducs distribution on inductive types introduced
    */
  case class ConsiderInductiveTypes(inducs: FiniteDistribution[ExstInducDefn])

  /**
    * modifying parameters, for instance excluding islands, strengthening backward reasoning etc
    * for serialization, inherit from this class
    *
    * @param modification the modifications
    */
  class ModifyParams(modification: TermGenParams => TermGenParams)

  /**
    * result of evolution, say of a local-prover; could be just from equations
    *
    * @param ts the evolved state
    */
  case class FinalState(ts: TermState) {
    lazy val successes = ts.successes.map {
      case (t, w, pfs) =>
        (ts.context.exportTypStrict(t), w, ts.context.exportStrict(pfs))
    }
  }

  /**
    * Result of generation and normalization
    *
    * @param eqn resulting equation nodes to use
    */
  case class GeneratedEquationNodes(eqn: Set[EquationNode])

  case object EquationsCompleted

  /**
    * instruction to seek lemmas
    *
    * @param weightPower power to flatten distributions
    * @param scale scale?
    */
  case class SeekLemmas(weightPower: Double, scale: Double)

  /**
    * lemmas that have been identified based on non-triviality (and possibly goals) from a final state.
    *
    * @param lemmas the lemmas with weights
    *
    */
  case class Lemmas(
      lemmas: Vector[(Typ[Term], Option[Term], Double)]
  )

  case class UsedLemmas(
      lemmas: Vector[(Typ[Term], Double)]
  ) {
    def weight(typ: Typ[Term]): Double =
      lemmas.find(_._1 == typ).map(_._2).getOrElse(0.0)

    val support = lemmas.map(_._1).toSet
  }

  case class TangentLemmas(
      lemmas: Vector[(Typ[Term], Option[Term], Double)]
  )

  case class BaseMixinLemmas(
      lemmas: Vector[(Typ[Term], Option[Term], Double)]
  )

  /**
    * instruction to use a lemma - could be as a tangent or mixing in to generators
    * if mixing in just call `ConsiderTerms`
    *
    * @param lemma lemma statement
    * @param proof proof term
    */
  case class UseLemma(lemma: Typ[Term], proofOpt: Option[Term]) {
    lazy val proof = proofOpt.getOrElse(s"lemma:$lemma" :: lemma)
  }

  /**
    * instruction to use a distribution of lemmas - could be as a tangent or mixing in to generators
    * if mixing in just call `ConsiderTerms`
    *
    * @param lemmas statement of lemmas
    * @param proofOptMap optional proofs
    */
  case class UseLemmaDistribution(
      lemmas: FiniteDistribution[Typ[Term]],
      proofOptMap: Option[Map[Typ[Term], Term]]
  ) {
    def proof(lemma: Typ[Term]) =
      proofOptMap
        .flatMap(proofMap => proofMap.get(lemma))
        .getOrElse(s"lemma:$lemma" :: lemma)

    lazy val proofs: FiniteDistribution[HoTT.Term] =
      lemmas.map(proof(_))
  }

  /**
    * proceed by tangent evolution, perhaps from a lemma
    *
    * @param term tangent direction
    * @param weight weight
    */
  case class TangentWithTerm(term: Term, weight: Double)

  /**
    * result of goal chomping
    *
    * @param successes results proved
    * @param failures results neither proved nor disproved
    * @param eqns equations contributed as a by-product
    */
  case class ChompResult(
      successes: Vector[StrategicProvers.Successes],
      failures: Vector[Typ[Term]],
      eqns: Set[EquationNode]
  )

  /**
    * instruction to generate subgoals from a given goal, using a function with eventual codomain, induction etc
    *
    */
  trait ReasonBackward {
    val goal: Typ[Term]
  }

  case class SeekInduction(goal: Typ[Term]) extends ReasonBackward

  case class SeekEventualCodomain(goal: Typ[Term]) extends ReasonBackward

  /**
    * backward reasoning for special types - products, co-products, Sigma-types and Pi-types;
    * respond with seeking instantiations and adding variables for the last two cases.
    *
    * @param goal type to resolve
    */
  case class ResolveGoal(goal: Typ[Term]) extends ReasonBackward

  sealed trait Decided {
    val statement: Typ[Term]

    val context: Context
  }

  object Decided {
    implicit def decideMap: PostMaps[Decided] =
      PostMaps.empty[Decided] || ((p: Proved) => p) || ((c: Contradicted) => c)

    def asEither(d: Decided): Either[Contradicted, Proved] = d match {
      case p @ Proved(statement, proofOpt, _)        => Right(p)
      case c @ Contradicted(statement, contraOpt, _) => Left(c)
    }
  }

  case class Proved(
      statement: Typ[Term],
      proofOpt: Option[Term],
      context: Context
  ) extends Decided {
    proofOpt.foreach(proof => assert(proof.typ == statement))
  }

  trait PropagateProof {
    def propagate(proofs: Set[Term]): Set[Decided]

    val context: Context

    def inContext(proofs: Set[Term]): Set[Term] =
      proofs.flatMap(context.importOpt(_))
  }

  object PropagateProof {
    implicit def propsMap: PostMaps[PropagateProof] =
      PostMaps.empty[PropagateProof] || ((x: Consequence) => x) || (
          (x: Contradicts) => x
      ) || ((x: FromAll) => x) || ((x: FromAny) => x)
  }

  case class Consequence(
      premise: Typ[Term],
      conclusion: Typ[Term],
      proofMapOpt: Option[ExstFunc],
      context: Context
  ) extends PropagateProof {
    def propagate(proofs: Set[Term]): Set[Decided] =
      proofs.filter(_.typ == premise).map { proof =>
        Proved(conclusion, proofMapOpt.flatMap(m => m(proof)), context)
      }

    proofMapOpt.foreach { pfMap =>
      assert(
        pfMap("hyp" :: premise).map(_.typ) == Some(conclusion),
        s"Cannot conclude: ${pfMap.func.typ} on ${premise} gives $conclusion"
      )
    }
  }

  case class Contradicts(
      premise: Typ[Term],
      conclusion: Typ[Term],
      contraMapOpt: Option[ExstFunc],
      context: Context
  ) extends PropagateProof {
    def propagate(proofs: Set[HoTT.Term]): Set[Decided] =
      proofs.filter(_.typ == premise).map { proof =>
        val contraOpt =
          contraMapOpt
            .flatMap { contraMap =>
              contraMap(proof)
            }
            .flatMap(ExstFunc.opt(_))
        Contradicted(conclusion, contraOpt, context)
      }

    contraMapOpt.foreach(
      contraMap =>
        assert(
          fold(contraMap("assume" :: premise).get)("also" :: conclusion).typ == Zero
        )
    )
  }

  case class Contradicted(
      statement: Typ[Term],
      contraOpt: Option[ExstFunc],
      context: Context
  ) extends Decided {
    for {
      contra <- contraOpt
    } assert(contra("assume" :: statement).map(_.typ) == Some(Zero))
  }

  case class RepresentationMap(
      rep: Map[GeneratorVariables.Variable[_], Vector[Double]]
  )

  case class TimeLimit(duration: scala.concurrent.duration.FiniteDuration)

  case class HaltIf(condition: Unit => Boolean)

  case class Weight(scale: Double)

  type WithWeight[A] = Weight :: A :: HNil

  def withWeightFD[A](fd: FiniteDistribution[A]): Vector[WithWeight[A]] =
    fd.pmf.map { case Weighted(x, p) => withWeight(x, p) }

  /**
    * convenient posting with weight preceding posts, say for lemmas with weight
    *
    * @param value the stuff to post
    * @param weight the weight
    * @return return ID after posting stuff
    */
  def withWeight[A](value: A, weight: Double): WithWeight[A] =
    Weight(weight) :: value :: HNil

  case class OptimizeGenerators(decay: Double)

  case class NarrowOptimizeGenerators(
      hW: Double,
      klW: Double,
      smoothing: Double,
      decay: Double
  )

  case class OptimalInitial(
      lp: LocalProver,
      hW: Double,
      klW: Double,
      smoothing: Double,
      decay: Double
  )

  case object GenerateTypes

  case object GenerateTerms

  trait SeekInstances {
    type U <: Term with Subs[U]
    type V <: Term with Subs[V]

    val typ: Typ[U]
    val goal: TypFamily[U, V]

    def goalCast(t: Term) = goal(t.asInstanceOf[U])
    val context: Context
    val forConsequences: Set[Typ[Term]]

    def sigma = SigmaTyp[U, V](goal)
  }

  object SeekInstances {
    def apply[X <: Term with Subs[X], Y <: Term with Subs[Y]](
        tp: Typ[X],
        gl: TypFamily[X, Y],
        ctx: Context,
        fc: Set[Typ[Term]]
    ): SeekInstances =
      new SeekInstances {
        type U = X

        type V = Y

        val typ: HoTT.Typ[U] = tp

        val goal: HoTT.TypFamily[U, V] = gl

        val context: Context = ctx

        val forConsequences: Set[HoTT.Typ[HoTT.Term]] = fc

      }
  }

  case class Instance(term: Term, typ: Typ[Term], context: Context) {
    assert(term.typ == typ)
  }

  def product[A](vv: Vector[Set[A]]): Set[Vector[A]] = vv match {
    case Vector() => Set(Vector())
    case x +: ys =>
      for {
        a <- x
        w <- product(ys)
      } yield a +: w
  }
  case class FromAll(
      typs: Vector[Typ[Term]],
      conclusion: Typ[Term],
      proofOpt: Option[Term],
      context: Context,
      forConsequences: Set[Typ[Term]]
  ) extends PropagateProof {
    def propagate(proofs: Set[HoTT.Term]): Set[Decided] =
      if (typs.toSet.subsetOf(proofs.map(_.typ))) {
        val termsVec   = typs.map(typ => proofs.filter(_.typ == typ))
        val prodProofs = product(termsVec)
        for {
          terms <- prodProofs
        } yield
          Proved(
            conclusion,
            proofOpt.map(pf => foldterms(pf, terms.toList)),
            context
          )
      } else Set()
  }

  object FromAll {
    def backward(
        fn: Term,
        cod: Typ[Term]
    ): Option[(Vector[Typ[Term]], Option[Term])] =
      if (fn.typ == cod) Some((Vector(), Some(fn)))
      else
        fn match {
          case func: FuncLike[u, v] =>
            val head = func.dom
            val x    = head.Var
            backward(func(x), cod).map {
              case (tail, pfO) =>
                (
                  head +: tail,
                  pfO.map(pf => x :~> pf)
                )
            }
          case _ => None
        }

    def get(
        fn: Term,
        cod: Typ[Term],
        forConsequences: Set[Typ[Term]] = Set(),
        context: Context
    ): Option[FromAll] =
      backward(fn, cod).map {
        case (typs, proofOpt) =>
          FromAll(typs, cod, proofOpt, context, forConsequences)
      }
  }

  case class FunctionForGoal(
      fn: Term,
      goal: Typ[Term],
      context: Context,
      forConsequences: Set[Typ[Term]] = Set()
  )

  /**
    * Other types that are likely to have common ingredients in proofs
    *
    * @param typ the original type
    * @param related types related
    */
  case class RelatedStatements(
      typ: Typ[Term],
      related: FiniteDistribution[Typ[Term]]
  )

  /**
    * Given a type, a lemma is the type of an ingredient of the generating set, ideally not easily inhabited.
    * These are candidate lemmas
    *
    * @param typ the goal
    * @param lemmas candidate lemmas
    */
  case class IntermediateStatements(
      typ: Typ[Term],
      lemmas: FiniteDistribution[Typ[Term]]
  )

  case class FromAny(
      typs: Vector[Typ[Term]],
      conclusion: Typ[Term],
      exhaustive: Boolean,
      proofsOpt: Vector[Option[ExstFunc]],
      context: Context,
      forConsequences: Set[Typ[Term]]
  ) extends PropagateProof {
    def propagate(proofs: Set[HoTT.Term]): Set[Decided] =
      if (typs.toSet.intersect(proofs.map(_.typ)).nonEmpty) {
        val proofSet = proofs
          .filter(t => typs.contains(t.typ))
          .flatMap(
            x =>
              typs.zip(proofsOpt).find(_._1 == x.typ).flatMap {
                case (_, pfMap) => pfMap.flatMap(_(x))
              }
          )
        proofSet.map(pf => Proved(conclusion, Some(pf), context))
      } else if (exhaustive && typs.toSet.subsetOf(proofs.map(_.typ)))
        Set(Contradicted(conclusion, None, context))
      else Set()
  }

  @annotation.tailrec
  def propagateProofs(
      props: Set[PropagateProof],
      decisions: Set[Decided]
  ): Set[Decided] = {
    val proofs = decisions.collect {
      case Proved(statement, proofOpt, context) =>
        proofOpt
          .map(context.export(_))
          .getOrElse("proved" :: context.exportTyp(statement))
    }

    val offspring: Set[Decided] =
      for {
        p  <- props
        pf <- p.propagate(p.inContext(proofs))
      } yield pf

    // if (offspring.nonEmpty) propagateProofs(props, decisions union offspring)
    // else decisions

    // pprint.log(decisions.size)
    // pprint.log((offspring -- decisions).size)

    if (offspring.subsetOf(decisions)) decisions
    else propagateProofs(props, decisions union offspring)
  }

  def derivedProofs(
      props: Set[PropagateProof],
      decisions: Set[Decided]
  ): Set[Decided] = propagateProofs(props, decisions) -- decisions

  case class AddVariable(typ: Typ[Term])

  case class SplitFocus(number: Int, scale: Double)
}
