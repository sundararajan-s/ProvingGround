package provingground.learning
import provingground._
import HoTT._
import provingground.{FiniteDistribution => FD, _}
import shapeless._
import induction._

import scala.language.higherKinds
import TermRandomVars._
import monix.eval.Task
import interface._

import scala.concurrent._
import duration._
import upickle.default.{read, write, ReadWriter => RW, readwriter}
import TermGenParams._
import ujson.Value

import scala.collection.mutable

object TermGenParams {
  def fromJson(js: ujson.Value): TermGenParams = {
    val m: mutable.Map[String, Value] = js.obj
    TermGenParams(
      appW = m("application").num,
      unAppW = m("unified-application").num,
      argAppW = m("application-by-argument").num,
      lmW = m("lambda").num,
      piW = m("pi-type").num,
      termsByTypW = m("terms-by-type").num,
      typFromFamilyW = m("type-from-family").num,
      sigmaW = m("sigma-type").num,
      recDefW = m("recursive-definition").num,
      inducDefW = m("inductive-definition").num,
      typAsCodW = m("type-as-codomain").num,
      targetInducW = m("targeted-induction").num,
      varWeight = m("variable-weight").num,
      goalWeight = m("goal-weight").num,
      typVsFamily = m("types-versus-families").num,
      negTargetW = m("negation-target").num,
      solverW = m("solver-weight").num
    )
  }

  val zero = TermGenParams(
    appW = 0,
    unAppW = 0,
    argAppW = 0,
    lmW = 0,
    piW = 0,
    termsByTypW = 0,
    typFromFamilyW = 0,
    sigmaW = 0,
    recDefW = 0,
    inducDefW = 0,
    typAsCodW = 0,
    targetInducW = 0,
    varWeight = 0.3,
    goalWeight = 0.7,
    typVsFamily = 0.5,
    negTargetW = 0,
    solverW = 0
  )

  implicit def rw: RW[TermGenParams] =
    readwriter[ujson.Value].bimap(_.toJson, fromJson)

  case class AddVar(typ: Typ[Term], wt: Double)
      extends (TermState => (TermState, Term)) {
    def apply(ts: TermState): (TermState, Term) = ts.addVar(typ, wt)

    override def toString = "AddVar"
  }

  case object GetVar extends (Typ[Term] => Term) {
    def apply(typ: Typ[Term]): Term = typ.Var

    override def toString = "GetVar"
  }

  case object InIsle extends ((Term, TermState) => TermState) {
    def apply(t: Term, state: TermState): TermState = state.inIsle(t)

    override def toString = "InIsle"
  }
}

case class TermGenParamsNodes(tg: TermGenParams)
    extends TermGeneratorNodes[TermState](
      { case (fn, arg) => applyFunc(fn.func, arg) },
      { case (fn, arg) => Unify.appln(fn.func, arg) },
      AddVar(_, tg.varWeight),
      GetVar,
      InIsle
    )

case class TermGenParams(
    appW: Double = 0.1,
    unAppW: Double = 0.1,
    argAppW: Double = 0.1,
    lmW: Double = 0.1,
    piW: Double = 0.1,
    termsByTypW: Double = 0.05,
    typFromFamilyW: Double = 0.05,
    sigmaW: Double = 0.05,
    recDefW: Double = 0,
    inducDefW: Double = 0,
    typAsCodW: Double = 0,
    targetInducW: Double = 0,
    varWeight: Double = 0.3,
    goalWeight: Double = 0.7,
    typVsFamily: Double = 0.5,
    negTargetW: Double = 0,
    solverW: Double = 0
) { tg =>

  val Gen = TermGenParamsNodes(this)

  val toJson: ujson.Value =
    ujson.Obj(
      "application"             -> appW,
      "unified-application"     -> unAppW,
      "application-by-argument" -> argAppW,
      "lambda"                  -> lmW,
      "pi-type"                 -> piW,
      "terms-by-type"           -> termsByTypW,
      "type-from-family"        -> typFromFamilyW,
      "sigma-type"              -> sigmaW,
      "recursive-definition"    -> recDefW,
      "inductive-definition"    -> inducDefW,
      "type-as-codomain"        -> typAsCodW,
      "targeted-induction"      -> targetInducW,
      "variable-weight"         -> varWeight,
      "goal-weight"             -> goalWeight,
      "types-versus-families"   -> typVsFamily,
      "negation-target"         -> negTargetW,
      "solver-weight"           -> solverW
    )

  import Gen._, GeneratorNode._,
  TermRandomVars.{withTypNode => wtN, funcWithDomTermNode => fdtN}

  val termInit
      : Double = 1.0 - appW - unAppW - argAppW - lmW - termsByTypW - recDefW - inducDefW

  val typInit
      : Double = 1.0 - appW - unAppW - piW - sigmaW - typFromFamilyW - recDefW - inducDefW

  val termNodes: NodeCoeffs.Cons[TermState, Term, Double, HNil, Term] =
    (Init(Terms)           -> termInit) ::
      (applnNode           -> appW) ::
      (unifApplnNode       -> unAppW) ::
      (applnByArgNode      -> argAppW) ::
      (lambdaNode          -> lmW) ::
      (termsByTyps         -> termsByTypW) ::
      (recFuncFoldedNode   -> recDefW) ::
      (inducFuncFoldedNode -> inducDefW) ::
      Terms.target[TermState, Term, Double, Term]

  val typNodes: NodeCoeffs.Cons[TermState, Term, Double, HNil, Typ[Term]] =
    (Init(Typs)                                -> typInit) ::
      (typApplnNode                            -> appW) ::
      (typUnifApplnNode                        -> unAppW) ::
      (piNode                                  -> piW) ::
      (sigmaNode                               -> sigmaW) ::
      (typFoldNode                             -> typFromFamilyW) ::
      ((recFuncFoldedNode | (typSort, Typs))   -> recDefW) ::
      ((inducFuncFoldedNode | (typSort, Typs)) -> inducDefW) ::
      Typs.target[TermState, Term, Double, Typ[Term]]

  val inducNodes
      : NodeCoeffs.Cons[TermState, Term, Double, HNil, ExstInducDefn] =
    (Init(InducDefns) -> 1.0) ::
      InducDefns.target[TermState, Term, Double, ExstInducDefn]

  val inducDomainNodes
      : NodeCoeffs.Cons[TermState, Term, Double, ExstInducDefn :: HNil, Term] =
    (domainForDefnNodeFamily -> 1.0) ::
      DomForInduc.target[TermState, Term, Double, Term]

  val goalNodes
      : NodeCoeffs.Cons[TermState, Term, Double, HNil, Typ[Term]] = (Init(
    Goals
  ) -> 1.0) :: Goals
    .target[TermState, Term, Double, Typ[Term]]

  val isleDomainsNode: NodeCoeffs.Cons[TermState, Term, Double, HNil, Typ[
    Term
  ]] = (GeneratorNode
    .Map(identity[Typ[Term]], Typs, IsleDomains) -> 1.0) :: IsleDomains
    .target[TermState, Term, Double, Typ[Term]]

  val funcForCodNodes
      : NodeCoeffs.Cons[TermState, Term, Double, Typ[Term] :: HNil, Term] =
    (codomainNodeFamily -> 1.0) ::
      FuncForCod.target[TermState, Term, Double, Term]

  val funcNodes: NodeCoeffs.Cons[TermState, Term, Double, HNil, ExstFunc] =
    (Init(Funcs)                                 -> termInit) ::
      ((applnNode | (funcSort, Funcs))           -> appW) ::
      ((unifApplnNode | (funcSort, Funcs))       -> unAppW) ::
      ((applnByArgNode | (funcSort, Funcs))      -> argAppW) ::
      ((lambdaNode | (funcSort, Funcs))          -> lmW) ::
      ((termsByTyps | (funcSort, Funcs))         -> termsByTypW) ::
      ((recFuncFoldedNode | (funcSort, Funcs))   -> recDefW) ::
      ((inducFuncFoldedNode | (funcSort, Funcs)) -> inducDefW) ::
      Funcs.target[TermState, Term, Double, ExstFunc]

  val typFamilyNodes: NodeCoeffs.Cons[TermState, Term, Double, HNil, ExstFunc] =
    (Init(TypFamilies)                                      -> termInit) ::
      (typFamilyApplnNode                                   -> appW) ::
      (typFamilyUnifApplnNode                               -> unAppW) ::
      ((applnByArgNode | (typFamilySort, TypFamilies))      -> argAppW) ::
      (lambdaTypFamilyNode                                  -> lmW) ::
      ((termsByTyps | (typFamilySort, TypFamilies))         -> termsByTypW) ::
      ((recFuncFoldedNode | (typFamilySort, TypFamilies))   -> recDefW) ::
      ((inducFuncFoldedNode | (typFamilySort, TypFamilies)) -> inducDefW) ::
      TypFamilies.target[TermState, Term, Double, ExstFunc]

  val termsByTypNodes
      : NodeCoeffs.Cons[TermState, Term, Double, Typ[Term] :: HNil, Term] =
    (TermsWithTyp.init       -> (termInit * (1 - goalWeight - typAsCodW - targetInducW - solverW))) ::
      (wtN(applnNode)        -> appW) ::
      (wtN(unifApplnNode)    -> unAppW) ::
      (wtN(applnByArgNode)   -> argAppW) ::
      (lambdaByTypNodeFamily -> (termInit * goalWeight + lmW)) ::
      (typAsCodNodeFamily    -> typAsCodW) ::
      (targetInducNodeFamily -> targetInducW) ::
      (solveFamily           -> solverW) ::
      TermsWithTyp.target[TermState, Term, Double, Term]

  val typOrFmlyNodes: NodeCoeffs.Cons[TermState, Term, Double, HNil, Term] =
    (TypsAndFamilies.fromTyp        -> typVsFamily) ::
      (TypsAndFamilies.fromFamilies -> (1.0 - typVsFamily)) ::
      TypsAndFamilies.target[TermState, Term, Double, Term]

  val targTypNodes: NodeCoeffs.Cons[TermState, Term, Double, HNil, Term] =
    (TargetTyps.fromGoal     -> goalWeight) ::
      (TargetTyps.fromTyp    -> (1.0 - goalWeight - negTargetW)) ::
      (TargetTyps.fromNegTyp -> negTargetW) ::
      TargetTyps.target[TermState, Term, Double, Term]

  val funcWithDomNodes
      : NodeCoeffs.Cons[TermState, Term, Double, Typ[Term] :: HNil, ExstFunc] =
    (FuncsWithDomain.init         -> termInit) ::
      (fdtN(applnNode)            -> appW) ::
      (fdtN(unifApplnNode)        -> unAppW) ::
      (fdtN(applnByArgNode)       -> argAppW) ::
      (lambdaForFuncWithDomFamily -> lmW) ::
      FuncsWithDomain.target[TermState, Term, Double, ExstFunc]

  val nodeCoeffSeq: NodeCoeffSeq[TermState, Term, Double] =
    funcWithDomNodes +: targTypNodes +: goalNodes +: isleDomainsNode +: inducDomainNodes +: inducNodes +: funcForCodNodes +:
      termNodes +: typNodes +: funcNodes +: typFamilyNodes +: typOrFmlyNodes +: funcWithDomNodes +: termsByTypNodes +:
      NodeCoeffSeq.Empty[TermState, Term, Double]()

  lazy val monixFD: MonixFiniteDistribution[TermState, Term] =
    MonixFiniteDistribution(nodeCoeffSeq)

  def monixTangFD(baseState: TermState) =
    MonixTangentFiniteDistribution(nodeCoeffSeq, baseState)

  def nextStateTask(
      initState: TermState,
      epsilon: Double,
      limit: FiniteDuration = 3.minutes
  ): Task[TermState] =
    for {
      terms <- monixFD.varDist(initState)(Terms, epsilon, limit)
      typs  <- monixFD.varDist(initState)(Typs, epsilon, limit)
    } yield TermState(terms, typs, initState.vars, initState.inds)

  def evolvedStateTask(
      initState: TermState,
      epsilon: Double,
      limit: FiniteDuration = 3.minutes
  ): Task[EvolvedState] =
    nextStateTask(initState, epsilon, limit).map(
      result => EvolvedState(initState, result, tg, epsilon)
    )

  def nextTangStateTask(
      baseState: TermState,
      tangState: TermState,
      epsilon: Double,
      limit: FiniteDuration = 3.minutes
  ): Task[TermState] =
    for {
      terms <- monixTangFD(baseState).varDist(tangState)(Terms, epsilon, limit)
      typs  <- monixTangFD(baseState).varDist(tangState)(Typs, epsilon, limit)
    } yield TermState(terms, typs, baseState.vars, baseState.inds)

  def findProof(
      initState: TermState,
      typ: Typ[Term],
      epsilon: Double,
      limit: FiniteDuration = 3.minutes
  ): Task[FD[Term]] =
    monixFD
      .varDist(initState)(TermsWithTyp.at(typ :: HNil), epsilon, limit)
      .map(_.flatten)
}

trait EvolvedStateLike {
  val init: TermState
  val result: TermState
  val params: TermGenParams

  val goalsAttained: Set[Typ[Term]] =
    init.goals.support.intersect(result.terms.support.map(_.typ))

  val foundGoal: Boolean = goalsAttained.nonEmpty
}

case class EvolvedState(
    init: TermState,
    result: TermState,
    params: TermGenParams,
    epsilon: Double
) extends EvolvedStateLike

object TermGenJson {

  def nextStateTask(inp: String): Task[String] = {
    val obj           = ujson.read(inp).obj
    val termGenParams = read[TermGenParams](obj("generator-parameters").str)
    val epsilon       = obj("epsilon").num
    val initState     = TermState.fromJson(obj("initial-state"))
    val task          = termGenParams.nextStateTask(initState, epsilon)
    task.map { (ts) =>
      write(ts.json)
    }
  }

  def nextTangStateTask(inp: String): Task[String] = {
    val obj           = read[ujson.Value](inp).obj
    val termGenParams = read[TermGenParams](obj("generator-parameters").str)
    val epsilon       = obj("epsilon").num
    val baseState     = TermState.fromJson(obj("initial-state"))
    val tangState     = TermState.fromJson(obj("tangent-state"))
    val task          = termGenParams.nextTangStateTask(baseState, tangState, epsilon)
    task.map((ts) => write(ts.json))
  }

  val all =
    MultiTask(
      "step"         -> nextStateTask,
      "tangent-step" -> nextTangStateTask
    )

}