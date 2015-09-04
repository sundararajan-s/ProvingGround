package provingground

import HoTT._

import ScalaPolyRep._

import ScalaRep._

/**
 * @author gadgil
 */
case class VecTyp[+U<: Term with Subs[U], X](basetyp: Typ[U], dim: Int)(implicit _baserep: ScalaRep[U, X]) extends 
  Typ[RepTerm[Vector[X]]]{
     val typ = Universe(0)

    def symbObj(name: AnySym): RepTerm[Vector[X]] = RepSymbObj[Vector[X], RepTerm[Vector[X]]](name, this)

    def newobj = this

    def subs(x: Term, y: Term) = (x, y) match {
      case (xt: Typ[_], yt: Typ[_]) if (xt == this) => yt.asInstanceOf[Typ[RepTerm[Vector[X]]]]
      case _ => this
    }
  
    val baserep = _baserep
}

object VecTyp{
  case class VecPolyRep[U <: Term with Subs[U], X]() extends ScalaPolyRep[Term, Vector[X]]{
    
    def apply(typ: Typ[Term])(elem: Vector[X]) = typ match{
      case tp @ VecTyp(basetyp, dim) if dim == elem.size => {
        val pattern = new ScalaSym[Term, Vector[X]](tp)
        Some(pattern(elem))        
      }
      case _ => None
    }
    
    def unapply(term: Term) = term.typ match {
      case tp : VecTyp[_, X] => {
        val pattern = new ScalaSym[Term, Vector[X]](tp)
        pattern.unapply(term)
      }
      case _ => None
    }
    
    def subs(x: Term, y: Term) = this
  }
  
  implicit def vecRep[U <: Term with Subs[U], X](implicit baserep: ScalaPolyRep[U, X]) : ScalaPolyRep[Term, Vector[X]] = VecPolyRep[U, X]
   
  import Nat._

  val n = "n" :: Nat
  
  implicit val NatVecRep = vecRep[Term, Long]

  val Vec = (((n: Long) => (VecTyp[Term, Long](Nat, n.toInt) : Typ[Term])).hott(Nat ->: __)).get 
  
  private val ltyp = n ~>: (Vec(n) ->: Nat)
  
  private val vsize = (n : Long) => 
    (v : Vector[Long]) => {
      assert(v.size ==n ,"domain mismatch in Pi-type")
      n    
  }
    
  val nsize = vsize.hott(ltyp)

  private val nvsucc = 
    (n : Long) => 
      (a : Long) =>
        (v : Vector[Long]) => {
          assert(v.size ==n ,"domain mismatch in Pi-type")
          a +: v  
          }
        
  private val nsucctyp  = n  ~>: Nat ->: (Vec(n) ->: Vec(Nat.succ(n)))
  
  val nsucc = nvsucc.hott(nsucctyp)
}