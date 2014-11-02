package ch.usi.inf.l3.elang.namer

import ch.usi.inf.l3._
import elang.typecheck._
import elang.namer._
import fj.namer.Names._

trait Symbol {
  var tpe: Type
  var name: String
  var owner: Symbol
  def enclosingClass: Option[ClassSymbol]
}

trait NoSymbol extends Symbol {
  var tpe: Type = NoType
  var name: String = NONAME
  var owner: Symbol = NoSymbol
  def enclosingClass: Option[ClassSymbol] = None
  override def toString: String = "no symbol"
  override def hashCode: Int = toString.hashCode
}

object NoSymbol extends NoSymbol

trait ClassSymbol extends Symbol {
  var parent: Symbol
  def enclosingClass: Option[ClassSymbol] = None
  override def toString: String = s"class ${name}"
  override def hashCode: Int = toString.hashCode
  override def equals(other: Any) = {
    other != null && (other match {
      case a: ClassSymbol =>
        a.name == name && a.tpe == tpe && a.parent == parent
      case _ => false
    })
  }
}

object ClassSymbol {
  def apply(): ClassSymbol = apply(null, null, null, null)
  def unapply(cs: ClassSymbol): Option[(String, Type, Symbol, Symbol)] = {
    Some((cs.name, cs.tpe, cs.owner, cs.parent))
  }
  def apply(n: String, t: Type,
            o: Symbol, p: Symbol): ClassSymbol = {
    new ClassSymbol {
      var name = n
      var tpe = t
      var owner = o
      var parent = p
    } 
  }
} 


trait TermSymbol extends Symbol {
  def enclosingClass: Option[ClassSymbol] = owner match {
    case x: ClassSymbol => Some(x)
    case _ => owner.owner.enclosingClass
  }
  override def toString: String = tpe match {
    case v: VarType => s"variable ${name}"
    case _ => s"method ${name}"
  }
  override def hashCode = tpe.hashCode
  override def equals(other: Any) = {
    other != null && (other match {
      case a: TermSymbol =>
        a.name == name && a.tpe == tpe && a.owner == owner
      case _ => false
    })
  }
}

object TermSymbol {
  def apply(): TermSymbol = apply(null, null, null)
  def unapply(cs: TermSymbol): Option[(String, Type, Symbol)] = {
    Some((cs.name, cs.tpe, cs.owner))
  }
  def apply(n: String, t: Type,
            o: Symbol): TermSymbol = {
    new TermSymbol {
      var name = n
      var tpe = t
      var owner = o
    } 
  }
} 


trait UseSymbol extends Symbol {
  // This is mean, but we really don't care about setting the tpe
  var uses: Symbol
  def tpe_=(tpe: Type): Unit = ()
  def tpe: Type = uses match {
    case null => NoType
    case _ => uses.tpe
  }
  // This is mean, but we really don't care about setting the name
  def name_=(n: String): Unit = ()
  def name: String = uses match {
    case null => NONAME
    case _ => uses.name
  }
  def owner_=(sym: Symbol): Unit = ???
  def owner: Symbol = uses.owner
  def enclosingClass: Option[ClassSymbol] = None
  override def toString = uses.toString
  override def hashCode = uses.hashCode
  override def equals(other: Any) = {
    other != null && (other match {
      case u: UseSymbol => uses == u.uses
      case _ => false
    })
  }
}

object UseSymbol {
  def apply(): UseSymbol = apply(NoSymbol)
  def unapply(cs: UseSymbol): Option[Symbol] = {
    Some(cs.uses)
  }
  def apply(u: Symbol): UseSymbol = {
    new UseSymbol {
      var uses = u
    } 
  }
}

case object ObjectSymbol extends Symbol {
  var tpe: Type = ObjectType
  var name: String = "Object"
  var owner: Symbol = NoSymbol
  def enclosingClass: Option[ClassSymbol] = None
}

trait Namer {
  def nameIt(owner: Symbol): Unit
}



