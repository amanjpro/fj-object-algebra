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

case object NoSymbol extends Symbol {
  var tpe: Type = NoType
  var name: String = NONAME
  var owner: Symbol = NoSymbol
  def enclosingClass: Option[ClassSymbol] = None
}

case class ClassSymbol(var name: String, var tpe: Type,
        var owner: Symbol, var parent: Symbol) extends Symbol {
  def this() = this(null, null, null, null)
  def enclosingClass: Option[ClassSymbol] = None
}

case class TermSymbol(var name: String, var tpe: Type,
        var owner: Symbol) extends Symbol {
  def this() = this(null, null, null) 
  def enclosingClass: Option[ClassSymbol] = owner match {
    case x: ClassSymbol => Some(x)
    case _ => owner.owner.enclosingClass
  }
  override def toString: String = s"method ${name}"
  override def hashCode = name.hashCode
}

case class UseSymbol(var uses: Symbol) extends Symbol {
  def this() = this(null)
  // This is mean, but we really don't care about setting the tpe
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



