package ch.usi.inf.l3.fjexpr.namer


import ch.usi.inf.l3._
import fj.ast._
import elang.namer._
import fj.namer._
import elang.typecheck._
import elang.ast._
import fjexpr.ast._
import fjexpr.typecheck._



trait LitSymbol extends Symbol {
  def enclosingClass: Option[ClassSymbol] = owner match {
    case x: ClassSymbol => Some(x)
    case _ => owner.owner.enclosingClass
  }
  override def toString: String = "literal symbol"
  override def hashCode: Int = tpe.hashCode
  override def equals(other: Any) = other != null && (
    other match {
      case l: LitSymbol => l.tpe == tpe
      case _ => false
    }
  )
}

object LitSymbol {
  def apply(): LitSymbol = apply(null, null)
  def apply(t: PrimitiveType, o: Symbol): LitSymbol = {
    new LitSymbol {
      var name = "Lit Symbol"
      var owner = o
      var tpe: Type = t
    }
  }
  def unapply(l: LitSymbol): Option[Type] = {
    Some(l.tpe)
  }
}
trait FJExprNamer extends FJExprAlg[Namer with Tree] 
                  with FJNamer {


  def BinOp(l: Namer with Tree, o: Bop, r: Namer with Tree,
                    p: Position, s: LitSymbol): Namer with Tree = {
    new Namer with Tree {
      type S = Symbol
      def nameIt(owner: Symbol): Unit = ()
      val symbol: Symbol = s
      val pos: Position = p
    }
  }


  def UniOp(op: Uop, expr: Namer with Tree, p: Position, 
          s: LitSymbol): Namer with Tree = {
    new Namer with Tree {
      type S = Symbol
      def nameIt(owner: Symbol): Unit = ()
      val symbol: Symbol = s
      val pos: Position = p
    }
  }


  def Literal(v: Int, p: Position, s: LitSymbol): Namer with Tree = {
    new Namer with Tree {
      type S = Symbol
      def nameIt(owner: Symbol): Unit = ()
      val symbol: Symbol = s
      val pos: Position = p
    }
  }

  def Literal(v: Double, p: Position, s: LitSymbol): Namer with Tree = {
    new Namer with Tree {
      type S = Symbol
      def nameIt(owner: Symbol): Unit = ()
      val symbol: Symbol = s
      val pos: Position = p
    }
  }

  def Literal(v: Boolean, p: Position, s: LitSymbol): Namer with Tree = {
    new Namer with Tree {
      type S = Symbol
      def nameIt(owner: Symbol): Unit = ()
      val symbol: Symbol = s
      val pos: Position = p
    }
  }

  def Literal(v: String, p: Position, s: LitSymbol): Namer with Tree = {
    new Namer with Tree {
      type S = Symbol
      def nameIt(owner: Symbol): Unit = ()
      val symbol: Symbol = s
      val pos: Position = p
    }
  }

  def NullLiteral(p: Position, s: LitSymbol): Namer with Tree = {
    new Namer with Tree {
      type S = Symbol
      def nameIt(owner: Symbol): Unit = ()
      val symbol: Symbol = s
      val pos: Position = p
    }
  }
}

object FJExprNamer extends FJExprNamer {
  val context: TypeContext = new TypeContext
}
