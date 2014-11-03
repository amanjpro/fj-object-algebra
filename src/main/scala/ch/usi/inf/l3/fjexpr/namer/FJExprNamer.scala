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
  def enclosingClass: Option[ClassSymbol] = None 
  def owner = NoSymbol
  def owner_=(o: Symbol) = ???
  override def toString: String = "literal symbol"
  override def hashCode: Int = tpe.hashCode
  override def equals(other: Any) = other != null && (
    other match {
      case l: LitSymbol => l.tpe == tpe
      case _ => false
    }
  )
}

case object FloatSymbol extends LitSymbol {
  def tpe: Type = FloatType
  def tpe_=(t: Type): Unit = ???
  def name = "Float Symbol"
  def name_=(n: String) = ???
}

case object IntSymbol extends LitSymbol {
  def tpe: Type = IntType
  def tpe_=(t: Type): Unit = ???
  def name = "Int Symbol"
  def name_=(n: String) = ???
}

case object BoolSymbol extends LitSymbol {
  def tpe: Type = BoolType
  def tpe_=(t: Type): Unit = ???
  def name = "Bool Symbol"
  def name_=(n: String) = ???
}

case object StrSymbol extends LitSymbol {
  def tpe: Type = StrType
  def tpe_=(t: Type): Unit = ???
  def name = "String Symbol"
  def name_=(n: String) = ???
}

case object NullSymbol extends LitSymbol {
  def tpe: Type = NullType
  def tpe_=(t: Type): Unit = ???
  def name = "Null Symbol"
  def name_=(n: String) = ???
}

trait FJExprNamer extends FJExprAlg[Namer with Tree] 
                  with FJNamer {


  def BinOp(l: Namer with Tree, o: Bop, r: Namer with Tree,
                    p: Position, s: UseSymbol): Namer with Tree = {
    new Namer with Tree {
      type S = Symbol
      def nameIt(owner: Symbol): Unit = ()
      val symbol: Symbol = s
      val pos: Position = p
    }
  }


  def UniOp(op: Uop, expr: Namer with Tree, p: Position, 
          s: UseSymbol): Namer with Tree = {
    new Namer with Tree {
      type S = Symbol
      def nameIt(owner: Symbol): Unit = ()
      val symbol: Symbol = s
      val pos: Position = p
    }
  }


  def Literal(v: Int, p: Position): Namer with Tree = {
    new Namer with Tree {
      type S = Symbol
      def nameIt(owner: Symbol): Unit = ()
      val symbol: Symbol = IntSymbol
      val pos: Position = p
    }
  }

  def Literal(v: Double, p: Position): Namer with Tree = {
    new Namer with Tree {
      type S = Symbol
      def nameIt(owner: Symbol): Unit = ()
      val symbol: Symbol = FloatSymbol
      val pos: Position = p
    }
  }

  def Literal(v: Boolean, p: Position): Namer with Tree = {
    new Namer with Tree {
      type S = Symbol
      def nameIt(owner: Symbol): Unit = ()
      val symbol: Symbol = BoolSymbol
      val pos: Position = p
    }
  }

  def Literal(v: String, p: Position): Namer with Tree = {
    new Namer with Tree {
      type S = Symbol
      def nameIt(owner: Symbol): Unit = ()
      val symbol: Symbol = StrSymbol
      val pos: Position = p
    }
  }

  def NullLiteral(p: Position): Namer with Tree = {
    new Namer with Tree {
      type S = Symbol
      def nameIt(owner: Symbol): Unit = ()
      val symbol: Symbol = NullSymbol
      val pos: Position = p
    }
  }
}

object FJExprNamer extends FJExprNamer {
  private val primitives = List(("int", IntSymbol),
                                ("boolean", BoolSymbol),
                                ("double", FloatSymbol),
                                ("null", NullSymbol),
                                ("String", StrSymbol))
  val context: TypeContext = new TypeContext
  primitives.map((x) => context.put(x._1, x._2))
}
