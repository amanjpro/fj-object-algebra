package ch.usi.inf.l3.fjexpr.ast

import ch.usi.inf.l3._
import elang.ast._
import fj.ast._
import fjexpr.namer._
import fjexpr.typecheck._
import elang.namer._

trait Literal extends Expr {
  type S = LitSymbol
  val v: Constant
  val pos: Position
}


trait Constant {
  def getInt: Int = ???
  def getBool: Boolean = ???
  def getFloat: Double = ???
  def getStr: String = ???
}

case object NullConst extends Constant 
case class StrConst(v: String) extends Constant 
case class IntConst(v: Int) extends Constant 
case class BoolConst(v: Boolean) extends Constant 
case class FloatConst(v: Double) extends Constant 


trait Bop {
  val name: String
}
case object Add extends Bop {
  val name = "+"
}
case object Sub extends Bop {
  val name = "-"
}

case object Mul extends Bop {
  val name = "*"
}

case object Div extends Bop {
  val name = "/"
}

case object Mod extends Bop {
  val name = "%"
}

case object And extends Bop {
  val name = "&&"
}

case object Or extends Bop {
  val name = "||"
}

case object Gt extends Bop {
  val name = ">"
}

case object Lt extends Bop {
  val name = "<"
}

case object Geq extends Bop {
  val name = ">="
}

case object Leq extends Bop {
  val name = "<="
}

case object Eq extends Bop {
  val name = "=="
}

case object Neq extends Bop {
  val name = "!="
}


trait Uop {
  val name: String
}

case object Neg extends Uop {
  val name = "-"
}

case object Posi extends Uop {
  val name = "+"
}

case object Not extends Uop {
  val name = "!"
}


trait BinOp extends Expr {
  type S = UseSymbol
  val rhs: Expr
  val lhs: Expr
  val op: Bop
}

trait UniOp extends Expr {
  type S = UseSymbol
  val expr: Expr
  val op: Uop
}

trait GFJExprAlg[T, E <: T, P <: T, C <: T,
             VD <: T, CD <: T, MD <: T,
             FI <: T, S <: T,
             I <: E, Tz <: E, SE <: E, A <: E,
             N <: E, CA <: E, BO <: E,
             UO <: E, LI <: E] extends GFJAlg[T, E, P, C, VD, CD,
                                             MD, FI, S, I, Tz,
                                             SE, A, N, CA] {


  def BinOp(lhs: E, op: Bop, rhs: E, pos: Position, symbol: UseSymbol): BO

  def UniOp(op: Uop, expr: E, pos: Position, symbol: UseSymbol): UO

  def Literal(v: Int, pos: Position): LI
  def Literal(v: Double, pos: Position): LI
  def Literal(v: Boolean, pos: Position): LI
  def Literal(v: String, pos: Position): LI
  def NullLiteral(pos: Position): LI
}

trait FJExprAlg[E] extends GFJExprAlg[E, E, E, E, E, E, E, E, E,
                                      E, E, E, E, E, E, E, E, E] {
  def BinOp(lhs: E, op: Bop, rhs: E, pos: Position, symbol: UseSymbol): E

  def UniOp(op: Uop, expr: E, pos: Position, symbol: UseSymbol): E

  def Literal(v: Int, pos: Position): E
  def Literal(v: Double, pos: Position): E
  def Literal(v: Boolean, pos: Position): E
  def Literal(v: String, pos: Position): E
  def NullLiteral(pos: Position): E
}

trait FJExprAlgAST extends GFJExprAlg[Tree, Expr, Program, ClassDef,
                             ValDef, ConstDef, MethodDef,
                             FieldInit, Super, Ident, This,
                             Select, Apply, New, Cast,
                             BinOp, UniOp, Literal] 
                   with FJAlgAST {

  def BinOp(l: Expr, o: Bop, r: Expr, p: Position, sym: UseSymbol): BinOp = {
    new BinOp {
      val lhs = l
      val rhs = r
      val op = o
      val pos = p
      val symbol = sym
    }
  }

  def UniOp(o: Uop, e: Expr, p: Position, sym: UseSymbol): UniOp = {
    new UniOp {
      val op = o
      val expr = e
      val pos = p
      val symbol = sym
    }
  }

  def Literal(va: Int, p: Position): Literal = {
    new {
      val v = IntConst(va)
    } with Literal {
      val pos = p
      val symbol = IntSymbol
    }
  }
  def Literal(va: Double, p: Position): Literal = {
    new {
      val v = FloatConst(va)
    } with Literal {
      val pos = p
      val symbol = FloatSymbol
    }
  }

  def Literal(va: Boolean, p: Position): Literal = {
    new {
      val v = BoolConst(va)
    } with Literal {
      val pos = p
      val symbol = BoolSymbol
    }
  }

  def Literal(va: String, p: Position): Literal = {
    new {
      val v = StrConst(va)
    } with Literal {
      val pos = p
      val symbol = StrSymbol
    }
  }

  def NullLiteral(p: Position): Literal = {
    new {
      val v = NullConst
    } with Literal {
      val pos = p
      val symbol = NullSymbol
    }
  }

}

object FJExprAlgAST extends FJExprAlgAST
