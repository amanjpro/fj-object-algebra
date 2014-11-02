package ch.usi.inf.l3.fjexpr.typecheck


import ch.usi.inf.l3._
import fj.ast._
import elang.namer._
import fj.namer._
import fj.typecheck._
import elang.typecheck._
import elang.ast._
import fjexpr.ast._
import fjexpr.namer._


 

trait PrimitiveType extends TType {
  def isSubtypeOf(other: TType): Boolean = this == other
  def vars: List[TermSymbol] = Nil
  def methods: List[TermSymbol] = Nil
  def field(name: String): Option[TermSymbol] = None
  def method(name: String): Option[TermSymbol] = None
  def overrideCheck(mtpe: MethodType): Result = Success
}

case object IntType extends PrimitiveType {
  val name = "Int"
}
case object StrType extends PrimitiveType {
  val name = "String"
}

case object FloatType extends PrimitiveType {
  val name = "Float"
}

case object BoolType extends PrimitiveType {
  val name = "Boolean"
}

case object NullType extends PrimitiveType {
  val name = "Null"
}

trait FJExprTypers extends FJExprAlg[TypeCheck with Tree] with FJTypers {

  def BinOp(lhs: TypeCheck with Tree, op: Bop, 
      rhs: TypeCheck with Tree, po: Position, 
      sym: LitSymbol): TypeCheck with Tree = {
    new TypeCheck with Tree {
      type S = LitSymbol
      def check: Result = {
        val rl = lhs.check
        val rr = rhs.check
        val rt = symbol.tpe match {
          case NoType => Failure(s"${lhs.symbol.tpe} is not compatible with " +
                                 s"${rhs.symbol.tpe}")
          case _ => Success
        }
        rl && rr && rt
      }
      val symbol = sym
      val pos: Position = po
    }
  }

  def UniOp(op: Uop, expr: TypeCheck with Tree, 
      po: Position, sym: LitSymbol): TypeCheck with Tree = {
    new TypeCheck with Tree {
      type S = LitSymbol
      def check: Result = {
        val re = expr.check
        val rt = symbol.tpe match {
          case NoType => Failure(s"${expr.symbol.tpe} is not compatible with " +
                                 s"${op.name}")
          case _ => Success
        }
        re && rt 
      }
      val symbol = sym
      val pos: Position = po
    }
  }

  def Literal(v: Int, po: Position, sym: LitSymbol): TypeCheck with Tree = {
    new TypeCheck with Tree {
      type S = LitSymbol
      def check: Result = Success
      val symbol = sym
      val pos: Position = po
    }
  }
  def Literal(v: Double, po: Position, 
      sym: LitSymbol): TypeCheck with Tree = {
    new TypeCheck with Tree {
      type S = LitSymbol
      def check: Result = Success
      val symbol = sym
      val pos: Position = po
    }
  }
  def Literal(v: Boolean, po: Position, 
      sym: LitSymbol): TypeCheck with Tree = {
   new TypeCheck with Tree {
      type S = LitSymbol
      def check: Result = Success
      val symbol = sym
      val pos: Position = po
    }
  }
  def Literal(v: String, po: Position, 
      sym: LitSymbol): TypeCheck with Tree = {
    new TypeCheck with Tree {
      type S = LitSymbol
      def check: Result = Success
      val symbol = sym
      val pos: Position = po
    }
  }
  def NullLiteral(po: Position, sym: LitSymbol): TypeCheck with Tree = {
    new TypeCheck with Tree {
      type S = LitSymbol
      def check: Result = Success
      val symbol = sym
      val pos: Position = po
    }
  }
}


class FJExprTyper(val namer: FJExprNamer) extends FJExprTypers