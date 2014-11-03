package ch.usi.inf.l3.fjexpr.eval


import ch.usi.inf.l3._
import elang.ast._
import elang.namer._
import elang.typecheck._
import elang.eval._
import fj.ast._
import fj.eval._
import fj.namer._
import fjexpr.namer._
import fjexpr.ast._


case class IntValue(v: Int) extends Value 
case class StringValue(v: String) extends Value 
case class FloatValue(v: Double) extends Value 
case class BoolValue(v: Boolean) extends Value 
case object NullValue extends Value 

trait FJExprEval extends FJExprAlg[Eval with Tree] with FJEval {

  def BinOp(lhs: Eval with Tree, op: Bop, rhs: Eval with Tree, 
        po: Position, sym: UseSymbol): Eval with Tree = {
    new Eval with Tree {
      def eval(env: Store): (Value, Store) = {
        val (v1, env2) = lhs.eval(env)
        val (v2, env3) = rhs.eval(env2)
        val r = (v1, op, v2) match {
          case (StringValue(a), Add, StringValue(b)) => 
            StringValue(a + b.toString)
          case (StringValue(a), Add, BoolValue(b)) => 
            StringValue(a + b.toString)
          case (StringValue(a), Add, FloatValue(b)) => 
            StringValue(a + b.toString)
          case (StringValue(a), Add, IntValue(b)) => 
            StringValue(a + b.toString)
          case (StringValue(a), Eq, StringValue(b)) => BoolValue(a == b)
          case (StringValue(a), Neq, StringValue(b)) => BoolValue(a != b)

          case (IntValue(a), Add, IntValue(b)) => IntValue(a + b)
          case (IntValue(a), Sub, IntValue(b)) => IntValue(a - b)
          case (IntValue(a), Mul, IntValue(b)) => IntValue(a * b)
          case (IntValue(a), Div, IntValue(b)) => IntValue(a / b)
          case (IntValue(a), Mod, IntValue(b)) => IntValue(a % b)
          case (IntValue(a), Gt, IntValue(b)) => BoolValue(a > b)
          case (IntValue(a), Lt, IntValue(b)) => BoolValue(a < b)
          case (IntValue(a), Geq, IntValue(b)) => BoolValue(a >= b)
          case (IntValue(a), Leq, IntValue(b)) => BoolValue(a <= b)
          case (IntValue(a), Eq, IntValue(b)) => BoolValue(a == b)
          case (IntValue(a), Neq, IntValue(b)) => BoolValue(a != b)

          case (FloatValue(a), Add, IntValue(b)) => FloatValue(a + b)
          case (FloatValue(a), Sub, IntValue(b)) => FloatValue(a - b)
          case (FloatValue(a), Mul, IntValue(b)) => FloatValue(a * b)
          case (FloatValue(a), Div, IntValue(b)) => FloatValue(a / b)
          case (FloatValue(a), Gt, IntValue(b)) => BoolValue(a > b)
          case (FloatValue(a), Lt, IntValue(b)) => BoolValue(a < b)
          case (FloatValue(a), Geq, IntValue(b)) => BoolValue(a >= b)
          case (FloatValue(a), Leq, IntValue(b)) => BoolValue(a <= b)


          case (FloatValue(a), Add, FloatValue(b)) => FloatValue(a + b)
          case (FloatValue(a), Sub, FloatValue(b)) => FloatValue(a - b)
          case (FloatValue(a), Mul, FloatValue(b)) => FloatValue(a * b)
          case (FloatValue(a), Div, FloatValue(b)) => FloatValue(a / b)

          case (FloatValue(a), Gt, FloatValue(b)) => BoolValue(a > b)
          case (FloatValue(a), Lt, FloatValue(b)) => BoolValue(a < b)
          case (FloatValue(a), Geq, FloatValue(b)) => BoolValue(a >= b)
          case (FloatValue(a), Leq, FloatValue(b)) => BoolValue(a <= b)
          case (FloatValue(a), Eq, FloatValue(b)) => BoolValue(a == b)
          case (FloatValue(a), Neq, FloatValue(b)) => BoolValue(a != b)

          case (BoolValue(a), And, BoolValue(b)) => BoolValue(a && b)
          case (BoolValue(a), Or, BoolValue(b)) => BoolValue(a || b)
          case (BoolValue(a), Eq, BoolValue(b)) => BoolValue(a == b)
          case (BoolValue(a), Neq, BoolValue(b)) => BoolValue(a != b)
          case _ => NoValue
        }
        (r, env3)
      }
      val symbol = sym
      def index: Unit = ()
      val pos: Position = po
      type S = UseSymbol
    }
  }

  def UniOp(op: Uop, expr: Eval with Tree, 
        po: Position, sym: UseSymbol): Eval with Tree = {
    new Eval with Tree {
      def eval(env: Store): (Value, Store) = {
        val (v, env2) = expr.eval(env)
        val r = (op, v) match {
          case (Posi, IntValue(b)) => v
          case (Posi, FloatValue(b)) => v
          case (Neg, IntValue(b)) => IntValue(-b)
          case (Neg, FloatValue(b)) => FloatValue(-b)
          case (Not, BoolValue(b)) => BoolValue(!b)
          case _ => NoValue
        }
        (r, env2)
      }
      val symbol = sym
      def index: Unit = ()
      val pos: Position = po
      type S = UseSymbol
    }
  }

  def Literal(v: Int, po: Position): Eval with Tree = {
    new Eval with Tree {
      def eval(env: Store): (Value, Store) = (IntValue(v), env)
      val symbol = IntSymbol
      def index: Unit = ()
      val pos: Position = po
      type S = LitSymbol
    }
  }
  def Literal(v: Double, po: Position): Eval with Tree = {
    new Eval with Tree {
      def eval(env: Store): (Value, Store) = (FloatValue(v), env)
      val symbol = FloatSymbol
      def index: Unit = ()
      val pos: Position = po
      type S = LitSymbol
    }
  }
  def Literal(v: Boolean, po: Position): Eval with Tree = {
    new Eval with Tree {
      def eval(env: Store): (Value, Store) = (BoolValue(v), env)
      val symbol = BoolSymbol
      def index: Unit = ()
      val pos: Position = po
      type S = LitSymbol
    }
  }
  def Literal(v: String, po: Position): Eval with Tree = {
    new Eval with Tree {
      def eval(env: Store): (Value, Store) = (StringValue(v), env)
      val symbol = StrSymbol
      def index: Unit = ()
      val pos: Position = po
      type S = LitSymbol
    }
  }
  def NullLiteral(po: Position): Eval with Tree = {
    new Eval with Tree {
      def eval(env: Store): (Value, Store) = (NullValue, env)
      val symbol = NullSymbol
      def index: Unit = ()
      val pos: Position = po
      type S = LitSymbol
    }
  }
}


object FJExprEval extends FJExprEval
