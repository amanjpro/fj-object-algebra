package ch.usi.inf.l3.fjexpr.analyzer


import ch.usi.inf.l3._
import elang.namer._
import elang.typecheck._
import elang.ast._
import fjexpr.ast._
import fjexpr.namer._
import fjexpr.typecheck._
import elang.analyzer._
import fj.analyzer._
import fj.namer._

trait FJExprAnalyzers extends FJExprAlg[Analyzer with Tree] with FJAnalyzers {

  def BinOp(l: Analyzer with Tree, o: Bop, 
          r: Analyzer with Tree, p: Position, 
          s: LitSymbol): Analyzer with Tree = {
    new Analyzer with Tree {
      type S = LitSymbol
      val pos: Position = p
      val symbol: LitSymbol = s
      val treeName: String = o.name
      def bind(encl: Symbol): Unit = {
        s.owner = encl
        l.bind(encl)
        r.bind(encl)
        s.tpe = deriveType(l.symbol.tpe, o, r.symbol.tpe)
      }
    }
  }

  def UniOp(o: Uop, e: Analyzer with Tree, 
        p: Position, s: LitSymbol): Analyzer with Tree = {
    new Analyzer with Tree {
      type S = LitSymbol
      val pos: Position = p
      val symbol: LitSymbol = s
      val treeName: String = o.name
      def bind(encl: Symbol): Unit = {
        s.owner = encl
        e.bind(encl)
        s.tpe = deriveType(o, e.symbol.tpe)
      }
    }
  }


  def Literal(va: Int, p: Position, s: LitSymbol): Analyzer with Tree = {
    new Analyzer with Tree {
      type S = LitSymbol
      val pos: Position = p
      val treeName: String = s"lit ${va.toString}"
      def bind(encl: Symbol): Unit = {
        s.owner = encl
        s.tpe = IntType
      }
      val symbol: LitSymbol = s
    }
  }

  def Literal(va: Double, p: Position, s: LitSymbol): Analyzer with Tree = {
    new Analyzer with Tree {
      type S = LitSymbol
      val pos: Position = p
      val treeName: String = s"lit ${va.toString}"
      def bind(encl: Symbol): Unit = {
        s.owner = encl
        s.tpe = FloatType
      }
      val symbol: LitSymbol = s
    }
  }

  def Literal(va: Boolean, p: Position, s: LitSymbol): Analyzer with Tree = {
    new Analyzer with Tree {
      type S = LitSymbol
      val pos: Position = p
      val treeName: String = s"lit ${va.toString}"
      def bind(encl: Symbol): Unit = {
        s.owner = encl
        s.tpe = BoolType
      }
      val symbol: LitSymbol = s
    }
  }

  def Literal(va: String, p: Position, s: LitSymbol): Analyzer with Tree = {
    new Analyzer with Tree {
      type S = LitSymbol
      val pos: Position = p
      val treeName: String = s"lit ${va.toString}"
      def bind(encl: Symbol): Unit = {
        s.owner = encl
        s.tpe = StrType
      }
      val symbol: LitSymbol = s
    }
  }

  def NullLiteral(p: Position, s: LitSymbol): Analyzer with Tree = {
    new Analyzer with Tree {
      type S = LitSymbol
      val pos: Position = p
      val treeName: String = s"lit null"
      def bind(encl: Symbol): Unit = {
        s.owner = encl
        s.tpe = NullType
      }
      val symbol: LitSymbol = s
    }
  }


  // Private utilities

  def deriveType(op: Uop, t: Type): Type = {
    (op, t) match {
      case (Not, BoolType) => BoolType
      case (Posi, IntType) => IntType
      case (Posi, FloatType) => FloatType
      case (Neg, IntType) => IntType
      case (Neg, FloatType) => FloatType
      case (_, _) => NoType
    }
  }
  def deriveType(t1: Type, op: Bop, t2: Type): Type = {
    def isNumeric(op: Bop): Boolean = 
      op == Add || op == Sub || op == Mul || op == Div || op == Mod
    def isComparative(op: Bop) = 
      op == Gt || op == Lt || op == Leq || op == Geq
    def isLogic(op: Bop): Boolean = op == And || op == Or
    def isEq(op: Bop): Boolean = op == Eq || op == Neq
    (t1, op, t2) match {
      case (StrType, Add, _: PrimitiveType) => StrType
      case (IntType, Mod, IntType) => IntType
      case (_, Mod, _) => NoType
      case (FloatType, o, FloatType) if isNumeric(o) => FloatType
      case (FloatType, o, IntType) if isNumeric(o) => FloatType
      case (IntType, o, IntType) if isNumeric(o) => IntType
      case (FloatType, o, FloatType) if isComparative(o) => BoolType
      case (FloatType, o, IntType) if isComparative(o) => BoolType
      case (IntType, o, IntType) if isComparative(o) => BoolType
      case (BoolType, o, BoolType) if isLogic(o) => BoolType
      case (a: PrimitiveType, o, b: PrimitiveType) if a == b && isEq(o) => 
        BoolType
      case (_, _, _) => NoType
    } 
  }
}
class FJExprAnalyzer(val namer: FJExprNamer) extends FJExprAnalyzers 
