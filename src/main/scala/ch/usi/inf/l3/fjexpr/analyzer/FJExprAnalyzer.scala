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
          s: UseSymbol): Analyzer with Tree = {
    new Analyzer with Tree {
      type S = UseSymbol
      val pos: Position = p
      val symbol: UseSymbol = s
      val treeName: String = o.name
      def bind(encl: Symbol): Unit = {
        s.owner = encl
        l.bind(encl)
        r.bind(encl)
        s.uses = deriveSymbol(l.symbol.tpe, o, r.symbol.tpe)
      }
    }
  }

  def UniOp(o: Uop, e: Analyzer with Tree, 
        p: Position, s: UseSymbol): Analyzer with Tree = {
    new Analyzer with Tree {
      type S = UseSymbol
      val pos: Position = p
      val symbol: UseSymbol = s
      val treeName: String = o.name
      def bind(encl: Symbol): Unit = {
        s.owner = encl
        e.bind(encl)
        s.uses = deriveSymbol(o, e.symbol.tpe)
      }
    }
  }


  def Literal(va: Int, p: Position): Analyzer with Tree = {
    new Analyzer with Tree {
      type S = LitSymbol
      val pos: Position = p
      val treeName: String = s"lit ${va.toString}"
      def bind(encl: Symbol): Unit = ()
      val symbol: LitSymbol = IntSymbol
    }
  }

  def Literal(va: Double, p: Position): Analyzer with Tree = {
    new Analyzer with Tree {
      type S = LitSymbol
      val pos: Position = p
      val treeName: String = s"lit ${va.toString}"
      def bind(encl: Symbol): Unit = ()
      val symbol: LitSymbol = FloatSymbol
    }
  }

  def Literal(va: Boolean, p: Position): Analyzer with Tree = {
    new Analyzer with Tree {
      type S = LitSymbol
      val pos: Position = p
      val treeName: String = s"lit ${va.toString}"
      def bind(encl: Symbol): Unit = ()
      val symbol: LitSymbol = BoolSymbol
    }
  }

  def Literal(va: String, p: Position): Analyzer with Tree = {
    new Analyzer with Tree {
      type S = LitSymbol
      val pos: Position = p
      val treeName: String = s"lit ${va.toString}"
      def bind(encl: Symbol): Unit = ()
      val symbol: LitSymbol = StrSymbol
    }
  }

  def NullLiteral(p: Position): Analyzer with Tree = {
    new Analyzer with Tree {
      type S = LitSymbol
      val pos: Position = p
      val treeName: String = s"lit null"
      def bind(encl: Symbol): Unit = ()
      val symbol: LitSymbol = NullSymbol
    }
  }


  // Private utilities

  def deriveSymbol(op: Uop, t: Type): Symbol = {
    (op, t) match {
      case (Not, BoolType) => BoolSymbol
      case (Posi, IntType) => IntSymbol
      case (Posi, FloatType) => FloatSymbol
      case (Neg, IntType) => IntSymbol
      case (Neg, FloatType) => FloatSymbol
      case (_, _) => NoSymbol
    }
  }
  def deriveSymbol(t1: Type, op: Bop, t2: Type): Symbol = {
    def isNumeric(op: Bop): Boolean = 
      op == Add || op == Sub || op == Mul || op == Div || op == Mod
    def isComparative(op: Bop) = 
      op == Gt || op == Lt || op == Leq || op == Geq
    def isLogic(op: Bop): Boolean = op == And || op == Or
    def isEq(op: Bop): Boolean = op == Eq || op == Neq
    (t1, op, t2) match {
      case (StrType, Add, _: PrimitiveType) => StrSymbol
      case (IntType, Mod, IntType) => IntSymbol
      case (_, Mod, _) => NoSymbol
      case (FloatType, o, FloatType) if isNumeric(o) => FloatSymbol
      case (FloatType, o, IntType) if isNumeric(o) => FloatSymbol
      case (IntType, o, IntType) if isNumeric(o) => IntSymbol
      case (FloatType, o, FloatType) if isComparative(o) => BoolSymbol
      case (FloatType, o, IntType) if isComparative(o) => BoolSymbol
      case (IntType, o, IntType) if isComparative(o) => BoolSymbol
      case (BoolType, o, BoolType) if isLogic(o) => BoolSymbol
      case (a: PrimitiveType, o, b: PrimitiveType) if a == b && isEq(o) => 
        BoolSymbol
      case (_, _, _) => NoSymbol
    } 
  }
}
class FJExprAnalyzer(val namer: FJExprNamer) extends FJExprAnalyzers 
