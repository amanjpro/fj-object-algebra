package ch.usi.inf.l3.fjexpr.lang



import ch.usi.inf.l3._
import elang.ast._
import elang.namer._
import elang.analyzer._
import elang.typecheck._
import elang.eval._
import elang.debug._
import fj.debug._
import fj.ast._
import fj.namer._
import fj.analyzer._
import fj.typecheck._
import fj.eval._
import fj.lang._
import fjexpr.ast._
import fjexpr.namer._
import fjexpr.typecheck._
import fjexpr.analyzer._
import fjexpr.eval._
import fjexpr.debug._


trait FJExprCompilerTrait extends FJCompilerTrait {
  trait FJExprLangAlg extends GFJExprAlg[Tree with Others, 
                                         Expr with Others, 
                                         Program with Others, 
                                         ClassDef with Others, 
                                         ValDef with Others, 
                                         ConstDef with Others, 
                                         MethodDef with Others, 
                                         FieldInit with Others, 
                                         Super with Others, 
                                         Ident with Others, 
                                         This with Others, 
                                         Select with Others, 
                                         Apply with Others, 
                                         New with Others, 
                                         Cast with Others,
                                         BinOp with Others,
                                         UniOp with Others,
                                         Literal with Others] with FJLangAlg {



    val ae = FJExprAlgAST
    val be = FJExprNamer
    val ce = new FJExprAnalyzer(be)
    val de = new FJExprTyper(be)
    val ee = FJExprEval
    val fe = FJExprDebug

    def BinOp(lhs: Expr with Others, op: Bop, 
        rhs: Expr with Others, pos: Position, 
        symbol: UseSymbol): BinOp with Others = {

      val v1 = ae.BinOp(lhs, op, rhs, pos, symbol)
      val v2 = be.BinOp(lhs, op, rhs, pos, symbol)
      val v3 = ce.BinOp(lhs, op, rhs, pos, symbol)
      val v4 = de.BinOp(lhs, op, rhs, pos, symbol)
      val v5 = ee.BinOp(lhs, op, rhs, pos, symbol)
      val v6 = fe.BinOp(lhs, op, rhs, pos, symbol)

      new BinOp with Namer with Analyzer with TypeCheck with Eval with Show {
        //Tree
        val lhs = v1.lhs
        val op = v1.op
        val rhs = v1.rhs
        val pos = v1.pos
        val symbol = v1.symbol


        // Namer
        def nameIt(owner: Symbol): Unit = v2.nameIt(owner)
        

        // Analyzer
        def bind(encl: Symbol): Unit = v3.bind(encl)
        val treeName: String = v3.treeName

        // TypeCheck
        def check: Result = v4.check
        

        // Eval
        def eval(env: Store): (Value, Store) = v5.eval(env)
        def index: Unit = v5.index
        
        // Show
        def show(col: Int = 0): String = v6.show(col)
        def loc(col: Int = 0): String = v6.loc(col)
      }
    }

    def UniOp(op: Uop, expr: Expr with Others, 
        pos: Position, symbol: UseSymbol): UniOp with Others = {

      val v1 = ae.UniOp(op, expr, pos, symbol)
      val v2 = be.UniOp(op, expr, pos, symbol)
      val v3 = ce.UniOp(op, expr, pos, symbol)
      val v4 = de.UniOp(op, expr, pos, symbol)
      val v5 = ee.UniOp(op, expr, pos, symbol)
      val v6 = fe.UniOp(op, expr, pos, symbol)

      new UniOp with Namer with Analyzer with TypeCheck with Eval with Show {
        //Tree
        val op = v1.op
        val expr = v1.expr
        val pos = v1.pos
        val symbol = v1.symbol


        // Namer
        def nameIt(owner: Symbol): Unit = v2.nameIt(owner)
        

        // Analyzer
        def bind(encl: Symbol): Unit = v3.bind(encl)
        val treeName: String = v3.treeName

        // TypeCheck
        def check: Result = v4.check
        

        // Eval
        def eval(env: Store): (Value, Store) = v5.eval(env)
        def index: Unit = v5.index
        
        // Show
        def show(col: Int = 0): String = v6.show(col)
        def loc(col: Int = 0): String = v6.loc(col)
      }
    }

    def Literal(v: Int, pos: Position): Literal with Others = {
      val v1 = ae.Literal(v, pos)
      val v2 = be.Literal(v, pos)
      val v3 = ce.Literal(v, pos)
      val v4 = de.Literal(v, pos)
      val v5 = ee.Literal(v, pos)
      val v6 = fe.Literal(v, pos)
      
      new Literal with Namer with Analyzer with TypeCheck with Eval with Show {
        //Tree
        val v = v1.v
        val pos = v1.pos
        val symbol = v1.symbol


        // Namer
        def nameIt(owner: Symbol): Unit = v2.nameIt(owner)
        

        // Analyzer
        def bind(encl: Symbol): Unit = v3.bind(encl)
        val treeName: String = v3.treeName

        // TypeCheck
        def check: Result = v4.check
        

        // Eval
        def eval(env: Store): (Value, Store) = v5.eval(env)
        def index: Unit = v5.index
        
        // Show
        def show(col: Int = 0): String = v6.show(col)
        def loc(col: Int = 0): String = v6.loc(col)
      }
    }
    def Literal(v: Double, pos: Position): Literal with Others = {
      val v1 = ae.Literal(v, pos)
      val v2 = be.Literal(v, pos)
      val v3 = ce.Literal(v, pos)
      val v4 = de.Literal(v, pos)
      val v5 = ee.Literal(v, pos)
      val v6 = fe.Literal(v, pos)
      
      new Literal with Namer with Analyzer with TypeCheck with Eval with Show {
        //Tree
        val v = v1.v
        val pos = v1.pos
        val symbol = v1.symbol


        // Namer
        def nameIt(owner: Symbol): Unit = v2.nameIt(owner)
        

        // Analyzer
        def bind(encl: Symbol): Unit = v3.bind(encl)
        val treeName: String = v3.treeName

        // TypeCheck
        def check: Result = v4.check
        

        // Eval
        def eval(env: Store): (Value, Store) = v5.eval(env)
        def index: Unit = v5.index
        
        // Show
        def show(col: Int = 0): String = v6.show(col)
        def loc(col: Int = 0): String = v6.loc(col)
      }
    }
    def Literal(v: Boolean, pos: Position): Literal with Others = {
      val v1 = ae.Literal(v, pos)
      val v2 = be.Literal(v, pos)
      val v3 = ce.Literal(v, pos)
      val v4 = de.Literal(v, pos)
      val v5 = ee.Literal(v, pos)
      val v6 = fe.Literal(v, pos)
      
      new Literal with Namer with Analyzer with TypeCheck with Eval with Show {
        //Tree
        val v = v1.v
        val pos = v1.pos
        val symbol = v1.symbol


        // Namer
        def nameIt(owner: Symbol): Unit = v2.nameIt(owner)
        

        // Analyzer
        def bind(encl: Symbol): Unit = v3.bind(encl)
        val treeName: String = v3.treeName

        // TypeCheck
        def check: Result = v4.check
        

        // Eval
        def eval(env: Store): (Value, Store) = v5.eval(env)
        def index: Unit = v5.index
        
        // Show
        def show(col: Int = 0): String = v6.show(col)
        def loc(col: Int = 0): String = v6.loc(col)
      }
    }
    def Literal(v: String, pos: Position): Literal with Others = {
      val v1 = ae.Literal(v, pos)
      val v2 = be.Literal(v, pos)
      val v3 = ce.Literal(v, pos)
      val v4 = de.Literal(v, pos)
      val v5 = ee.Literal(v, pos)
      val v6 = fe.Literal(v, pos)
      
      new Literal with Namer with Analyzer with TypeCheck with Eval with Show {
        //Tree
        val v = v1.v
        val pos = v1.pos
        val symbol = v1.symbol


        // Namer
        def nameIt(owner: Symbol): Unit = v2.nameIt(owner)
        

        // Analyzer
        def bind(encl: Symbol): Unit = v3.bind(encl)
        val treeName: String = v3.treeName

        // TypeCheck
        def check: Result = v4.check
        

        // Eval
        def eval(env: Store): (Value, Store) = v5.eval(env)
        def index: Unit = v5.index
        
        // Show
        def show(col: Int = 0): String = v6.show(col)
        def loc(col: Int = 0): String = v6.loc(col)
      }
    }
    def NullLiteral(pos: Position): Literal with Others = {
      val v1 = ae.NullLiteral(pos)
      val v2 = be.NullLiteral(pos)
      val v3 = ce.NullLiteral(pos)
      val v4 = de.NullLiteral(pos)
      val v5 = ee.NullLiteral(pos)
      val v6 = fe.NullLiteral(pos)
      
      new Literal with Namer with Analyzer with TypeCheck with Eval with Show {
        //Tree
        val v = NullConst
        val pos = v1.pos
        val symbol = v1.symbol


        // Namer
        def nameIt(owner: Symbol): Unit = v2.nameIt(owner)
        

        // Analyzer
        def bind(encl: Symbol): Unit = v3.bind(encl)
        val treeName: String = v3.treeName

        // TypeCheck
        def check: Result = v4.check
        

        // Eval
        def eval(env: Store): (Value, Store) = v5.eval(env)
        def index: Unit = v5.index
        
        // Show
        def show(col: Int = 0): String = v6.show(col)
        def loc(col: Int = 0): String = v6.loc(col)
      }
    }
  }

  object FJExprLangAlgObject extends FJExprLangAlg
}

