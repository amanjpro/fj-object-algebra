package ch.usi.inf.l3.ifjexpr.lang



import ch.usi.inf.l3._
import elang.typecheck._
import elang.namer._
import elang.ast._
import elang.analyzer._
import elang.eval._
import elang.debug._

import fj.typecheck._
import fj.namer._
import fj.ast._
import fj.analyzer._
import fj.eval._
import fj.debug._
import fj.lang._

import fjexpr.typecheck._
import fjexpr.namer._
import fjexpr.ast._
import fjexpr.analyzer._
import fjexpr.eval._
import fjexpr.debug._

import ifj.typecheck._
import ifj.namer._
import ifj.ast._
import ifj.analyzer._
import ifj.eval._
import ifj.debug._




trait IFJExprCompilerTrait extends FJCompilerTrait {

  object IFJExprNamer extends FJExprNamer with IFJNamer {
    private val primitives = List(("int", IntSymbol),
                                  ("boolean", BoolSymbol),
                                  ("double", FloatSymbol),
                                  ("null", NullSymbol),
                                  ("String", StrSymbol))
    val context: TypeContext = new TypeContext
    primitives.map((x) => context.put(x._1, x._2))
  }

  trait IFJExprLangAlg extends FJLangAlg {

    override val a = new FJExprAlgAST with IFJAlgAST {}
    override val b = IFJExprNamer
    override val c = new {
      val namer = b
    } with FJExprAnalyzers with IFJAnalyzers
    override val d = new {
      val namer = b
    } with FJExprTypers with IFJTypers
    override val e = new FJExprEval with IFJEval {}
    override val f = new FJExprDebug with IFJAlgDebug {}
 


    // From FJExpr
    def BinOp(lhs: Expr with Others, op: Bop, 
        rhs: Expr with Others, pos: Position, 
        symbol: UseSymbol): BinOp with Others = {

      val v1 = a.BinOp(lhs, op, rhs, pos, symbol)
      val v2 = b.BinOp(lhs, op, rhs, pos, symbol)
      val v3 = c.BinOp(lhs, op, rhs, pos, symbol)
      val v4 = d.BinOp(lhs, op, rhs, pos, symbol)
      val v5 = e.BinOp(lhs, op, rhs, pos, symbol)
      val v6 = f.BinOp(lhs, op, rhs, pos, symbol)

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

      val v1 = a.UniOp(op, expr, pos, symbol)
      val v2 = b.UniOp(op, expr, pos, symbol)
      val v3 = c.UniOp(op, expr, pos, symbol)
      val v4 = d.UniOp(op, expr, pos, symbol)
      val v5 = e.UniOp(op, expr, pos, symbol)
      val v6 = f.UniOp(op, expr, pos, symbol)

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
      val v1 = a.Literal(v, pos)
      val v2 = b.Literal(v, pos)
      val v3 = c.Literal(v, pos)
      val v4 = d.Literal(v, pos)
      val v5 = e.Literal(v, pos)
      val v6 = f.Literal(v, pos)
      
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
      val v1 = a.Literal(v, pos)
      val v2 = b.Literal(v, pos)
      val v3 = c.Literal(v, pos)
      val v4 = d.Literal(v, pos)
      val v5 = e.Literal(v, pos)
      val v6 = f.Literal(v, pos)
      
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
      val v1 = a.Literal(v, pos)
      val v2 = b.Literal(v, pos)
      val v3 = c.Literal(v, pos)
      val v4 = d.Literal(v, pos)
      val v5 = e.Literal(v, pos)
      val v6 = f.Literal(v, pos)
      
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
      val v1 = a.Literal(v, pos)
      val v2 = b.Literal(v, pos)
      val v3 = c.Literal(v, pos)
      val v4 = d.Literal(v, pos)
      val v5 = e.Literal(v, pos)
      val v6 = f.Literal(v, pos)
      
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
      val v1 = a.NullLiteral(pos)
      val v2 = b.NullLiteral(pos)
      val v3 = c.NullLiteral(pos)
      val v4 = d.NullLiteral(pos)
      val v5 = e.NullLiteral(pos)
      val v6 = f.NullLiteral(pos)
      
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


    // From IFJ

    def Interface(n: String, ps: List[Ident with Others], 
                  ms: List[AbstractMethod with Others], po: Position,
                  sym: ClassSymbol): Interface with Others = {
      val v1 = a.Interface(n, ps, ms, po, sym)
      val v2 = b.Interface(n, ps, ms, po, sym)
      val v3 = c.Interface(n, ps, ms, po, sym)
      val v4 = d.Interface(n, ps, ms, po, sym)
      val v5 = e.Interface(n, ps, ms, po, sym)
      val v6 = f.Interface(n, ps, ms, po, sym)

      new Interface with Namer with Analyzer with 
                        TypeCheck with Eval with Show {
        //Tree
        val name = v1.name
        val parents = v1.parents
        val methods = v1.methods
        val pos = v1.pos


        // Namer
        def nameIt(owner: Symbol): Unit = v2.nameIt(owner)
        

        // Analyzer
        def bind(encl: Symbol): Unit = v3.bind(encl)
        val treeName: String = v3.treeName
        val symbol = v1.symbol

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

    def ClassDef(n: String, p: Ident with Others, 
                is: List[Ident with Others], 
                fs: List[ValDef with Others],
                con: ConstDef with Others, ms: List[MethodDef with Others], 
                po: Position, sym: ClassSymbol): IClassDef with Others = {
      val v1 = a.ClassDef(n, p, is, fs, con, ms, po, sym)
      val v2 = b.ClassDef(n, p, is, fs, con, ms, po, sym)
      val v3 = c.ClassDef(n, p, is, fs, con, ms, po, sym)
      val v4 = d.ClassDef(n, p, is, fs, con, ms, po, sym)
      val v5 = e.ClassDef(n, p, is, fs, con, ms, po, sym)
      val v6 = f.ClassDef(n, p, is, fs, con, ms, po, sym)

      new IClassDef with Namer with Analyzer with 
                        TypeCheck with Eval with Show {
        //Tree
        val name = v1.name
        val parent = v1.parent
        val fields = v1.fields
        val impls = v1.impls
        val const = v1.const
        val methods = v1.methods
        val pos = v1.pos


        // Namer
        def nameIt(owner: Symbol): Unit = v2.nameIt(owner)
        

        // Analyzer
        def bind(encl: Symbol): Unit = v3.bind(encl)
        val treeName: String = v3.treeName
        val symbol = v1.symbol

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

    override def ClassDef(n: String, p: Ident with Others, 
                fs: List[ValDef with Others],
                con: ConstDef with Others, ms: List[MethodDef with Others], 
                po: Position, sym: ClassSymbol): ClassDef with Others = {
      this.ClassDef(n, p, Nil, fs, con, ms, po, sym) 
    }


    override def MethodDef(tpe: Ident with Others, name: String, 
          params: List[ValDef with Others], 
          body: Expr with Others, pos: Position, 
          symbol: TermSymbol): MethodDef with Others = {
      val v1 = a.MethodDef(tpe, name, params, body, pos, symbol)
      val v2 = b.MethodDef(tpe, name, params, body, pos, symbol)
      val v3 = c.MethodDef(tpe, name, params, body, pos, symbol)
      val v4 = d.MethodDef(tpe, name, params, body, pos, symbol)
      val v5 = e.MethodDef(tpe, name, params, body, pos, symbol)
      val v6 = f.MethodDef(tpe, name, params, body, pos, symbol)

      new IMethodDef with Namer with Analyzer with 
                    TypeCheck with Eval with Show {
        //Tree
        val t = v1.t
        val params = v1.params
        val name = v1.name
        val body = v1.body
        val pos = v1.pos


        // Namer
        def nameIt(owner: Symbol): Unit = v2.nameIt(owner)
        

        // Analyzer
        def bind(encl: Symbol): Unit = v3.bind(encl)
        val treeName: String = v3.treeName
        val symbol = v1.symbol

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

    def AbstractMethod(tpe: Ident with Others, name: String, 
          params: List[ValDef with Others], 
          pos: Position, 
          symbol: TermSymbol): AbstractMethod with Others = {
      val v1 = a.AbstractMethod(tpe, name, params, pos, symbol)
      val v2 = b.AbstractMethod(tpe, name, params, pos, symbol)
      val v3 = c.AbstractMethod(tpe, name, params, pos, symbol)
      val v4 = d.AbstractMethod(tpe, name, params, pos, symbol)
      val v5 = e.AbstractMethod(tpe, name, params, pos, symbol)
      val v6 = f.AbstractMethod(tpe, name, params, pos, symbol)

      new AbstractMethod with Namer with Analyzer with 
                    TypeCheck with Eval with Show {
        //Tree
        val t = v1.t
        val params = v1.params
        val name = v1.name
        val pos = v1.pos


        // Namer
        def nameIt(owner: Symbol): Unit = v2.nameIt(owner)
        

        // Analyzer
        def bind(encl: Symbol): Unit = v3.bind(encl)
        val treeName: String = v3.treeName
        val symbol = v1.symbol

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

    override def New(i: Ident with Others, args: List[Expr with Others], 
              pos: Position, sym: UseSymbol): New with Others = {
      val v1 = a.New(i, args, pos, sym)
      val v2 = b.New(i, args, pos, sym)
      val v3 = c.New(i, args, pos, sym)
      val v4 = d.New(i, args, pos, sym)
      val v5 = e.New(i, args, pos, sym)
      val v6 = f.New(i, args, pos, sym)

      new New with Namer with Analyzer with TypeCheck with Eval with Show {
        //Tree
        val id = v1.id
        val args = v1.args
        val pos = v1.pos


        // Namer
        def nameIt(owner: Symbol): Unit = v2.nameIt(owner)
        

        // Analyzer
        def bind(encl: Symbol): Unit = v3.bind(encl)
        val treeName: String = v3.treeName
        val symbol = v1.symbol

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

  object IFJExprLangAlgObject extends IFJExprLangAlg
}
