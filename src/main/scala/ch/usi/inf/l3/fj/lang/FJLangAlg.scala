package ch.usi.inf.l3.fj.lang



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


// Tree, 
// Expr, 
// Program, 
// ClassDef,
// ValDef, 
// ConstDef, 
// MethodDef,
// FieldInit, 
// Super, 
// Ident, 
// This,
// Select, 
// Apply, 
// New, 
// Cast
//
trait FJCompilerTrait {

  type Others = Namer with Analyzer with TypeCheck with Eval with Show

  // trait FJLifter {
  //   def lift(a: Tree, b: Namer, c: Analyzer, d: TypeCheck, e: Eval,
  //         f: Show): Tree with Others = {
  //     new Tree with Namer with Analyzer with TypeCheck with Eval with Show {
  //       //Tree
  //       val symbol: a.S = a.symbol
  //       val pos: Position = a.pos
  //
  //
  //       // Namer
  //       def nameIt(owner: Symbol): Unit = b.nameIt(owner)
  //       
  //
  //       // Analyzer
  //       def bind(encl: Symbol): Unit = c.bind(encl)
  //       val treeName: String = c.treeName
  //
  //
  //
  //       // TypeCheck
  //       def check: Result = d.check
  //       
  //
  //
  //       // Eval
  //       def eval(env: Store): (Value, Store) = e.eval(env)
  //       def index: Unit = e.index
  //
  //
  //       // Show
  //       def show(col: Int = 0): String = f.show(col)
  //       def loc(col: Int = 0): String = f.loc(col)
  //     }
  //   }
  // }
  //
  // object FJLifter extends FJLifter
  //

  trait FJLangAlg extends GFJAlg[
    Tree with Others, 
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
    Cast with Others] {



    val a = FJAlgAST
    val b = FJNamer
    val c = new FJAnalyzer(b)
    val d = new FJTyper(b)
    val e = FJEval
    val f = FJAlgDebug


    def Program(cs: List[ClassDef with Others], 
              m: Expr with Others): Program with Others = {
      // FIXME: The ASTs should be traits (maybe supported by companion objects
      // So the following compiles
      val v1 = a.Program(cs, m)
      val v2 = b.Program(cs, m)
      val v3 = c.Program(cs, m)
      val v4 = d.Program(cs, m)
      val v5 = e.Program(cs, m)
      val v6 = f.Program(cs, m)

      new Program with Namer with Analyzer with TypeCheck with Eval with Show {
        //Tree
        val classes = v1.classes
        val main = v1.main
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
              fs: List[ValDef with Others],
              con: ConstDef with Others, ms: List[MethodDef with Others],
              po: Position, s: ClassSymbol): ClassDef with Others = {
      val v1 = a.ClassDef(n, p, fs, con, ms, po, s)
      val v2 = b.ClassDef(n, p, fs, con, ms, po, s)
      val v3 = c.ClassDef(n, p, fs, con, ms, po, s)
      val v4 = d.ClassDef(n, p, fs, con, ms, po, s)
      val v5 = e.ClassDef(n, p, fs, con, ms, po, s)
      val v6 = f.ClassDef(n, p, fs, con, ms, po, s)

      new ClassDef with Namer with Analyzer with TypeCheck with Eval with Show {
        //Tree
        val name = v1.name
        val parent = v1.parent
        val fields = v1.fields
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


    def ConstDef(tpe: Ident with Others, params: List[ValDef with Others], 
              su: Super with Others, finit: List[FieldInit with Others], 
              pos: Position, symbol: TermSymbol): ConstDef with Others = {
      val v1 = a.ConstDef(tpe, params, su, finit, pos, symbol)
      val v2 = b.ConstDef(tpe, params, su, finit, pos, symbol)
      val v3 = c.ConstDef(tpe, params, su, finit, pos, symbol)
      val v4 = d.ConstDef(tpe, params, su, finit, pos, symbol)
      val v5 = e.ConstDef(tpe, params, su, finit, pos, symbol)
      val v6 = f.ConstDef(tpe, params, su, finit, pos, symbol)

      new ConstDef with Namer with Analyzer with TypeCheck with Eval with Show {
        //Tree
        val t = v1.t
        val params = v1.params
        val su = v1.su
        val finit = v1.finit
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



    def FieldInit(name: Ident with Others, rhs: Expr with Others, 
              pos: Position, sym: UseSymbol): FieldInit with Others = {
      val v1 = a.FieldInit(name, rhs, pos, sym)
      val v2 = b.FieldInit(name, rhs, pos, sym)
      val v3 = c.FieldInit(name, rhs, pos, sym)
      val v4 = d.FieldInit(name, rhs, pos, sym)
      val v5 = e.FieldInit(name, rhs, pos, sym)
      val v6 = f.FieldInit(name, rhs, pos, sym)

      new FieldInit with Namer with Analyzer with TypeCheck with Eval with Show {
        //Tree
        val name = v1.name
        val rhs = v1.rhs
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



    def Super(exprs: List[Expr with Others], pos: Position, 
              symbol: UseSymbol): Super with Others = {
      val v1 = a.Super(exprs, pos, symbol)
      val v2 = b.Super(exprs, pos, symbol)
      val v3 = c.Super(exprs, pos, symbol)
      val v4 = d.Super(exprs, pos, symbol)
      val v5 = e.Super(exprs, pos, symbol)
      val v6 = f.Super(exprs, pos, symbol)

      new Super with Namer with Analyzer with TypeCheck with Eval with Show {
        //Tree
        val exprs = v1.exprs
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



    def MethodDef(tpe: Ident with Others, name: String, 
              params: List[ValDef with Others], body: Expr with Others, 
              pos: Position, symbol: TermSymbol): MethodDef with Others = {
      val v1 = a.MethodDef(tpe, name, params, body, pos, symbol)
      val v2 = b.MethodDef(tpe, name, params, body, pos, symbol)
      val v3 = c.MethodDef(tpe, name, params, body, pos, symbol)
      val v4 = d.MethodDef(tpe, name, params, body, pos, symbol)
      val v5 = e.MethodDef(tpe, name, params, body, pos, symbol)
      val v6 = f.MethodDef(tpe, name, params, body, pos, symbol)

      new MethodDef with Namer with Analyzer with TypeCheck with Eval with Show {
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



    def ValDef(tpe: Ident with Others, name: String, pos: Position, 
              symbol: TermSymbol): ValDef with Others  = {
      val v1 = a.ValDef(tpe, name, pos, symbol)
      val v2 = b.ValDef(tpe, name, pos, symbol)
      val v3 = c.ValDef(tpe, name, pos, symbol)
      val v4 = d.ValDef(tpe, name, pos, symbol)
      val v5 = e.ValDef(tpe, name, pos, symbol)
      val v6 = f.ValDef(tpe, name, pos, symbol)

      new ValDef with Namer with Analyzer with TypeCheck with Eval with Show {
        //Tree
        val t = v1.t
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



    def This(pos: Position, symbol: UseSymbol): This with Others = {
      val v1 = a.This(pos, symbol)
      val v2 = b.This(pos, symbol)
      val v3 = c.This(pos, symbol)
      val v4 = d.This(pos, symbol)
      val v5 = e.This(pos, symbol)
      val v6 = f.This(pos, symbol)

      new This with Namer with Analyzer with TypeCheck with Eval with Show {
        //Tree
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



    def Ident(name: String, pos: Position, 
              symbol: UseSymbol): Ident with Others = {
      val v1 = a.Ident(name, pos, symbol)
      val v2 = b.Ident(name, pos, symbol)
      val v3 = c.Ident(name, pos, symbol)
      val v4 = d.Ident(name, pos, symbol)
      val v5 = e.Ident(name, pos, symbol)
      val v6 = f.Ident(name, pos, symbol)

      new Ident with Namer with Analyzer with TypeCheck with Eval with Show {
        //Tree
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

    
    
    def Select(s: Expr with Others, m: String, 
              pos: Position, sym: UseSymbol): Select with Others  = {
      val v1 = a.Select(s, m, pos, sym)
      val v2 = b.Select(s, m, pos, sym)
      val v3 = c.Select(s, m, pos, sym)
      val v4 = d.Select(s, m, pos, sym)
      val v5 = e.Select(s, m, pos, sym)
      val v6 = f.Select(s, m, pos, sym)

      new Select with Namer with Analyzer with TypeCheck with Eval with Show {
        //Tree
        val s = v1.s
        val m = v1.m
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

    
    def Apply(expr: Expr with Others, 
            m: String, 
            args: List[Expr with Others], 
            pos: Position, sym: UseSymbol): Apply with Others = {
      val v1 = a.Apply(expr, m, args, pos, sym) // AST gen
      val v2 = b.Apply(expr, m, args, pos, sym) // Namer
      val v3 = c.Apply(expr, m, args, pos, sym) // Analyzer
      val v4 = d.Apply(expr, m, args, pos, sym) // Typer
      val v5 = e.Apply(expr, m, args, pos, sym) // Eval
      val v6 = f.Apply(expr, m, args, pos, sym) // Show

      new Apply with Namer with Analyzer with TypeCheck with Eval with Show {
        //Tree
        val expr = v1.expr
        val m = v1.m
        val args = v1.args
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



    def New(id: Ident with Others, args: List[Expr with Others], 
              pos: Position, sym: UseSymbol): New with Others = {
      val v1 = a.New(id, args, pos, sym)
      val v2 = b.New(id, args, pos, sym)
      val v3 = c.New(id, args, pos, sym)
      val v4 = d.New(id, args, pos, sym)
      val v5 = e.New(id, args, pos, sym)
      val v6 = f.New(id, args, pos, sym)

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


    def Cast(id: Ident with Others, expr: Expr with Others, 
              pos: Position, sym: UseSymbol): Cast with Others = {
      val v1 = a.Cast(id, expr, pos, sym)
      val v2 = b.Cast(id, expr, pos, sym)
      val v3 = c.Cast(id, expr, pos, sym)
      val v4 = d.Cast(id, expr, pos, sym)
      val v5 = e.Cast(id, expr, pos, sym)
      val v6 = f.Cast(id, expr, pos, sym)

      new Cast with Namer with Analyzer with TypeCheck with Eval with Show {
        //Tree
        val id = v1.id
        val expr = v1.expr
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
  object FJLangAlgObject extends FJLangAlg
}
