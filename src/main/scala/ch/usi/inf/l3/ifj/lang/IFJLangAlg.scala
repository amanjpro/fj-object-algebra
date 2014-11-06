package ch.usi.inf.l3.ifj.lang



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
import ifj.ast._
import ifj.namer._
import ifj.analyzer._
import ifj.typecheck._
import ifj.eval._
import ifj.debug._


trait IFJCompilerTrait extends FJCompilerTrait {

  trait IFJLangAlg extends GIFJAlg[Tree with Others, 
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
                                   Interface with Others,
                                   IClassDef with Others, 
                                   AbstractMethod with Others] 
                    with FJLangAlg {

    val ia: IFJAlgAST = IFJAlgAST
    val ib: IFJNamer = IFJNamer
    val ic: IFJAnalyzers = new IFJAnalyzer(ib)
    val id: IFJTypers = new IFJTyper(ib)
    val ie: IFJEval = IFJEval
    val iif: IFJAlgDebug = IFJAlgDebug

    def Interface(n: String, ps: List[Ident with Others], 
                  ms: List[AbstractMethod with Others], po: Position,
                  sym: ClassSymbol): Interface with Others = {
      val v1 = ia.Interface(n, ps, ms, po, sym)
      val v2 = ib.Interface(n, ps, ms, po, sym)
      val v3 = ic.Interface(n, ps, ms, po, sym)
      val v4 = id.Interface(n, ps, ms, po, sym)
      val v5 = ie.Interface(n, ps, ms, po, sym)
      val v6 = iif.Interface(n, ps, ms, po, sym)

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
      val v1 = ia.ClassDef(n, p, is, fs, con, ms, po, sym)
      val v2 = ib.ClassDef(n, p, is, fs, con, ms, po, sym)
      val v3 = ic.ClassDef(n, p, is, fs, con, ms, po, sym)
      val v4 = id.ClassDef(n, p, is, fs, con, ms, po, sym)
      val v5 = ie.ClassDef(n, p, is, fs, con, ms, po, sym)
      val v6 = iif.ClassDef(n, p, is, fs, con, ms, po, sym)

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
      val v1 = ia.MethodDef(tpe, name, params, body, pos, symbol)
      val v2 = ib.MethodDef(tpe, name, params, body, pos, symbol)
      val v3 = ic.MethodDef(tpe, name, params, body, pos, symbol)
      val v4 = id.MethodDef(tpe, name, params, body, pos, symbol)
      val v5 = ie.MethodDef(tpe, name, params, body, pos, symbol)
      val v6 = iif.MethodDef(tpe, name, params, body, pos, symbol)

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
      val v1 = ia.AbstractMethod(tpe, name, params, pos, symbol)
      val v2 = ib.AbstractMethod(tpe, name, params, pos, symbol)
      val v3 = ic.AbstractMethod(tpe, name, params, pos, symbol)
      val v4 = id.AbstractMethod(tpe, name, params, pos, symbol)
      val v5 = ie.AbstractMethod(tpe, name, params, pos, symbol)
      val v6 = iif.AbstractMethod(tpe, name, params, pos, symbol)

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
      val v1 = ia.New(i, args, pos, sym)
      val v2 = ib.New(i, args, pos, sym)
      val v3 = ic.New(i, args, pos, sym)
      val v4 = id.New(i, args, pos, sym)
      val v5 = ie.New(i, args, pos, sym)
      val v6 = iif.New(i, args, pos, sym)

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

  object IFJLangAlgObject extends IFJLangAlg
}
