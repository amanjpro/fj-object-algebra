package ch.usi.inf.l3.fj.eval


import ch.usi.inf.l3._
import elang.ast._
import elang.namer._
import elang.typecheck._
import elang.eval._
import fj.ast._
import fj.namer._
import scala.collection.mutable.{Map => MMap}

trait FJEval extends FJAlg[Eval with Tree] {

  import Names._

  val nodef = UseSymbol(NoSymbol)
  var bank: MMap[ClassSymbol, 
                 MMap[TermSymbol, Eval with Tree]] = MMap.empty

  private def evalList(l: List[Eval with Tree], 
          env: Store): (List[Value], Store) = {
    l.foldLeft((Nil: List[Value], env))((z, y) => {
      val (v, e) = y.eval(z._2)
      (z._1 ++ List(v), e)
    })
  }


  def Program(classes: List[Eval with Tree], 
          main: Eval with Tree): Eval with Tree = {
    new Eval with Tree {
      def eval(env: Store): (Value, Store) = {
        index
        main.eval(env)
      }
      def index: Unit = classes.foreach((x) => x.index)
      val symbol: Symbol = NoSymbol
      val pos: Position = NoPosition
      type S = Symbol
    }
  }

  def ClassDef(name: String, parent: Eval with Tree, 
          fields: List[Eval with Tree], 
          const: Eval with Tree, ms: List[Eval with Tree], 
          po: Position, sym: ClassSymbol): Eval with Tree = {
    new Eval with Tree {
      def eval(env: Store): (Value, Store) = (NoValue, env)
      def index: Unit = {
        bank += (sym -> MMap.empty)
        const.index
        ms.foreach((x) => x.index)
      }
      val symbol: ClassSymbol = sym
      type S = ClassSymbol
      val pos: Position = po
    }
  }


  def ConstDef(tpe: Eval with Tree, params: List[Eval with Tree], 
          su: Eval with Tree, finit: List[Eval with Tree], 
          po: Position, sym: TermSymbol): Eval with Tree = {
    new Eval with Tree {
      def eval(env: Store): (Value, Store) = {
        su.eval(env)
        val (r, env2) = evalList(finit, env)
        (ObjectValue(sym, env2), env2)
      }
      def index: Unit = sym.owner match {
        case o: ClassSymbol =>
          bank(o) += (sym -> this)
        case _ => ()
      }
      val symbol: TermSymbol = sym
      val pos: Position = po
      type S = TermSymbol
    }
  }


  def FieldInit(name: Eval with Tree, 
          rhs: Eval with Tree, po: Position,
          sym: UseSymbol): Eval with Tree = {
    new Eval with Tree {
      def eval(env: Store): (Value, Store) = {
        rhs.eval(env)
      }
      val symbol: UseSymbol = sym
      def index: Unit = ()
      type S = UseSymbol
      val pos: Position = po
    }
  }


  def Super(exprs: List[Eval with Tree], 
          po: Position, sym: UseSymbol): Eval with Tree  = {
    new Eval with Tree {
      def eval(env: Store): (Value, Store) = {
        val (_, env2) = evalList(exprs, env)
        (NoValue, env2)
      }
      val symbol: UseSymbol = sym
      type S = UseSymbol
      def index: Unit = ()
      val pos: Position = po
    }
  }


  def MethodDef(tpe: Eval with Tree, name: String, 
            params: List[Eval with Tree], body: Eval with Tree, 
            po: Position, sym: TermSymbol): Eval with Tree  = {
    new Eval with Tree {
      def eval(env: Store): (Value, Store) = {
        body.eval(env)
      }
      def index: Unit = sym.owner match {
        case o: ClassSymbol => bank(o) += (sym -> this)
        case _ => ()
      }
      val symbol: TermSymbol = sym
      val pos: Position = po
      type S = TermSymbol
    }
  }


  def ValDef(tpe: Eval with Tree, name: String, 
          po: Position, sym: TermSymbol): Eval with Tree = {
    new Eval with Tree {
      def eval(env: Store): (Value, Store) = (NoValue, env)
      type S = TermSymbol
      val symbol: TermSymbol = sym
      val pos: Position = po
      def index: Unit = ()
    }
  }



  def This(po: Position, sym: UseSymbol): Eval with Tree = {
    new Eval with Tree {
      def eval(env: Store): (Value, Store) = (NoValue, env)
      type S = UseSymbol
      val symbol: UseSymbol = sym
      def index: Unit = ()
      val pos: Position = po
    }
  }


  def Ident(name: String, po: Position, sym: UseSymbol): Eval with Tree  = {
    new Eval with Tree {
      def eval(env: Store): (Value, Store) = {
        (env.lookup(symbol), env)
      }
      val symbol: UseSymbol = sym
      type S = UseSymbol
      def index: Unit = ()
      val pos: Position = po
    }
  }


  def Select(s: Eval with Tree, m: String, 
            po: Position, sym: UseSymbol): Eval with Tree = {
    new Eval with Tree {
      def eval(env: Store): (Value, Store) = {
        val (_, senv) = s.eval(env)
        (senv.lookup(symbol), env)
      }
      val pos: Position = po
      def index: Unit = ()
      type S = UseSymbol
      val symbol: UseSymbol = sym
    }
  }

  
  def Apply(expr: Eval with Tree, m: String, args: List[Eval with Tree], 
            po: Position,
            sym: UseSymbol): Eval with Tree  = {
    new Eval with Tree {
      def eval(env: Store): (Value, Store) = {
        val (_, senv) = expr.eval(env)
        val senv2 = senv.enter
        val (vargs, _) = evalList(args, senv2)
        val vpzip = symbol.tpe match {
          case t: MethodType => 
            t.params.zip(vargs)
          case _ => 
            Nil
        }
        symbol.owner match {
          case o: ClassSymbol => 
            val ceval = bank(o)
            symbol.uses match {
              case ms : TermSymbol => 
                val meval = ceval(ms)
                val senv3 = senv2.put(vpzip)
                val (v, _) = meval.eval(senv3)
                (v, senv2)
              case _ => 
                (NoValue, env)
            }
          case _ => 
            (NoValue, env)
        }
      }
      val pos: Position = po
      val symbol: UseSymbol = sym
      def index: Unit = ()
      type S = UseSymbol
    }
  }


  def New(id: Eval with Tree, 
            args: List[Eval with Tree], po: Position,
            sym: UseSymbol): Eval with Tree = {
    new Eval with Tree {
      def eval(env: Store): (Value, Store) = {
        (symbol.uses.tpe, symbol.uses) match {
          case (ts: TType, cs: ClassSymbol) =>
            val ceval = bank(cs)
            val mso = ts.method(Names.CONSTRUCTOR)
            mso match {
              case Some(ms) => 
                val env2 = new Store()
                val (vargs, _) = evalList(args, env)
                val vpzip = ms.tpe match {
                  case t: MethodType => 
                    t.params.zip(vargs)
                  case _ => Nil
                }
                val meval = ceval(ms)
                val env3 = env2.put(vpzip)
                val (v, env4) = meval.eval(env3)
                (v, env4)
              case _ => 
                (NoValue, env)
            }
          case _ => 
            (NoValue, env)
        }
      }
      val symbol: UseSymbol = sym
      def index: Unit = ()
      val pos: Position = po
      type S = UseSymbol
    }
  }


  def Cast(id: Eval with Tree, expr: Eval with Tree, 
            po: Position, sym: UseSymbol): Eval with Tree  = {
    new Eval with Tree {
      def eval(env: Store): (Value, Store) = expr.eval(env)
      val symbol: UseSymbol = sym
      def index: Unit = ()
      val pos: Position = po
      type S = UseSymbol
    }
  }

}


object FJEval extends FJEval
