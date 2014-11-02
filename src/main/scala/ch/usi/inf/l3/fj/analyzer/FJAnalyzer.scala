package ch.usi.inf.l3.fj.analyzer


import ch.usi.inf.l3._
import elang.ast._
import elang.analyzer._
import elang.namer._
import elang.typecheck._
import fj.ast._
import fj.namer._


/* 
  If we instead of Tree, mix in the proper types of trees we end up avoiding
  almost all the pattern matchings on types, but it has a huge overhead, as
  need to re-implements methods we have already implemented.

  A way to get around this is to have a super parent for every AST node, that
  sets the type of Symbol but doesn't add new members for the trait.

  But that way we need to extend GFJAlg instead of FJAlg, which has far more 
  type parameters.

  -- Amanj
*/
/**
  * This trait takes care of symbol binding for USEs.
  * And correctly types all the nodes.
  */
trait FJAnalyzers extends FJAlg[Analyzer with Tree] {
  val namer: FJNamer

  lazy val context: TypeContext = namer.context

  val nodef = UseSymbol(NoSymbol)

  import Names._
  
  def Program(classes: List[Analyzer with Tree], 
    main: Analyzer with Tree): Analyzer with Tree = {
    new Analyzer with Tree {
      type S = Symbol
      val pos: Position = NoPosition
      val symbol: Symbol = NoSymbol
      val treeName: String = NONAME
      def bind(encl: Symbol): Unit = {
        classes.foreach(_.bind(encl))
        main.bind(encl)
      }
    }
  }

  def ClassDef(name: String, parent: Analyzer with Tree, 
    fields: List[Analyzer with Tree], 
    const: Analyzer with Tree, ms: List[Analyzer with Tree], 
    p: Position, sym: ClassSymbol): Analyzer with Tree = {
    new Analyzer with Tree {
      type S = ClassSymbol
      val pos: Position = p
      val symbol: ClassSymbol = sym
      val treeName: String = name
      def bind(encl: Symbol): Unit = {
        parent.symbol match {
          case isym: UseSymbol =>
            isym.uses = context.get(parent.treeName).getOrElse(NoSymbol)
          case _ => ()
        }
        symbol.parent = parent.symbol
        symbol.tpe match {
          case a: ClassType =>
            a.parent = parent.symbol match {
              case s: UseSymbol => s
              case s => UseSymbol(s)
            }
          case _ => ()
        }
        fields.foreach(_.bind(sym))
        const.bind(sym)
        ms.foreach(_.bind(sym))
      }
    }
  }


  def ConstDef(tpe: Analyzer with Tree, params: List[Analyzer with Tree], 
    su: Analyzer with Tree, finit: List[Analyzer with Tree], 
    p: Position, sym: TermSymbol): Analyzer with Tree = {
    new Analyzer with Tree {
      type S = TermSymbol
      val pos: Position = p
      val treeName: String = CONSTRUCTOR
      val symbol: TermSymbol = sym
      def bind(encl: Symbol): Unit = {
        // Set type for the constructor
        params.foreach(_.bind(sym))
        tpe.symbol match {
          case isym: UseSymbol =>
            isym.uses = sym.enclosingClass.getOrElse(NoSymbol)
          case _ => ()
        }
        val mtpe = MethodType(tpe.symbol, treeName ,params.map(_.symbol))
        sym.tpe = mtpe

        // bind the body

        // Set the proper symbol for super call
        su.symbol match {
          case isym: UseSymbol =>
            isym.uses = tpe.symbol match {
              case UseSymbol(a: ClassSymbol) => a.parent
              case _ =>
                NoSymbol
            }
          case _ => ()
        }
        
        su.bind(sym)
        

        // bind symbols for finits
        finit.foreach((x) => {
          tpe.symbol.tpe match {
            case ct: TType =>
              (ct.field(x.treeName), x.symbol) match {
                case (Some(s), xt: UseSymbol) => xt.uses = s
                case _ => ()
              }
            case _ => ()
          }
        })

        finit.foreach(_.bind(sym))
      }
    }
  }


  def FieldInit(name: Analyzer with Tree, rhs: Analyzer with Tree, 
    p: Position, sym: UseSymbol): Analyzer with Tree = {
    new Analyzer with Tree {
      type S = UseSymbol
      val pos: Position = p
      val treeName: String = name.treeName
      val symbol: UseSymbol = sym
      def bind(encl: Symbol): Unit = {
        name.bind(encl)
        rhs.bind(encl)
      }
    }
  }


  def Super(exprs: List[Analyzer with Tree], p: Position, 
      sym: UseSymbol): Analyzer with Tree = {
    new Analyzer with Tree {
      type S = UseSymbol
      val pos: Position = p
      val treeName: String = SUPER
      val symbol: UseSymbol = sym
      def bind(encl: Symbol): Unit = {
        exprs.foreach(_.bind(encl))
      }
    }
  }


  def MethodDef(tpe: Analyzer with Tree, name: String, 
        params: List[Analyzer with Tree], 
        body: Analyzer with Tree, p: Position, 
        sym: TermSymbol): Analyzer with Tree = {
    new Analyzer with Tree {
      type S = TermSymbol
      val pos: Position = p
      val symbol: TermSymbol = sym
      val treeName: String = name
      def bind(encl: Symbol): Unit = {
        // Set type for the method
        params.foreach(_.bind(sym))
        (context.get(tpe.treeName), tpe.symbol) match {
          case (Some(s), xt: UseSymbol) => xt.uses = s
          case _ => ()
        }
        val mtpe = MethodType(tpe.symbol, treeName ,params.map(_.symbol))
        symbol.tpe = mtpe

        body.bind(sym)
      }
    }
  }
 

  def ValDef(tpe: Analyzer with Tree, name: String, 
    p: Position, sym: TermSymbol): Analyzer with Tree = {
    new Analyzer with Tree {
      type S = TermSymbol
      val pos: Position = p
      val symbol: TermSymbol = sym
      val treeName: String = name
      def bind(encl: Symbol): Unit = {
        (context.get(tpe.treeName), tpe.symbol) match {
          case (Some(s), xt: UseSymbol) => xt.uses = s
          case _ => ()
        }
        symbol.tpe = VarType(treeName, tpe.symbol)
      }
    }
  }



  def This(p: Position, sym: UseSymbol): Analyzer with Tree = {
    new Analyzer with Tree {
      type S = UseSymbol
      val pos: Position = p
      val symbol: UseSymbol = sym
      val treeName: String = THIS
      def bind(encl: Symbol): Unit = {
        encl match {
          case cs: ClassSymbol =>
            sym.uses = cs
          case _ =>
            encl.enclosingClass match {
              case Some(cs: ClassSymbol) => sym.uses = cs
              case _ => ()
            }
        }
      }
    }
  }


  def Ident(name: String, p: Position, 
    sym: UseSymbol): Analyzer with Tree = {
    new Analyzer with Tree {
      type S = UseSymbol
      val pos: Position = p
      val symbol: UseSymbol = sym
      val treeName: String = name
      def bind(encl: Symbol): Unit = {
        encl.tpe match {
          case t: MethodType => 
            t.param(name) match {
              case Some(ps) => symbol.uses = ps
              case _ => bind(encl.owner)
            }
          case t: TType =>
            t.field(name) match {
              case Some(fs) => symbol.uses = fs
              case _ => ()
            }
          case _ => 
            symbol.uses = context.get(name).getOrElse(NoSymbol)
        }
      }
    }
  }


  def Select(s: Analyzer with Tree, m: String, 
    p: Position, sym: UseSymbol): Analyzer with Tree = {
    new Analyzer with Tree {
      type S = UseSymbol
      val pos: Position = p
      val symbol: UseSymbol = sym 
      val treeName: String = NONAME
      def bind(encl: Symbol): Unit = {
        s.bind(encl)
        s.symbol.tpe match {
          case t: TType =>
            t.field(m) match {
              case Some(fs) =>
                symbol.uses = fs
              case _ => 
                ()
            }
          case _ => 
            ()
        }
      }
    }
  }

  
  def Apply(expr: Analyzer with Tree, m: String, 
    args: List[Analyzer with Tree], 
        p: Position, sym: UseSymbol): Analyzer with Tree = {
    new Analyzer with Tree {
      type S = UseSymbol
      val pos: Position = p
      val symbol: UseSymbol = sym
      val treeName: String = NONAME
      def bind(encl: Symbol): Unit = {
        expr.bind(encl)
        expr.symbol.tpe match {
          case t: TType =>
            t.method(m) match {
              case Some(ms) =>
                symbol.uses = ms
              case _ => ()
            }
          case _ => ()
        }
        args.foreach(_.bind(encl))
      }
    }
  }


  def New(id: Analyzer with Tree, args: List[Analyzer with Tree], 
    p: Position, sym: UseSymbol): Analyzer with Tree = {
    new Analyzer with Tree {
      type S = UseSymbol
      val pos: Position = p
      
      val symbol: UseSymbol = sym
      val treeName: String = NONAME
      def bind(encl: Symbol): Unit = {
        context.get(id.treeName) match {
          case Some(s) => 
            id.symbol match {
              case x: UseSymbol =>
                symbol.uses = s
                x.uses = s
              case _ => 
                ()
            }
          case _ => 
            ()
        }
        args.foreach(_.bind(encl))
      }
    }
  }


  def Cast(id: Analyzer with Tree, expr: Analyzer with Tree, 
    p: Position, sym: UseSymbol): Analyzer with Tree = {
    new Analyzer with Tree {
      type S = UseSymbol
      val pos: Position = p
      val symbol = sym
      val treeName: String = NONAME
      def bind(encl: Symbol): Unit = {
        expr.bind(encl)
        (context.get(id.treeName), id.symbol) match {
          case (Some(s), x: UseSymbol) =>
            x.uses = s
            symbol.uses = s
          case _ => ()
        }
      }
    }
  }
}

class FJAnalyzer(val namer: FJNamer) extends FJAnalyzers
