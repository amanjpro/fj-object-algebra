package ch.usi.inf.l3.fj.namer


import ch.usi.inf.l3._
import fj.ast._
import elang.namer._
import elang.typecheck._
import elang.ast._

object Names {
  val NONAME = "NO_NAME"
  val CONSTRUCTOR = "<init>"
  val SUPER = "super"
  val THIS = "this"
  val BADMETH = "BAD_METHOD"
  val BADFIELD = "BAD_FIELD"
}
/**
  * This trait takes care of the symbols of all DEFs, but
  * doesn't address USEs.
  */
trait FJNamer extends FJAlg[Namer with Tree] {

  val context: TypeContext

  def Program(classes: List[Namer with Tree], 
            main: Namer with Tree): Namer with Tree = {
    new Namer with Tree {
      type S = Symbol
      def nameIt(owner: Symbol): Unit = classes.foreach(_.nameIt(NoSymbol))
      val symbol: Symbol = NoSymbol
      val pos: Position = NoPosition
    }
  }

  def ClassDef(nme: String, parent: Namer with Tree, 
        fields: List[Namer with Tree], 
        const: Namer with Tree, 
        ms: List[Namer with Tree], 
        po: Position,
        sym: ClassSymbol): Namer with Tree = {
    new Namer with Tree {
      type S = ClassSymbol
      def nameIt(owner: Symbol): Unit = {
        val fsyms = fields.map(_.symbol match {
          case s: TermSymbol => s
          case s => TermSymbol(Names.BADFIELD, NoType, NoSymbol)
        })
        val fmeths = (const :: ms).map(_.symbol match {
          case s: TermSymbol => s
          case s => TermSymbol(Names.BADMETH, NoType, NoSymbol)
        })
        val psym = parent.symbol match {
          case s: UseSymbol => s
          case s => UseSymbol(s)
        }
        val ctpe = ClassType(nme, psym, fsyms, fmeths)

        sym.tpe = ctpe
        sym.name = nme
        sym.owner = NoSymbol
        
        context.put(nme, symbol)

        fields.foreach(_.nameIt(sym))
        const.nameIt(sym)
        ms.foreach(_.nameIt(sym))

      }
      val symbol: ClassSymbol = sym
      val pos: Position = po
    }
  }

  def ConstDef(tpe: Namer with Tree, 
    params: List[Namer with Tree], 
    su: Namer with Tree, finit: List[Namer with Tree], 
    po: Position, sym: TermSymbol): Namer with Tree = {
    new Namer with Tree {
      type S = TermSymbol
      def nameIt(owner: Symbol): Unit = {
        // We cannot really set tpe for this symbol yet,
        // we need to finish DEFs first then set tpe
        sym.owner = owner
        sym.name = Names.CONSTRUCTOR

        params.foreach(_.nameIt(sym))
      }
      val symbol: TermSymbol = sym
      val pos: Position = po
    }
  }

  def FieldInit(nme: Namer with Tree, rhs: Namer with Tree, 
    po: Position, sym: UseSymbol): Namer with Tree = {
    new Namer with Tree {
      type S = UseSymbol
      def nameIt(owner: Symbol): Unit = ()
      val symbol: UseSymbol = sym
      val pos: Position = po
    }
  }

  def Super(exprs: List[Namer with Tree], 
        po: Position, sym: UseSymbol): Namer with Tree = {
    new Namer with Tree {
      type S = UseSymbol
      def nameIt(owner: Symbol): Unit = ()
      val symbol: UseSymbol = sym
      val pos: Position = po
    }
  }

  def MethodDef(tpe: Namer with Tree, nme: String, 
      params: List[Namer with Tree], 
      body: Namer with Tree, 
      po: Position, sym: TermSymbol): Namer with Tree = {
    new Namer with Tree {
      type S = TermSymbol
      def nameIt(owner: Symbol): Unit = {
        // We cannot really set tpe for this symbol yet,
        // we need to finish DEFs first then set tpe
        sym.owner = owner
        sym.name = nme

        params.foreach(_.nameIt(sym))
      }
      val symbol: TermSymbol = sym
      val pos: Position = po
    }
  }

  def ValDef(tpe: Namer with Tree, 
    nme: String, po: Position, sym: TermSymbol): Namer with Tree = {
    new Namer with Tree {
      type S = TermSymbol
      def nameIt(owner: Symbol): Unit = {
        // We cannot really set tpe for this symbol yet,
        // we need to finish DEFs first then set tpe
        symbol.name = nme
        symbol.owner = owner
      }
      val symbol: TermSymbol = sym
      val pos: Position = po
    }
  }


  def Ident(nme: String, po: Position, 
        sym: UseSymbol): Namer with Tree = {
    new Namer with Tree {
      type S = UseSymbol
      def nameIt(owner: Symbol): Unit = ()
      val symbol: UseSymbol = sym
      val pos: Position = po
    }
  }


  def This(po: Position, sym: UseSymbol): Namer with Tree = {
    new Namer with Tree {
      type S = UseSymbol
      def nameIt(owner: Symbol): Unit = {
        sym.owner = owner match {
          case cs: ClassSymbol => cs
          case s => s.enclosingClass.getOrElse(NoSymbol)
        }
        sym.tpe = sym.owner.tpe
        sym.name = "this"
      }
      val symbol: UseSymbol = sym
      val pos: Position = po
    }
  }


  def Select(s: Namer with Tree, 
    m: String, po: Position, sym: UseSymbol): Namer with Tree = {
    new Namer with Tree {
      type S = UseSymbol
      def nameIt(owner: Symbol): Unit = ()
      val symbol: UseSymbol = sym
      val pos: Position = po
    }
  }

  
  def Apply(e: Namer with Tree, 
    m: String, args: List[Namer with Tree], 
        po: Position, sym: UseSymbol): Namer with Tree = {
    new Namer with Tree {
      type S = UseSymbol
      def nameIt(owner: Symbol): Unit = ()
      val symbol: UseSymbol = sym
      val pos: Position = po
    }
  }

  def New(id: Namer with Tree, 
    args: List[Namer with Tree], 
    po: Position, sym: UseSymbol): Namer with Tree = {
    new Namer with Tree {
      type S = UseSymbol
      def nameIt(owner: Symbol): Unit = ()
      val symbol: UseSymbol = sym
      val pos: Position = po
    }
  }


  def Cast(id: Namer with Tree, 
    expr: Namer with Tree, 
    po: Position, sym: UseSymbol): Namer with Tree = {
    new Namer with Tree {
      type S = UseSymbol
      def nameIt(owner: Symbol): Unit = ()
      val symbol: UseSymbol = sym
      val pos: Position = po
    }
  }
}

object FJNamer extends FJNamer {
  val context: TypeContext = new TypeContext
}
