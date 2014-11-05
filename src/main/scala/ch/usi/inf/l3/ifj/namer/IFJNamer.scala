package ch.usi.inf.l3.ifj.namer

import ch.usi.inf.l3._
import elang.ast._
import fj.ast._
import elang.namer._
import fj.namer._
import elang.typecheck._
import ifj.ast._
import ifj.typecheck._

trait IFJNamer extends IFJAlg[Namer with Tree] with FJNamer {

  def Interface(nme: String, parents: List[Namer with Tree], 
              ms: List[Namer with Tree], po: Position, 
              sym: ClassSymbol): Namer with Tree = {
    new Namer with Tree {
      type S = ClassSymbol
      def nameIt(owner: Symbol): Unit = {
        val fmeths = ms.map(_.symbol match {
          case s: TermSymbol => s
          case s => TermSymbol(Names.BADMETH, NoType, NoSymbol)
        })
        val isym = parents.map((x) => x.symbol match {
          case s: UseSymbol => s
          case s => UseSymbol(s)
        })

        val itpe = InterfaceType(nme, isym, fmeths)

        sym.tpe = itpe
        sym.name = nme
        sym.owner = NoSymbol
        
        context.put(nme, symbol)

        ms.foreach(_.nameIt(sym))
      }
      val symbol: ClassSymbol = sym
      val pos: Position = po
    }
  }

  def ClassDef(nme: String, parent: Namer with Tree, 
              impls: List[Namer with Tree], fields: List[Namer with Tree], 
              const: Namer with Tree, ms: List[Namer with Tree], 
              po: Position, sym: ClassSymbol): Namer with Tree = {
    val isym = impls.map((x) => x.symbol match {
      case s: UseSymbol => s
      case s => UseSymbol(s)
    })
    val classDef = super[FJNamer].ClassDef _
    new Namer with Tree {
      type S = ClassSymbol
      def nameIt(owner: Symbol): Unit = {
        val r = classDef(nme, parent, fields, const, ms, po, sym)
        r.nameIt(owner)
        val ntpe = r.symbol.tpe match {
          case ClassType(n, p, v, m) => 
            IClassType(n, p, isym, v, m)
          case x => 
            println(r.symbol + "  alkjfd " + nme)
            x
        }
        r.symbol.tpe = ntpe
      }
      val symbol: ClassSymbol = sym
      val pos: Position = po
    }

  }

  override def ClassDef(nme: String, parent: Namer with Tree, 
              fields: List[Namer with Tree], const: Namer with Tree, 
              ms: List[Namer with Tree], po: Position,
              sym: ClassSymbol): Namer with Tree = {
    this.ClassDef(nme, parent, fields, const, ms, po, sym)
  }

  override def MethodDef(tpe: Namer with Tree, name: String, 
              params: List[Namer with Tree], body: Namer with Tree, 
              po: Position, sym: TermSymbol): Namer with Tree  = {
    val methodDef = super[FJNamer].MethodDef _
    new Namer with Tree {
      type S = TermSymbol
      def nameIt(owner: Symbol): Unit = {
        val r = methodDef(tpe, name, params, body, po, sym)
        r.nameIt(owner)
        val ntpe = r.symbol.tpe match {
          case MethodType(t, n, p) => IMethodType(t, n, p)
          case x => x
        }
        r.symbol.tpe = ntpe
      }
      val symbol: TermSymbol = sym
      val pos: Position = po
    }
  }

  def AbstractMethod(tpe: Namer with Tree, name: String, 
              params: List[Namer with Tree], po: Position, 
              sym: TermSymbol): Namer with Tree = {
    val body = new EmptyExpr with Namer {
      def nameIt(owner: Symbol): Unit = ()
    }
    val methodDef = super[FJNamer].MethodDef _
    new Namer with Tree {
      type S = TermSymbol
      def nameIt(owner: Symbol): Unit = {
        val r = methodDef(tpe, name, params, body, po, sym)
        r.nameIt(owner)
        val ntpe = r.symbol.tpe match {
          case MethodType(t, n, p) => AbstractMethodType(t, n, p)
          case x => x
        }
        symbol.owner = owner
        r.symbol.tpe = ntpe
      }
      val symbol: TermSymbol = sym
      val pos: Position = po
    }
  }

}


object IFJNamer extends IFJNamer {
  val context: TypeContext = new TypeContext
}
