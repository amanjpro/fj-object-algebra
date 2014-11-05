package ch.usi.inf.l3.ifj.analyzer

import ch.usi.inf.l3._
import elang.ast._
import elang.namer._
import elang.typecheck._
import elang.analyzer._
import fj.ast._
import fj.analyzer._
import fj.namer._
import ifj.ast._
import ifj.namer._
import ifj.typecheck._

trait IFJAnalyzers extends IFJAlg[Analyzer with Tree] with FJAnalyzers {

  def Interface(name: String, parents: List[Analyzer with Tree], 
              ms: List[Analyzer with Tree], p: Position, 
              sym: ClassSymbol): Analyzer with Tree = {
    new Analyzer with Tree {
      type S = ClassSymbol
      val pos: Position = p
      val symbol: ClassSymbol = sym
      val treeName: String = name
      def bind(encl: Symbol): Unit = {
        parents.foreach((x) => x.symbol match {
           case isym: UseSymbol =>
            isym.uses = context.get(x.treeName).getOrElse(NoSymbol)
          case _ => ()

        })
        val itpes = parents.map(_.symbol match {
          case x: UseSymbol => x
          case x => UseSymbol(x)
        })
        
        symbol.tpe match {
          case a: InterfaceType =>
            a.parents = itpes
          case _ => ()
        }
        ms.foreach(_.bind(sym))
      }
    }
  }

  def ClassDef(name: String, parent: Analyzer with Tree, 
              impls: List[Analyzer with Tree], 
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

        impls.foreach((x) => x.symbol match {
           case isym: UseSymbol =>
            isym.uses = context.get(x.treeName).getOrElse(NoSymbol)
          case _ => ()

        })
        symbol.parent = parent.symbol
        val itpes = impls.map(_.symbol match {
          case x: UseSymbol => x
          case x => UseSymbol(x)
        })
        
        symbol.tpe match {
          case a: IClassType =>
            a.parent = parent.symbol match {
              case s: UseSymbol => s
              case s => UseSymbol(s)
            }
            a.impls = itpes
          case _ => ()
        }
        fields.foreach(_.bind(sym))
        const.bind(sym)
        ms.foreach(_.bind(sym))
      }
    }
  }
  override def ClassDef(name: String, parent: Analyzer with Tree, 
    fields: List[Analyzer with Tree], 
    const: Analyzer with Tree, ms: List[Analyzer with Tree], 
    p: Position, sym: ClassSymbol): Analyzer with Tree = {

    this.ClassDef(name, parent, Nil, fields, const, ms, p, sym)
  }


  override def MethodDef(tpe: Analyzer with Tree, name: String, 
        params: List[Analyzer with Tree], 
        body: Analyzer with Tree, p: Position, 
        sym: TermSymbol): Analyzer with Tree = {
    val methodDef = super[FJAnalyzers].MethodDef _
    new Analyzer with Tree {
      type S = TermSymbol
      val pos: Position = p
      val symbol: TermSymbol = sym
      val treeName: String = name
      def bind(encl: Symbol): Unit = {
        val r = methodDef(tpe, name, params, body, p, sym)
        r.bind(encl)
        val ntpe = r.symbol.tpe match {
          case MethodType(t, n, ps) => IMethodType(t, n, ps)
          case x => x
        }
        r.symbol.tpe = ntpe

      }
    }
  }
  def AbstractMethod(tpe: Analyzer with Tree, name: String, 
              params: List[Analyzer with Tree], p: Position, 
              sym: TermSymbol): Analyzer with Tree = {

    val body = new Analyzer with EmptyExpr {
      val treeName: String = Names.NONAME
      def bind(encl: Symbol): Unit = ()
    }
 
    val methodDef = super[FJAnalyzers].MethodDef _
    new Analyzer with Tree {
      type S = TermSymbol
      val pos: Position = p
      val symbol: TermSymbol = sym
      val treeName: String = name
      def bind(encl: Symbol): Unit = {
        val r = methodDef(tpe, name, params, body, p, sym)
        r.bind(encl)
        val ntpe = r.symbol.tpe match {
          case MethodType(t, n, ps) => AbstractMethodType(t, n, ps)
          case x => x
        }
        r.symbol.tpe = ntpe

      }
    }
  }
}


class IFJAnalyzer(val namer: IFJNamer) extends IFJAnalyzers
