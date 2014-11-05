package ch.usi.inf.l3.ifj.ast

import ch.usi.inf.l3._
import elang.ast._
import fj.ast._
import elang.namer._
import fj.namer._


trait IClassDef extends ClassDef {
  val impls: List[Ident]
}

trait NoIdent extends Ident {
  val name = Names.NONAME
  val symbol = UseSymbol(NoSymbol)
  val pos = NoPosition
}
object NoIdent extends NoIdent

trait Interface extends ClassDef {
  val const = NoConst
  val parent = NoIdent
  val parents: List[Ident]
  val methods: List[AbstractMethod]
  val fields = Nil
}

trait AbstractMethod extends MethodDef {
  val body = EmptyExpr
}
object AbstractMethod {

}

trait EmptyExpr extends Expr {
  type S = NoSymbol
  val symbol = NoSymbol
  val pos = NoPosition
}


trait NoConst extends ConstDef { 
  def t = ???
  val params = Nil
  def su: Super = ???
  val finit: List[FieldInit] = Nil
  val symbol = null
  val pos = NoPosition
}

object NoConst extends NoConst

object EmptyExpr extends EmptyExpr


trait IMethodDef extends MethodDef 

trait GIFJAlg[T, E <: T, P <: T, C <: T,
             VD <: T, CD <: T, MD <: T,
             FI <: T, S <: T,
             I <: E, Tz <: E, SE <: E, A <: E,
             N <: E, CA <: E, IN <: C,
             CL <: C, AM <: MD] 
                      extends GFJAlg[T, E, P, C, VD, CD, MD, FI,
                                      S, I, Tz, SE, A, N, CA] {


  def Interface(name: String, parents: List[I], ms: List[AM], pos: Position,
                symbol: ClassSymbol): IN

  def ClassDef(name: String, parent: I, impls: List[I], fields: List[VD],
            const: CD, ms: List[MD], pos: Position, symbol: ClassSymbol): CL


  def AbstractMethod(tpe: I, name: String, params: List[VD], pos: Position,
        symbol: TermSymbol): AM

}

trait IFJAlg[E] extends GIFJAlg[E, E, E, E, E, E, E, E,
                              E, E, E, E, E, E, E, E, 
                              E, E] {
  // Re-writing the defs just for convenience


  def Interface(name: String, parents: List[E], ms: List[E], pos: Position,
                symbol: ClassSymbol): E

  def ClassDef(name: String, parent: E, impls: List[E], fields: List[E],
           const: E, ms: List[E], pos: Position, symbol: ClassSymbol): E


  def AbstractMethod(tpe: E, name: String, params: List[E], pos: Position,
        symbol: TermSymbol): E

}

trait IFJAlgAST extends GIFJAlg[Tree, Expr, Program, ClassDef,
                             ValDef, ConstDef, MethodDef,
                             FieldInit, Super, Ident, This,
                             Select, Apply, New, Cast, Interface,
                             IClassDef, AbstractMethod] 
                with FJAlgAST {

  def Interface(n: String, ps: List[Ident], ms: List[AbstractMethod], 
                p: Position, sym: ClassSymbol): Interface = {
    new {
      val name = n
      val parents = ps
      val methods = ms
      val pos = p
      val symbol = sym
    } with Interface
  }


  def ClassDef(n: String, p: Ident, ps: List[Ident], fs: List[ValDef],
            c: ConstDef, ms: List[MethodDef],
            po: Position, s: ClassSymbol): IClassDef = {
    new {
      val name = n
      val parent = p
      val impls = ps
      val fields = fs
      val const = c
      val methods = ms
      val pos = po
      val symbol = s
    } with IClassDef
  }

  override def ClassDef(n: String, p: Ident, fs: List[ValDef],
            c: ConstDef, ms: List[MethodDef],
            po: Position, s: ClassSymbol): ClassDef = {
    this.ClassDef(n, p, Nil, fs, c, ms, po, s)
  }

  def AbstractMethod(tpe: Ident, n: String, 
        ps: List[ValDef], p: Position, 
        s: TermSymbol): AbstractMethod = {
    new {
      val t = tpe
      val name = n
      val params = ps
      val pos = p
      val symbol = s
    } with AbstractMethod
  }

  override def MethodDef(tpe: Ident, n: String, 
        ps: List[ValDef], b: Expr, 
        p: Position, s: TermSymbol): MethodDef = {
    new {
      val t = tpe
      val name = n
      val params = ps
      val body = b
      val pos = p
      val symbol = s
    } with IMethodDef
  }
}

object IFJAlgAST extends IFJAlgAST

