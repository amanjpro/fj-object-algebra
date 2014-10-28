package ch.usi.inf.l3.fj.ast

import ch.usi.inf.l3.elang.ast._


case class Program(classes: List[ClassDef], main: Expr) extends Tree {
  val pos: Position = NoPosition
}

case class ClassDef(name: String, parent: Ident, fields: List[ValDef], 
      const: ConstDef, methods: List[MethodDef], 
      pos: Position) extends Tree


case class ConstDef(tpe: Ident, params: List[ValDef], 
  su: Super, finit: List[FieldInit], pos: Position) extends Tree

case class FieldInit(name: String, rhs: Expr, pos: Position) extends Tree
case class Super(exprs: List[Expr], pos: Position) extends Tree

case class MethodDef(tpe: Ident, name: String, 
      params: List[ValDef], body: Expr, pos: Position) extends Tree
case class ValDef(tpe: Ident, name: String, pos: Position) extends Tree


trait Expr extends Tree
case class Ident(name: String, pos: Position) extends Expr
case class Select(s: Expr, m: String, pos: Position) extends Expr
case class Apply(expr: Expr, m: String, args: List[Expr], 
      pos: Position) extends Expr
case class New(id: Ident, args: List[Expr], pos: Position) extends Expr
case class Cast(id: Ident, expr: Expr, pos: Position) extends Expr
// case class Literal(v: Constant, pos: Position) extends Expr


// trait Constant {
//   def getInt: Int = ???
//   def getBool: Boolean = ???
//   def getFloat: Boolean = ???
//   def getStr: String = ???
// }
// case object NullConst extends Constant
// case class StrConst(v: String) extends Constant {
//   override getString: String = v
// }
// case class IntConst(v: Int) extends Constant {
//   override getInt: Int = v
// }
// case class BoolConst(v: Boolean) extends Constant {
//   override getBool: Boolean = v
// }
// case class DoubleConst(v: Double) extends Constant {
//   override getDouble: Double = v
// }
//
trait GFJAlg[T, E <: T, P <: T, C <: T,
             VD <: T, CD <: T, MD <: T,
             FI <: T, S <: T,
             I <: E, SE <: E, A <: E,
             N <: E, CA <: E] extends EmptyAlg {
  def Program(classes: List[C], main: E): P

  def ClassDef(name: String, parent: I, fields: List[VD], 
            const: CD, ms: List[MD], pos: Position): C

  def ConstDef(tpe: I, params: List[VD], 
    su: S, finit: List[FI], pos: Position): CD

  def FieldInit(name: String, rhs: E, pos: Position): FI

  def Super(exprs: List[E], pos: Position): S 

  def MethodDef(tpe: I, name: String, 
        params: List[VD], body: E, pos: Position): MD 

  def ValDef(tpe: I, name: String, pos: Position): VD


  def Ident(name: String, pos: Position): I 

  def Select(s: E, m: String, pos: Position): SE
  
  def Apply(expr: E, m: String, args: List[E], 
        pos: Position): A 

  def New(id: I, args: List[E], pos: Position): N

  def Cast(id: I, expr: E, pos: Position): CA 

  // def Literal(v: Constant, pos: Position): E 
}

trait FJAlg[E] extends GFJAlg[E, E, E, E, E, E, E, 
                              E, E, E, E, E, E, E] {
  // Re-writing the defs just for convenience
  def Program(classes: List[E], main: E): E

  def ClassDef(name: String, parent: E, fields: List[E], 
            const: E, ms: List[E], pos: Position): E

  def ConstDef(tpe: E, params: List[E], 
    su: E, finit: List[E], pos: Position): E

  def FieldInit(name: String, rhs: E, pos: Position): E

  def Super(exprs: List[E], pos: Position): E 

  def MethodDef(tpe: E, name: String, 
        params: List[E], body: E, pos: Position): E 

  def ValDef(tpe: E, name: String, pos: Position): E


  def Ident(name: String, pos: Position): E 

  def Select(s: E, m: String, pos: Position): E
  
  def Apply(expr: E, m: String, args: List[E], 
        pos: Position): E 

  def New(id: E, args: List[E], pos: Position): E

  def Cast(id: E, expr: E, pos: Position): E 
}

trait FJAlgAST extends GFJAlg[Tree, Expr, Program, ClassDef,
                             ValDef, ConstDef, MethodDef,
                             FieldInit, Super, Ident,
                             Select, Apply, New, Cast] {
  def Program(classes: List[ClassDef], main: Expr): Program = {
    Program(classes, main)
  }

  def ClassDef(name: String, parent: Ident, fields: List[ValDef],
            const: ConstDef, methods: List[MethodDef],
            pos: Position): ClassDef = {
    ClassDef(name, parent, fields, const, methods, pos)
  }

  def ConstDef(tpe: Ident, params: List[ValDef], 
    su: Super, finit: List[FieldInit], pos: Position): ConstDef = {
    ConstDef(tpe, params, su, finit, pos)
  }

  def FieldInit(name: String, rhs: Expr, pos: Position): FieldInit = {
    FieldInit(name, rhs, pos)
  }

  def Super(exprs: List[Expr], pos: Position): Super = {
    Super(exprs, pos)
  } 

  def MethodDef(tpe: Ident, name: String, 
        params: List[ValDef], body: Expr, pos: Position): MethodDef = {
    MethodDef(tpe, name, params, body, pos)
  }

  def ValDef(tpe: Ident, name: String, pos: Position): ValDef = {
    ValDef(tpe, name, pos)
  }


  def Ident(name: String, pos: Position): Ident = {
    Ident(name, pos)
  }
  def Select(s: Expr, m: String, pos: Position): Select = {
    Select(s, m, pos)
  }
  def Apply(expr: Expr, m: String, args: List[Expr], 
        pos: Position): Apply = {
    Apply(expr, m, args, pos)
  }

  def New(id: Ident, args: List[Expr], pos: Position): New = {
    New(id, args, pos)
  }

  def Cast(id: Ident, expr: Expr, pos: Position): Cast = {
    Cast(id, expr, pos)
  }

  // def Literal(v: Constant, pos: Position): Literal = {
  //   Literal(v, pos)
  // }
  //

}

object FJAlgAST extends FJAlgAST
