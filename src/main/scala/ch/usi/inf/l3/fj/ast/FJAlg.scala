package ch.usi.inf.l3.fj.ast

import ch.usi.inf.l3._
import elang.ast._
import elang.namer._

trait Program extends Tree {
  type S = Symbol
  val classes: List[ClassDef]
  val main: Expr

  // override def canEqual(other: Any): Boolean = other.isInstanceOf[Program]
  //
  // override def equals(other: Any): Boolean = {
  //   other match {
  //     case c: Program => c.canEqual(this) && super.equals(c) && 
  //                 fname == c.fname &&
  //                 line == c.line
  //                 col == c.line
  //     case _ => false
  //   } 
  // }
  //
  // override def hashCode: Int = {
  //   val p = 43
  //   val r1 = p + fname.hashCode
  //   val r2 = p * r1 + line.hashCode
  //   p * r2 + col.hashCode
  // }
}

trait ClassDef extends Tree {
  type S = ClassSymbol
  val name: String
  val parent: Ident
  val fields: List[ValDef]
  val const: ConstDef
  val methods: List[MethodDef]
}


trait ConstDef extends Tree {
  type S = TermSymbol
  val t: Ident
  val params: List[ValDef]
  val su: Super
  val finit: List[FieldInit]
}

trait FieldInit extends Tree {
  type S = UseSymbol
  val name: Ident
  val rhs: Expr
}

trait Super extends Tree {
  type S = UseSymbol
  val exprs: List[Expr]
}


trait MethodDef extends Tree {
  type S = TermSymbol
  val t: Ident
  val name: String
  val params: List[ValDef]
  val body: Expr
}

trait ValDef extends Tree {
  type S = TermSymbol
  val t: Ident
  val name: String
  override def toString = s"${t} ${name}"
}


trait Expr extends Tree {
  type S = UseSymbol
}

trait This extends Expr {
  override def toString = "this"
}

trait Ident extends Expr {
  val name: String
  override def toString = s"${name}"
}

trait Select extends Expr {
  val s: Expr
  val m: String
  override def toString = s"${s}.${m}"
}

trait Apply extends Expr {
  val expr: Expr
  val m: String
  val args: List[Expr] 
  override def toString = s"${expr}.${m}${args}"
}

trait New extends Expr {
  val id: Ident
  val args: List[Expr]
  override def toString = s"new ${id}${args}"
}

trait Cast extends Expr {
  val id: Ident
  val expr: Expr
  override def toString = s"(${id}) ${expr}"
}

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
             I <: E, Tz <: E, SE <: E, A <: E,
             N <: E, CA <: E] extends EmptyAlg {


  def Program(classes: List[C], main: E): P

  def ClassDef(name: String, parent: I, fields: List[VD], 
            const: CD, ms: List[MD], pos: Position, symbol: ClassSymbol): C

  def ConstDef(tpe: I, params: List[VD], 
    su: S, finit: List[FI], pos: Position, symbol: TermSymbol): CD

  def FieldInit(name: I, rhs: E, pos: Position, symbol: UseSymbol): FI

  def Super(exprs: List[E], pos: Position, symbol: UseSymbol): S 

  def MethodDef(tpe: I, name: String, 
        params: List[VD], body: E, pos: Position, symbol: TermSymbol): MD 

  def ValDef(tpe: I, name: String, pos: Position, symbol: TermSymbol): VD


  def Ident(name: String, pos: Position, symbol: UseSymbol): I 

  def This(pos: Position, symbol: UseSymbol): Tz

  def Select(s: E, m: String, pos: Position, symbol: UseSymbol): SE
  
  def Apply(expr: E, m: String, args: List[E], 
        pos: Position, symbol: UseSymbol): A 

  def New(id: I, args: List[E], pos: Position, symbol: UseSymbol): N

  def Cast(id: I, expr: E, pos: Position, symbol: UseSymbol): CA 

  // def Literal(v: Constant, pos: Position): E 
}

trait FJAlg[E] extends GFJAlg[E, E, E, E, E, E, E, E,
                              E, E, E, E, E, E, E] {
  // Re-writing the defs just for convenience
  def Program(classes: List[E], main: E): E

  def ClassDef(name: String, parent: E, fields: List[E], 
            const: E, ms: List[E], pos: Position, symbol: ClassSymbol): E

  def ConstDef(tpe: E, params: List[E], 
    su: E, finit: List[E], pos: Position, symbol: TermSymbol): E

  def FieldInit(name: E, rhs: E, pos: Position, symbol: UseSymbol): E

  def Super(exprs: List[E], pos: Position, symbol: UseSymbol): E 

  def MethodDef(tpe: E, name: String, 
        params: List[E], body: E, pos: Position, symbol: TermSymbol): E 

  def ValDef(tpe: E, name: String, pos: Position, symbol: TermSymbol): E


  def This(pos: Position, symbol: UseSymbol): E

  def Ident(name: String, pos: Position, symbol: UseSymbol): E 

  def Select(s: E, m: String, pos: Position, symbol: UseSymbol): E
  
  def Apply(expr: E, m: String, args: List[E], 
        pos: Position, symbol: UseSymbol): E 

  def New(id: E, args: List[E], pos: Position, symbol: UseSymbol): E

  def Cast(id: E, expr: E, pos: Position, symbol: UseSymbol): E 
}

trait FJAlgAST extends GFJAlg[Tree, Expr, Program, ClassDef,
                             ValDef, ConstDef, MethodDef,
                             FieldInit, Super, Ident, This,
                             Select, Apply, New, Cast] {
  def Program(cs: List[ClassDef], m: Expr): Program = {
    new {
      val classes = cs
      val main = m
      val symbol: Symbol = NoSymbol
      val pos = NoPosition
    } with Program
  }

  def ClassDef(n: String, p: Ident, fs: List[ValDef],
            c: ConstDef, ms: List[MethodDef],
            po: Position, s: ClassSymbol): ClassDef = {
    new {
      val name = n
      val parent = p
      val fields = fs
      val const = c
      val methods = ms
      val pos = po
      val symbol = s
    } with ClassDef
  }

  def ConstDef(tpe: Ident, ps: List[ValDef], 
    s: Super, f: List[FieldInit], p: Position, 
    sym: TermSymbol): ConstDef = {
    new {
      val t = tpe
      val params = ps
      val su = s
      val finit = f
      val pos = p
      val symbol = sym
    } with ConstDef
  }

  def FieldInit(n: Ident, r: Expr, p: Position, s: UseSymbol): FieldInit = {
    new {
      val name = n
      val rhs = r
      val pos = p
      val symbol = s
    } with FieldInit
  }

  def Super(e: List[Expr], p: Position, s: UseSymbol): Super = {
    new {
      val exprs = e
      val pos = p
      val symbol = s
    } with Super
  } 

  def MethodDef(tpe: Ident, n: String, 
        ps: List[ValDef], b: Expr, 
        p: Position, s: TermSymbol): MethodDef = {
    new {
      val t = tpe
      val name = n
      val params = ps
      val body = b
      val pos = p
      val symbol = s
    } with MethodDef
  }

  def ValDef(tpe: Ident, n: String, p: Position, 
        s: TermSymbol): ValDef = {
    new {
      val t = tpe
      val name = n
      val pos = p
      val symbol = s
    } with ValDef 
  }

  def This(p: Position, s: UseSymbol): This = {
    new {
      val pos = p
      val symbol: UseSymbol = s
    } with This 
  }

  def Ident(n: String, p: Position, s: UseSymbol): Ident = {
    new {
      val name = n
      val pos = p
      val symbol: UseSymbol = s
    } with Ident 
  }
  def Select(sel: Expr, mem: String, p: Position, sym: UseSymbol): Select = {
    new {
      val s = sel
      val m = mem
      val symbol: UseSymbol = sym
      val pos = p
    } with Select 
  }
  def Apply(e: Expr, mem: String, as: List[Expr], 
        p: Position, s: UseSymbol): Apply = {
    new {
      val expr = e
      val m = mem
      val args = as
      val symbol: UseSymbol = s
      val pos = p
    } with Apply 
  }

  def New(i: Ident, as: List[Expr], p: Position, s: UseSymbol): New = {
    new {
      val id = i
      val args = as
      val pos = p
      val symbol: UseSymbol = s
    } with New 
  }

  def Cast(i: Ident, e: Expr, p: Position, s: UseSymbol): Cast = {
    new {
      val id = i
      val expr = e
      val pos = p
      val symbol: UseSymbol = s
    } with Cast 
  }

  // def Literal(v: Constant, pos: Position): Literal = {
  //   Literal(v, pos)
  // }
  //

}

object FJAlgAST extends FJAlgAST

