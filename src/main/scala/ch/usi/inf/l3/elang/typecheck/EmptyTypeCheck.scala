package ch.usi.inf.l3.elang.typecheck

import ch.usi.inf.l3._
import elang.ast._
import elang.namer._
import fj.namer.Names._

trait Result {
  def &&(other: Result): Result
}
case object Success extends Result {
  def &&(other: Result): Result = other
}
case class Failure(msg: String) extends Result {
  def &&(other: Result): Result = other match {
    case Success => this
    case Failure(m) => Failure(msg + "\n" + m)
  }
}


trait Type {
  val name: String
}
case object NoType extends Type {
  val name: String = "NOTYPE"
}

trait TType extends Type {
  def isSubtypeOf(other: TType): Boolean
  def vars: List[TermSymbol]
  def methods: List[TermSymbol]
  def field(name: String): Option[TermSymbol]
  def method(name: String): Option[TermSymbol]
  def overrideCheck(mtpe: MethodType): Result
}

case object ObjectType extends TType {
  def isSubtypeOf(other: TType): Boolean = 
    other == this
  private lazy val const = 
    TermSymbol(CONSTRUCTOR, // Name
              MethodType(ObjectSymbol, CONSTRUCTOR, Nil), // Type
              ObjectSymbol) // Owner
  val name: String = "Object"
  def vars: List[TermSymbol] = Nil
  def methods: List[TermSymbol] = List(const)
  def field(name: String): Option[TermSymbol] = 
    None
  def method(name: String): Option[TermSymbol] = name match {
    case CONSTRUCTOR =>
      Some(const)
    case _ => None
  }
  def overrideCheck(mtpe: MethodType): Result = Success
}

case class ClassType(name: String, var parent: UseSymbol, 
                     vars: List[TermSymbol], 
                     methods: List[TermSymbol]) extends TType {
  def isSubtypeOf(other: TType): Boolean = {
    val p = parent.tpe 
    if(other == this || other == p || other == ObjectType) true
    else {
      p match {
        case a: TType => a.isSubtypeOf(other) 
        case _ => false
      }
    }
  }

  def field(name: String): Option[TermSymbol] = {
    vars.filter(_.name == name).headOption match {
      case None => parent.tpe match {
        case a: TType =>
          a.method(name)
        case _ => None
      }
      case m => m
    }
  }

  def method(name: String): Option[TermSymbol] = {
    methods.filter(_.name == name).headOption match {
      case None => parent.tpe match {
        case a: TType =>
          a.method(name)
        case _ => None
      }
      case m => m
    }
  }
  def overrideCheck(mtpe: MethodType): Result = {
    parent.tpe match {
      case t: TType =>
        t.method(mtpe.name) match {
          case None => Success
          case Some(msym) =>
            msym.tpe match {
              case (mtpe2 @ MethodType(ret, _, params)) =>
                val fail = Failure(s"${mtpe} cannot override ${mtpe2}")
                (mtpe.params.size == params.size) match {
                  case true => 
                    val pr = 
                      (mtpe.params.zip(params)).foldLeft(Success: Result)(
                        (z, y) => {
                          z && (if(y._1 == y._2) Success else fail)
                        })
                    val rr = if(ret == mtpe.ret) Success else fail
                    pr && rr
                  case false => fail
                }
              case t => Failure(s"${t} is not a proper type")
            }
        }
      case _ => Failure(s"${parent} is not a proper type")
    }
  }
}

case class VarType(name: String, tpe: Symbol) extends Type
case class MethodType(ret: Symbol, name: String, 
        params: List[Symbol]) extends Type {
  def param(name: String): Option[Symbol] = {
    params.filter(_.name == name).headOption 
  }
}

case class TypeContext private (private var context: Map[String, Symbol]){

  private var duplicates: List[Symbol] = Nil
  def this() {
    this(Map("Object" -> ObjectSymbol))
  }

  def size: Int = context.size
  def put(nme: String, sym: Symbol): Unit = {
    context.get(nme) match {
      case None => ()
      case _ =>
        duplicates = sym :: duplicates
    }
    context += (nme -> sym)
  }
  def get(nme: String): Option[Symbol] = context.get(nme)
  def defines(nme: String): Boolean = context.contains(nme)
  def healthy: Result = {
    duplicates match {
      case Nil => Success
      case l => 
        l.foldLeft(Success: Result)((z, y) => {
          z && Failure(s"Class ${y.name} is already defined")
        })
    }
  }

}




trait TypeCheck {
  def check: Result
}
