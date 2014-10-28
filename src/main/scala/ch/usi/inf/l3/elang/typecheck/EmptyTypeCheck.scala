package ch.usi.inf.l3.elang.typecheck

import ch.usi.inf.l3.elang.ast._
import ch.usi.inf.l3.elang.namer._

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


trait TypeCheck {
  def check(st: SymbolTable): Result
}
