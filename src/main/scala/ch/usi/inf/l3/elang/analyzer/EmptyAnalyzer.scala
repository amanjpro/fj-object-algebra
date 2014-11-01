package ch.usi.inf.l3.elang.analyzer


import ch.usi.inf.l3._
import elang.namer._

trait Analyzer {
  def bind(encl: Symbol): Unit
  val treeName: String
}
