package ch.usi.inf.l3.elang.debug

trait Show {
  def show: String
  def loc: String
  def debug: String = loc + "\n" + show
}

