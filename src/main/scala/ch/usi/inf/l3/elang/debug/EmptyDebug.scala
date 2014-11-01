package ch.usi.inf.l3.elang.debug

trait Show {
  def show(col: Int = 0): String
  def loc(col: Int = 0): String
  def debug(col: Int = 0): String = loc(col) + "\n" + show(col)
}

