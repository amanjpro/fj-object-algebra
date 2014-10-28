package ch.usi.inf.l3.elang.ast


trait Position
object Position {
  def apply(fname: String, line: Int, col: Int): Pos = {
    new Pos(fname, line, col)
  }
  def unapply(p: Pos): Some [(String, Int, Int)] = {
    Some((p.fname, p.line, p.col))
  }
}
object NoPosition extends Position {
  override def toString: String = "NoPosition"
}
class Pos(val fname: String, val line: Int, val col: Int) 
  extends Position with Equals {
  
  override def canEqual(other: Any): Boolean = other.isInstanceOf[Pos]
  override def equals(other: Any): Boolean = {
    other match {
      case c: Pos => c.canEqual(this) && super.equals(c) && 
                  fname == c.fname &&
                  line == c.line
                  col == c.line
      case _ => false
    } 
  }

  override def hashCode: Int = {
    val p = 43
    val r1 = p + fname.hashCode
    val r2 = p * r1 + line.hashCode
    p * r2 + col.hashCode
  }

  override def toString: String = 
    s"File: ${fname}, line: ${line}, col: ${col}"
}


trait Tree {
  val pos: Position
}

trait EmptyAlg

