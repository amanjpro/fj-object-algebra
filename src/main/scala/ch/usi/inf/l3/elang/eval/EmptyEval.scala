package ch.usi.inf.l3.elang.eval

import ch.usi.inf.l3._
import elang.namer._
import elang.typecheck._



trait Value
case object NoValue extends Value
case class ObjectValue(s: Symbol, env: Store) extends Value {
  override def toString: String = {
    val tpe = s.tpe match {
      case MethodType(UseSymbol(tpe), _, _) =>
        tpe.name
      case _ => ""
    }
    s"new ${tpe}.${s.name}${env.toString}"
  }
}



class Store private(env: Map[Symbol, Value],
                          outer: Option[Store]) {
  def this() {
    this(Map.empty, None)
  }

  def put(s: Symbol, v: Value): Store = {
    new Store(env + (s -> v), outer)
  }


  def put(svs: List[(Symbol, Value)]): Store = {
    new Store(env ++ svs.toMap, outer)
  }

  def lookup(s: Symbol): Value = {
    env.get(s) match {
      case Some(v) => v
      case _ => outer match {
        case Some(st) => st.lookup(s)
        case _ => NoValue
      }
    }
  }

  def enter: Store = new Store(Map.empty, Some(this))

  def exit: Option[Store] = outer


  override def toString: String = (env.map((x) => 
          s"${x._1} -> ${x._2}").mkString("(", ", ", ")"))
}



trait Eval {
  def eval(env: Store): (Value, Store)
  def index: Unit
}
