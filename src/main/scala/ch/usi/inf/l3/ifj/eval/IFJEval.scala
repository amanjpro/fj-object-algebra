package ch.usi.inf.l3.ifj.eval

import ch.usi.inf.l3._
import elang.ast._
import elang.namer._
import elang.eval._
import elang.typecheck._
import fj.ast._
import fj.namer._
import fj.eval._
import ifj.ast._

trait IFJEval extends IFJAlg[Eval with Tree] with FJEval {


  def Interface(name: String, parents: List[Eval with Tree], 
              ms: List[Eval with Tree], po: Position, 
              sym: ClassSymbol): Eval with Tree = {
    new Eval with Tree {
      def eval(env: Store): (Value, Store) = (NoValue, env)
      def index: Unit = ()
      val symbol: ClassSymbol = sym
      type S = ClassSymbol
      val pos: Position = po
    }
  }

  def ClassDef(name: String, parent: Eval with Tree, 
              impls: List[Eval with Tree], fields: List[Eval with Tree], 
              const: Eval with Tree, ms: List[Eval with Tree], 
              pos: Position, symbol: ClassSymbol): Eval with Tree = {
    super[FJEval].ClassDef(name, parent, fields, const, ms, pos, symbol)
  }


  def AbstractMethod(tpe: Eval with Tree, name: String, 
              params: List[Eval with Tree], po: Position, 
              sym: TermSymbol): Eval with Tree = {
    new Eval with Tree {
      def eval(env: Store): (Value, Store) = (NoValue, env)
      def index: Unit = ()
      val symbol: TermSymbol = sym
      val pos: Position = po
      type S = TermSymbol
    }
  }

}


object IFJEval extends IFJEval
