package ch.usi.inf.l3.ifj.typecheck

import ch.usi.inf.l3._
import elang.ast._
import fj.ast._
import elang.namer._
import fj.namer._
import elang.typecheck._
import fj.typecheck._
import ifj.ast._
import ifj.namer._

trait InterfaceType extends ClassType {
  def parent: UseSymbol = UseSymbol(NoSymbol)
  def parent_=(u: UseSymbol): Unit = ??? 

  val vars: List[TermSymbol] = Nil
  var parents: List[UseSymbol]
  val methods: List[TermSymbol]

  override def isSubtypeOf(other: TType): Boolean = {
    val ps = parents.map(_.tpe)
    if(other == this || ps.foldLeft(false)((z, y) => z || y == other)) true
    else {
      ps.foldLeft(false)((z, y) => y match {
          case a: TType => a.isSubtypeOf(other)
          case _ => false
        })
    }
  }

  override def toString: String = "interface " + name

  override def overrideCheck(mtpe: MethodType): Result = Success
  override def hashCode: Int = 
    43 * (name.hashCode + parents.hashCode)
  
  override def equals(other: Any): Boolean = 
    (other != null) && (other match {
      case c: InterfaceType =>
        c.name == name && parents == c.parents
      case _ => false
    })

  override def method(name: String): Option[TermSymbol] = {
    methods.filter(_.name == name).headOption match {
      case None => 
        parents.foldLeft(None: Option[TermSymbol])((z, y) => {
          (z, y.tpe) match {
            case (None, a: TType) => a.method(name)
            case _ => z
          }
        })
      case m => m
    }
  }

  def allMethods: List[TermSymbol] = {
    parents.foldLeft(methods)((z, y) => {
      y.tpe match {
        case a: InterfaceType => z ++ a.allMethods
        case _ => z
      }
    })
  }
}

object InterfaceType {
  def apply(n: String, ps: List[UseSymbol], 
            ms: List[TermSymbol]): InterfaceType = {
    new InterfaceType {
      val name = n
      var parents = ps
      val methods = ms
    }
  }
  def unapply(ct: InterfaceType): Option[(String, List[UseSymbol], 
                                      List[TermSymbol])] = {
    Some(ct.name, ct.parents, ct.methods)
  }
}
trait IClassType extends ClassType {
  var impls: List[UseSymbol]
  override def isSubtypeOf(other: TType): Boolean = {
    val ps = parent.tpe :: impls.map(_.tpe)
    if(other == this || other == ObjectType ||
      ps.foldLeft(false)((z, y) => z || y == other)) true
    else {
      ps.foldLeft(false)((z, y) => y match {
          case a: TType => a.isSubtypeOf(other)
          case _ => false
        })
    }
  }

  override def hashCode: Int = 
    43 * (name.hashCode + parent.hashCode + impls.hashCode)
  override def equals(other: Any): Boolean = 
    (other != null) && (other match {
      case c: IClassType =>
        c.name == name && c.parent == parent && impls == c.impls
      case _ => false
    })
  def allMethods: List[TermSymbol] = {
    parent.tpe match {
      case t: IClassType => methods ++ t.allMethods
      case _ => methods
    }
  }
  def checkImplementation: Result = {
    val methodsToImplement = impls.foldLeft(Nil: List[TermSymbol])((z, y) => {
      y.tpe match {
        case x: InterfaceType => z ++ x.allMethods
        case _ => z
      }
    })
    val ms = allMethods
    methodsToImplement.foldLeft(Success: Result)((z, y) => {
      (ms.filter(_.name == y.name).map(_.tpe), y.tpe) match {
        case (List(at @ IMethodType(ret1, _, ps1)), 
                AbstractMethodType(ret2, _, ps2)) => 
          val fail = Failure(s"${at} cannot implement ${y}")
          (ps1.size == ps2.size) match {
            case true => 
              val pr = 
                (ps1.zip(ps2)).foldLeft(Success: Result)(
                  (z, y) => {
                    z && (if(y._1 == y._2) Success else fail)
                  })
              val rr = if(ret2 == ret2) Success else fail
              pr && rr
            case false => fail
          }
        case (Nil, _) => Failure(s"Method ${y} is not implemented")
        case r => 
          Failure(s"Method ${y} is implemented more than once")
      }
    })
  }
}

object IClassType {
  def apply(n: String, p: UseSymbol, 
            is: List[UseSymbol], 
            vs: List[TermSymbol], 
            ms: List[TermSymbol]): ClassType = {
    new IClassType {
      var impls = is
      val name = n
      var parent = p
      val vars = vs
      val methods = ms
    }
  }
  def unapply(ct: IClassType): Option[(String, UseSymbol, 
                                      List[UseSymbol], List[TermSymbol],
                                      List[TermSymbol])] = {
    Some(ct.name, ct.parent, ct.impls, ct.vars, ct.methods)
  }
}

trait IMethodType extends MethodType {
  val isAbstract = false
}
object IMethodType {
  def apply(r: Symbol, n: String, ps: List[Symbol]): IMethodType = {
    new IMethodType {
      val ret = r
      val name = n
      val params = ps
    }
  }
  def unapply(mt: IMethodType): Option[(Symbol, String, List[Symbol])] = {
    Some(mt.ret, mt.name, mt.params)
  }
}


trait AbstractMethodType extends MethodType {
  val isAbstract = true
}
object AbstractMethodType {
  def apply(r: Symbol, n: String, ps: List[Symbol]): AbstractMethodType = {
    new AbstractMethodType {
      val ret = r
      val name = n
      val params = ps
    }
  }
  def unapply(mt: AbstractMethodType): Option[(Symbol, 
                  String, List[Symbol])] = {
    Some(mt.ret, mt.name, mt.params)
  }
}



trait IFJTypers extends IFJAlg[TypeCheck with Tree] with FJTypers {

  import Names._

  def Interface(name: String, parents: List[TypeCheck with Tree], 
              ms: List[TypeCheck with Tree], po: Position, 
              tsymbol: ClassSymbol): TypeCheck with Tree = {
    new TypeCheck with Tree {
      type S = ClassSymbol
      def check: Result = {
        val mr = checkList(ms)

        val allMethods = symbol.tpe match {
          case t: InterfaceType => t.allMethods
          case _ => ms.map(_.symbol)
        }
        
        val duplicatedMethods = 
          allMethods.groupBy(_.tpe.name).map(_._2).filter(_.size > 2)

        val mdr = duplicatedMethods match {
          case Nil => Success
          case lss => 
            val ls = lss.map(_.head)
            ls.foldLeft(Success: Result)((z, y) => {
              z && Failure(s"Method ${y} is already defined")
            })
        }

        val amr = ms.foldLeft(Success: Result)((z, y) => {
          y.symbol.tpe match {
            case s: IMethodType => 
              z && Failure(s"Interface ${name} cannot have concrete " + 
                      s"method ${s.name}\n${pos}")
            case _ => z && Success
          }
        })

        
        val ir = parents.foldLeft(Success: Result)((z, y) => {
          y match {
            case s: InterfaceType => z && Success
            case s => 
              z && Failure(s"Only interfaces can be extended by ${symbol}")
          }
        })
        mr && mdr && amr && ir
      }
      val symbol: ClassSymbol = tsymbol
      val pos: Position = po
    }
  }


  def ClassDef(name: String, 
              parent: TypeCheck with Tree, 
              impls: List[TypeCheck with Tree], 
              fields: List[TypeCheck with Tree], 
              const: TypeCheck with Tree, 
              ms: List[TypeCheck with Tree], 
              po: Position, 
              tsymbol: ClassSymbol): TypeCheck with Tree = {
    val cd = super[FJTypers].ClassDef(name, 
                                          parent, 
                                          fields, 
                                          const, 
                                          ms, 
                                          po, 
                                          tsymbol)
    new TypeCheck with Tree {
      type S = ClassSymbol
      def check: Result = {
        val cr = cd.check
        val or = symbol.tpe match {
          case t: IClassType => t.checkImplementation
          case t => 
            Failure(s"Bad class definition ${symbol}\n${pos}")
        }

        val duplicatedInterfaces = 
            impls.groupBy(_.symbol.tpe.name).map(_._2).filter(_.size > 2)
        val idr = duplicatedInterfaces match {
          case Nil => Success
          case lss => 
            val ls = lss.map(_.head)
            ls.foldLeft(Success: Result)((z, y) => {
              z && Failure(s"Interface ${y} is already implemented\n${pos}")
            })
        }

        val amr = ms.foldLeft(Success: Result)((z, y) => {
          y.symbol.tpe match {
            case s: AbstractMethodType => 
              z && Failure(s"Concrete class ${name} cannot have abstract " + 
                      s"method ${s.name}\n${pos}")
            case _ => z && Success
          }
        })

        val pr = parent.symbol.tpe match {
          case _: IClassType | ObjectType => Success
          case s => 
            Failure(s"Only concrete classes are accepted as parents")
        }
        val ir = impls.foldLeft(Success: Result)((z, y) => {
          y.symbol.tpe match {
            case s: InterfaceType => z && Success
            case s => 
              z && Failure(s"Only interfaces can be implemented")
          }
        })


        cr && or && idr && amr && pr && ir

      }
      val symbol: ClassSymbol = tsymbol
      val pos: Position = po
    }
  }

  override def ClassDef(name: String, parent: TypeCheck with Tree, 
              fields: List[TypeCheck with Tree], const: TypeCheck with Tree, 
              ms: List[TypeCheck with Tree], po: Position,
              tsymbol: ClassSymbol): TypeCheck with Tree = {
    ClassDef(name, parent, Nil, fields, const, ms, po, tsymbol)
  }


  def AbstractMethod(ret: TypeCheck with Tree, name: String, 
              params: List[TypeCheck with Tree], po: Position, 
              tsymbol: TermSymbol): TypeCheck with Tree = {
    new TypeCheck with Tree {
      type S = TermSymbol
      def check: Result = {
        val rr = ret.symbol.tpe match {
          case _: TType => Success
          case t => 
            Failure(s"${t} is not found\n${po}")
        }
        val or = tsymbol.enclosingClass match {
          case Some(t) => 
            (t.tpe, symbol.tpe) match {
              case (ct : InterfaceType, mt: AbstractMethodType) => 
                ct.overrideCheck(mt)
              case s => 
                Failure(s"Bad enclosing class ${t}\n${po}")
            }
          case _ => 
            Failure(s"Bad enclosing class for ${tsymbol}\n${po}")
        }
        val pr = checkList(params)
        rr && or && pr
      }
      val symbol: TermSymbol = tsymbol
      val pos: Position = po
    }
  }

  override def New(id: TypeCheck with Tree, 
        args: List[TypeCheck with Tree], po: Position,
        sym: UseSymbol): TypeCheck with Tree = {
    val vn = super[FJTypers].New(id, args, po, sym)
    new TypeCheck with Tree {
      type S = UseSymbol
      def check: Result = {
        val ir = id.symbol.tpe match {
          case a: InterfaceType =>
            Failure(s"You cannot instantiate interfaces ${a}\n${pos}")
          case _ => Success
        }
        ir && vn.check
      }
      val symbol: UseSymbol = sym
      val pos: Position = po
    }
  }

}

class IFJTyper(val namer: IFJNamer) extends IFJTypers

