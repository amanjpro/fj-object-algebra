package ch.usi.inf.l3.fj.typecheck


import ch.usi.inf.l3._
import elang.typecheck._
import elang.namer._
import elang.ast._
import fj.ast._
import fj.namer._

trait FJTypers extends FJAlg[TypeCheck with Tree] {
  val namer: FJNamer
  lazy val context: TypeContext = namer.context

  import Names._

  def Program(classes: List[TypeCheck with Tree], 
        main: TypeCheck with Tree): TypeCheck with Tree = {
    new TypeCheck with Tree {
      type S = Symbol
      def check: Result = {
        context.healthy && checkList(classes) && main.check
      }
      val symbol: Symbol = NoSymbol
      val pos: Position = NoPosition
    }
  }


  def ClassDef(name: String, parent: TypeCheck with Tree, 
        fields: List[TypeCheck with Tree], 
        const: TypeCheck with Tree, ms: List[TypeCheck with Tree], 
        po: Position, tsymbol: ClassSymbol): TypeCheck with Tree = {
    new TypeCheck with Tree {
      type S = ClassSymbol
      def check: Result = {
        val fr = checkList(fields)
        val cr = const.check
        val mr = checkList(ms)

        val duplicatedFields = 
          fields.groupBy(_.symbol.tpe.name).map(_._2).filter(_.size > 2)
        val fdr = duplicatedFields match {
          case Nil => Success
          case lss => 
            val ls = lss.map(_.head)
            ls.foldLeft(Success: Result)((z, y) => {
              z && Failure(s"Field ${y} is already defined")
            })
        }
        val duplicatedMethods = 
          fields.groupBy(_.symbol.tpe.name).map(_._2).filter(_.size > 2)

        val mdr = duplicatedMethods match {
          case Nil => Success
          case lss => 
            val ls = lss.map(_.head)
            ls.foldLeft(Success: Result)((z, y) => {
              z && Failure(s"Method ${y} is already defined")
            })
        }

        fr && cr && mr && fdr && mdr
      }
      val symbol: ClassSymbol = tsymbol
      val pos: Position = po
    }
  }

  def ConstDef(t: TypeCheck with Tree, params: List[TypeCheck with Tree], 
        su: TypeCheck with Tree, finit: List[TypeCheck with Tree], 
        po: Position, tsymbol: TermSymbol): TypeCheck with Tree = {
    new TypeCheck with Tree {
      type S = TermSymbol
      def check: Result = {
        val sr = su.check
        val pr = checkList(params)

        val tr = (t.symbol.tpe == tsymbol.owner.tpe) match {
          case true => Success
          case false => Failure("Constructors should have the same type as "+
                                s"the class that includes it\n${pos}")
        }
        val fr = t.symbol.tpe match {
          case (t: TType) => Success
            t.vars.map(_.name).diff(finit.map(_.symbol.name)) match {
              case Nil => Success
              case l => 
                l.foldLeft(Success: Result)((z, y) => {
                z && Failure(s"${y} is not initialized\n${po}")
              })
            }
          case _ => 
            Failure(s"${t.symbol.tpe} is not a type\n${po}")
        }

        sr && pr && fr && tr
      }
      val symbol: TermSymbol = tsymbol
      val pos: Position = po
    }
  }

  def FieldInit(name: TypeCheck with Tree, 
        rhs: TypeCheck with Tree, po: Position, 
        sym: UseSymbol): TypeCheck with Tree = {
    new TypeCheck with Tree {
      def check: Result = (symbol.tpe, rhs.symbol.tpe) match {
        case (t1: TType, t2: TType) if t2.isSubtypeOf(t1) => Success
        case (t1, t2) => Failure(s"cannot assign ${t2} to ${t2}\n${po}")
      }
      val symbol: UseSymbol = sym
      val pos: Position = po
      type S = UseSymbol
    }
  }

  def Super(exprs: List[TypeCheck with Tree], po: Position, 
        tsymbol: UseSymbol): TypeCheck with Tree  = {
    new TypeCheck with Tree {
      type S = UseSymbol
      def check: Result = symbol.tpe match {
        case (t: TType) => t.method(CONSTRUCTOR) match {
          case Some(const) =>
            const.tpe match {
              case (mtpe: MethodType) => applyCheck(exprs, mtpe, po)
              case _ => Failure(s"Bad constructor definition\n${po}")
            }
          case _ => Failure(s"Bad constructor call\n${po}")
        }
        case _ => 
          Failure(s"Class not found ${tsymbol}\n${po}")
      }
      val symbol: UseSymbol = tsymbol match {
        case s: UseSymbol => s
        case s => UseSymbol(s)
      }
      val pos: Position = po
    }

  }
  def MethodDef(ret: TypeCheck with Tree, name: String, 
        params: List[TypeCheck with Tree], body: TypeCheck with Tree, 
        po: Position, tsymbol: TermSymbol): TypeCheck with Tree  = {
    new TypeCheck with Tree {
      type S = TermSymbol
      def check: Result = {
        val rr = (body.symbol.tpe, ret.symbol.tpe) match {
          case (t1: TType, t2: TType) if t1.isSubtypeOf(t2) =>
            Success
          case (t1, t2) => 
            Failure(s"${t1} is not compatible with ${t2}\n${po}")
        }
        val or = tsymbol.enclosingClass match {
          case Some(t) => 
            (t.tpe, symbol.tpe) match {
              case (ct : ClassType, mt: MethodType) => 
                ct.overrideCheck(mt)
              case _ => 
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

  def ValDef(id: TypeCheck with Tree, name: String, 
        po: Position, tsymbol: TermSymbol): TypeCheck with Tree = {
    new TypeCheck with Tree {
      type S = TermSymbol
      def check: Result = id.symbol.tpe match {
        case _ : TType => Success
        case _ => 
          Failure(typeNotFound(id.symbol.tpe, po))
      }
      val symbol: TermSymbol = tsymbol
      val pos: Position = po
    }
  }


  def This(po: Position, tsymbol: UseSymbol): TypeCheck with Tree = {
    new TypeCheck with Tree {
      def check: Result = Success
      val symbol: UseSymbol = tsymbol
      val pos: Position = po
      type S = UseSymbol
    }
  }

  def Ident(name: String, po: Position, 
        tsymbol: UseSymbol): TypeCheck with Tree  = {
    new TypeCheck with Tree {
      def check: Result = {
        symbol.tpe match {
          case NoType => Failure(typeNotFound(symbol.tpe, po))
          case _ => Success
        }
      }
      val symbol: UseSymbol = tsymbol
      val pos: Position = po
      type S = UseSymbol
    }
  }

  def Select(s: TypeCheck with Tree, 
        m: String, po: Position, sym: UseSymbol): TypeCheck with Tree = {
    new TypeCheck with Tree {
      def check: Result = {
        val er = s.check
        symbol.tpe match {
          case NoType => er && Failure(memberNotFound(m, s.symbol.tpe, po)) 
          case _ => er && Success
        }
      }
      type S = UseSymbol
      val symbol: UseSymbol = sym
      val pos: Position = po
    }
  }
  
  def Apply(expr: TypeCheck with Tree, m: String, 
        args: List[TypeCheck with Tree], 
        po: Position, sym: UseSymbol): TypeCheck with Tree  = {
    new TypeCheck with Tree {
      type S = UseSymbol
      def check: Result = {
        val er = expr.check
        val mr = symbol.tpe match {
          case NoType => Failure(memberNotFound(m, expr.symbol.tpe, po)) 
          case _ => Success
        }
        val ar = symbol.tpe match {
          case mt: MethodType => applyCheck(args, mt, po)
          case _ => 
            Failure(s"Bad method type ${symbol.tpe}\n${po}")
        }
        er && mr && ar
      }
      val symbol: UseSymbol = sym
      val pos: Position = po
    }
  }

  def New(id: TypeCheck with Tree, 
        args: List[TypeCheck with Tree], po: Position,
        sym: UseSymbol): TypeCheck with Tree = {
    new TypeCheck with Tree {
      type S = UseSymbol
      def check: Result = {
        val ir = id.check
        val ar = id.symbol.tpe match {
          case tt: TType => 
            tt.method(CONSTRUCTOR) match {
              case Some(ms: TermSymbol) => 
                ms.tpe match {
                  case (mt: MethodType) =>
                    applyCheck(args, mt, po)
                  case _ => 
                    Failure(s"Bad constructor type ${symbol.tpe}\n${po}")
                }
              case _ => Failure(s"Bad constructor name ${symbol.tpe}\n${po}")
            }
          case _ => Failure(typeNotFound(id.symbol.tpe, pos))
        }
        ir && ar
      }
      val symbol: UseSymbol = sym
      val pos: Position = po
    }
  }

  def Cast(id: TypeCheck with Tree, 
        expr: TypeCheck with Tree, po: Position,
        sym: UseSymbol): TypeCheck with Tree  = {
    new TypeCheck with Tree {
      type S = UseSymbol
      def check: Result = {
        val actual = expr.symbol.tpe
        val cast = symbol.tpe
        (actual, cast) match {
          case (a: TType, b: TType) if a.isSubtypeOf(b) =>
            Success
          case (a: TType, b: TType) if b.isSubtypeOf(a) =>
            Success
          case (a, b) =>
            Failure(s"Cannot cast from ${a} to ${b}\n${po}")
        }
      }
      val symbol: UseSymbol = sym
      val pos: Position = po
    }
  }


  /***************************************************************************
   * Helper functions                                                        *
   ***************************************************************************/
  private def typeNotFound(tpe: Type, po: Position): String = {
    s"Type ${tpe} is not defined\n${po}"
  }

  private def memberNotFound(m: String, tpe: Type, po: Position): String = {
    s"${m} is not a member of ${tpe}\n${po}"
  }

  def checkList(l: List[TypeCheck with Tree]): Result = {
    l.foldLeft(Success: Result)((z, y) => z && y.check)
  }

  private def applyCheck(args: List[TypeCheck with Tree],
            mtpe: MethodType, po: Position): Result = {
    val atpe = args.map(_.symbol.tpe)
    val ptpe = mtpe.params.map(_.tpe)
    val zipped = atpe.zip(ptpe)
    atpe.size == ptpe.size match {
      case true =>
        zipped.foldLeft(Success: Result)((z, y) => {
          (y._1, y._2) match {
            case (a: TType, VarType(_, UseSymbol(osym)))  =>
              osym.tpe match {
                case b: TType if a.isSubtypeOf(b) => 
                  z && Success
                case b =>
                  z && Failure(s"${a} is not compatible with ${b}\n${po}")
              }
            case (a, b) => 
              z && Failure(s"${a} and/or ${b} are not types\n${po}")
          }
        })
      case false =>
        Failure(s"""|Incorrect number of arguments is provided, 
                    |required: ${ptpe.size}, found ${atpe.size}
                    |method call: ${mtpe.name}${args}, does not 
                    |conform to the method ${mtpe}. 
                    |${po}""".stripMargin)
    }
  }
}

class FJTyper(val namer: FJNamer) extends FJTypers
