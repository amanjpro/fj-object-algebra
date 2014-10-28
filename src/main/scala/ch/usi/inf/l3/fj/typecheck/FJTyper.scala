package ch.usi.inf.l3.fj.typecheck


import ch.usi.inf.l3._
import fj.ast._
import elang.namer._
import elang.typecheck._
import elang.ast._

trait FJTyper extends FJAlg[Namer with TypeCheck] {

  val noname = "NO_NAME"
  val constructor = "<init>"

  private def applyCheck(args: List[Namer with TypeCheck],
            mtpe: MethodType, st: SymbolTable, pos: Position): Result = {
    val atpe = args.map(_.treeType(st))
    val ptpe = 
      mtpe.params.map((x) => st.get(x.tpe).getOrElse(ErrorType))
    val zipped = atpe.zip(ptpe)
    zipped.foldLeft(Success: Result)((z, y) => {
      (y._1, y._2) match {
        case (a: TType, b: TType) if a.isSubtypeOf(b)(st) => 
          z && Success
        case (a, b) => 
          val r = 
            Failure(s"${a} is not " + 
              s"compatible with ${b}\n${pos}")
          z && r
      }
    })
  }

  def Program(classes: List[Namer with TypeCheck], 
            main: Namer with TypeCheck): Namer with TypeCheck = {
    new Namer with TypeCheck {
      def name(st: SymbolTable): (SymbolTable, Type) = {
        val nst = classes.foldLeft(st)((z, y) => {
          y.name(z)._1
        })
        (nst, treeType(st))
      }
      val treeName: String = noname
      def check(st: SymbolTable): Result = Success
      def treeType(st: SymbolTable): Type = NoType
    }
  }

  def ClassDef(nme: String, parent: Namer with TypeCheck, 
        fields: List[Namer with TypeCheck], 
        const: Namer with TypeCheck, 
        ms: List[Namer with TypeCheck], 
        pos: Position): Namer with TypeCheck = {
    new Namer with TypeCheck {
      def name(st: SymbolTable): (SymbolTable, Type) = {
        (st, treeType(st))
      }
      val treeName: String = constructor
      def check(st: SymbolTable): Result = Success
      def treeType(st: SymbolTable): Type = {
       val constTpe = const.name(st) match {
          case (_, tpe: MethodType) => List(tpe)
          case _ => Nil
        }
        val mtpes = constTpe ++ ms.foldLeft(Nil: List[MethodType])((z, y) => {
            y.name(st) match {
              case (_, tpe: MethodType) => z ++ List(tpe)
              case _ => z
            }
        })
        val ftpes = fields.foldLeft(Nil: List[VarType])((z, y) => {
            y.name(st) match {
              case (_, tpe: VarType) => z ++ List(tpe)
              case _ => z
            }
        }) 
        ClassType(nme, parent.treeName, ftpes, mtpes)
      }
    }
  }

  def ConstDef(tpe: Namer with TypeCheck, 
    params: List[Namer with TypeCheck], 
    su: Namer with TypeCheck, finit: List[Namer with TypeCheck], 
    pos: Position): Namer with TypeCheck = {
    new Namer with TypeCheck {
      def name(st: SymbolTable): (SymbolTable, Type) = {
        
        (st, treeType(st))
      }
      val treeName: String = constructor
      def check(st: SymbolTable): Result = Success
      def treeType(st: SymbolTable): Type = {
        val ptpes = params.foldLeft(Nil: List[VarType])((z, y) => {
          y.name(st) match {
            case (_, tpe: VarType) => z ++ List(tpe)
            case _ => z
          }
        })
        MethodType(tpe.treeName, constructor, ptpes)
      }
    }
  }

  def FieldInit(name: String, rhs: Namer with TypeCheck, 
    pos: Position): Namer with TypeCheck = {
    new Namer with TypeCheck {
      def name(st: SymbolTable): (SymbolTable, Type) = {
        (st, treeType(st))
      }
      val treeName: String = noname
      def check(st: SymbolTable): Result = Success
      def treeType(st: SymbolTable): Type = rhs.treeType(st)
    }
  }

  def Super(exprs: List[Namer with TypeCheck], 
        pos: Position): Namer with TypeCheck = {
   new Namer with TypeCheck {
      def name(st: SymbolTable): (SymbolTable, Type) = {
        (st, treeType(st))
      }
      val treeName: String = noname
      def check(st: SymbolTable): Result = Success
      def treeType(st: SymbolTable): Type = NoType
    } 
  }

  def MethodDef(tpe: Namer with TypeCheck, nme: String, 
      params: List[Namer with TypeCheck], 
      body: Namer with TypeCheck, pos: Position): Namer with TypeCheck = {
    new Namer with TypeCheck {
      def name(st: SymbolTable): (SymbolTable, Type) = {
        (st, treeType(st))
      }
      val treeName: String = nme
      def check(st: SymbolTable): Result = {
        Success
      }
      def treeType(st: SymbolTable): Type = {
       val ptpes = params.foldLeft(Nil: List[VarType])((z, y) => {
          y.name(st) match {
            case (_, tpe: VarType) => z ++ List(tpe)
            case _ => z
          }
        })
        MethodType(tpe.treeName, nme, ptpes)
     }
    }
  }

  def ValDef(tpe: Namer with TypeCheck, 
    nme: String, pos: Position): Namer with TypeCheck = {
    new Namer with TypeCheck {
      def name(st: SymbolTable): (SymbolTable, Type) = {
        (st, treeType(st))
      }
      val treeName: String = nme
      def check(st: SymbolTable): Result = st.defines(tpe.treeName) match {
        case false => Failure(s"Class ${tpe} is not defined\n${pos}")
        case _ => Success
      }
      def treeType(st: SymbolTable): Type = VarType(nme, tpe.treeName)
    }
  }


  def Ident(nme: String, pos: Position): Namer with TypeCheck = {
    new Namer with TypeCheck {
      def name(st: SymbolTable): (SymbolTable, Type) = {
        (st, treeType(st))
      }
      val treeName: String = nme
      def check(st: SymbolTable): Result = Success
      def treeType(st: SymbolTable): Type = NoType
    }
  }

  def Select(s: Namer with TypeCheck, 
    m: String, pos: Position): Namer with TypeCheck = {
   new Namer with TypeCheck {
      def name(st: SymbolTable): (SymbolTable, Type) = {
        (st, treeType(st))
      }
      val treeName: String = noname
      def check(st: SymbolTable): Result = {
        s.treeType(st) match {
          case ct: ClassType => 
            ct.field(m)(st) match {
              case Some(_) => Success
              case _ => Failure(s"Field ${m} is not a member of ${s}\n${pos}")
            }
          case _ => Failure(s"Class ${s.treeName} is not defined\n${pos}")
        }
      }
      def treeType(st: SymbolTable): Type = {
        s.treeType(st) match {
          case ct: ClassType => ct.field(m)(st).getOrElse(ErrorType)
          case _ => ErrorType
        }
      }
    }
  }
  
  def Apply(e: Namer with TypeCheck, 
    m: String, args: List[Namer with TypeCheck], 
        pos: Position): Namer with TypeCheck = {
    new Namer with TypeCheck {
      def name(st: SymbolTable): (SymbolTable, Type) = {
        (st, treeType(st))
      }
      val treeName: String = noname
      def check(st: SymbolTable): Result = {
       e.treeType(st) match {
          case ct: ClassType => 
            ct.method(m)(st) match {
              case Some(mtpe) =>
                applyCheck(args, mtpe, st, pos) 
              case None => 
                Failure(s"Method ${m} is not a member of ${e.treeName}\n${pos}")
            }
          case _ => Failure(s"Class ${e.treeName} is not defined\n${pos}")
        } 
      }
      def treeType(st: SymbolTable): Type = {
        e.treeType(st) match {
          case ct: ClassType => 
            ct.method(m)(st) match {
              case Some(mtpe: MethodType) => 
                st.get(mtpe.ret).getOrElse(ErrorType)
              case _ => ErrorType
            }
          case _ => ErrorType
        }
      }
    }
  }

  def New(id: Namer with TypeCheck, 
    args: List[Namer with TypeCheck], 
    pos: Position): Namer with TypeCheck = {
    new Namer with TypeCheck {
      def name(st: SymbolTable): (SymbolTable, Type) = {
        (st, treeType(st))
      }
      val treeName: String = noname
      def check(st: SymbolTable): Result = {
        treeType(st) match {
          case ct: ClassType => 
            ct.method(constructor)(st) match {
              case Some(mtpe) =>
                applyCheck(args, mtpe, st, pos) 
              case None => 
                Failure(s"Constructor is not defined for ${id.treeName}" + 
                  s"\n${pos}")
            }
          case _ => Failure(s"Class ${id.treeName} is not defined\n${pos}")
        }
      }
      def treeType(st: SymbolTable): Type = id.treeType(st)
    }
  }

  def Cast(id: Namer with TypeCheck, 
    expr: Namer with TypeCheck, 
    pos: Position): Namer with TypeCheck = {
    new Namer with TypeCheck {
      def name(st: SymbolTable): (SymbolTable, Type) = {
        (st, treeType(st))
      }
      val treeName: String = noname
      def check(st: SymbolTable): Result = {
        val actual = expr.treeType(st)
        val cast = treeType(st)
        (actual, cast) match {
          case (a: TType, b: TType) if a.isSubtypeOf(b)(st) =>
            Success
          case (a: TType, b: TType) if b.isSubtypeOf(a)(st) =>
            Success
          case (a, b) =>
            Failure(s"Cannot cast from ${a} to ${b}\n${pos}")
        }
      }
      def treeType(st: SymbolTable): Type = id.treeType(st)
    }
  }
}

object FJTyper extends FJTyper
