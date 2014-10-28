package ch.usi.inf.l3.fj.debug

import ch.usi.inf.l3.elang.ast._
import ch.usi.inf.l3.elang.debug._
import ch.usi.inf.l3.fj.ast._

trait FJAlgDebug extends FJAlg[Show] {
  private def showList(l: List[Show]): String = {
    l.foldLeft("")((z, y) => {
        z ++ y.show
      })
  }
  private def locList(l: List[Show]): String = {
    l.foldLeft("")((z, y) => {
        z ++ y.loc
      })
  }

  def Program(classes: List[Show], main: Show): Show = {
    new Show {
      def show: String = main.show ++ showList(classes)
      def loc: String = main.loc ++ locList(classes)
    }
  }

  def ClassDef(name: String, parent: Show, fields: List[Show], 
            const: Show, ms: List[Show], pos: Position): Show = {
    new Show {
      def show: String = s"""|class ${name} extends ${parent.show} {
                             |${showList(fields)}
                             |${const.show}
                             |${showList(ms)}
                             |}""".stripMargin
      def loc: String = s"${pos}\n${show}"
    }
  }

  def ConstDef(tpe: Show, params: List[Show], 
    su: Show, finit: List[Show], pos: Position): Show = {
    new Show {
      def show: String = s"""|${tpe.show}(${showList(params)}) {
                             |${su.show}
                             |${showList(finit)}
                             |}""".stripMargin
      def loc: String = s"${pos}\n${show}"
    }
  }

  def FieldInit(name: String, rhs: Show, pos: Position): Show = {
    new Show {
      def show: String = s"${name} = ${rhs.show}"
      def loc: String = s"${pos}\n${show}"
    }
  }

  def Super(exprs: List[Show], pos: Position): Show = {
    new Show {
      def show: String = s"super(${showList(exprs)})"
      def loc: String = s"${pos}\n${show}"
    }
  }

  def MethodDef(tpe: Show, name: String, 
        params: List[Show], body: Show, pos: Position): Show = {
    new Show {
      def show: String = s"""|${tpe}(${showList(params)}) {
                             |${body.show}
                             |}""".stripMargin
      def loc: String = s"${pos}\n${show}"
    }
  }

  def ValDef(tpe: Show, name: String, pos: Position): Show = {
    new Show {
      def show: String = s"${tpe.show} ${name}"
      def loc: String = s"${pos}\n${show}"
    }
  }


  def Ident(name: String, pos: Position): Show = {
    new Show {
      def show: String = name 
      def loc: String = s"${pos}\n${show}"
    }
  }

  def Select(s: Show, m: String, pos: Position): Show = {
    new Show {
      def show: String = s"${s.show}.${m}"
      def loc: String = s"${pos}\n${show}"
    }
  }
  
  def Apply(expr: Show, m: String, args: List[Show], 
        pos: Position): Show = {
    new Show {
      def show: String = s"${expr.show}.${m}(${showList(args)})"
      def loc: String = s"${pos}\n${show}"
    }
  }

  def New(id: Show, args: List[Show], pos: Position): Show = {
    new Show {
      def show: String = s"new ${id}(${showList(args)})"
      def loc: String = s"${pos}\n${show}"
    }
  }

  def Cast(id: Show, expr: Show, pos: Position): Show = {
    new Show {
      def show: String = s"(${id.show}) ${expr.show}" 
      def loc: String = s"${pos}\n${show}"
    }
  }
}

object FJAlgDebug extends FJAlgDebug
