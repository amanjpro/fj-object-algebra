package ch.usi.inf.l3.ifj.parser


import ch.usi.inf.l3._
import elang.ast.{Position, NoPosition}
import elang.namer._
import elang.analyzer._
import elang.typecheck._
import elang.eval._
import fj.ast._
import fj.parser._
import fj.lang._
import ifj.ast._
import ifj.lang._
import scala.util.parsing.combinator._
import scala.language.postfixOps



trait IFJParser extends FJParser {
  type CompilerType = IFJCompilerTrait 
  val compiler: CompilerType
  override val alg = compiler.IFJLangAlgObject

  lazy val interface: Parser[Interface with Others] = 
    ("interface"~>ws~>id<~ws)~(implements<~wso<~"{"<~wso)~
      rep(abstractMethod)<~wso<~"}"^^({
        case a~b~c => alg.Interface(a.name, b, c, pos, ClassSymbol())
      })

  override def methods: Parser[List[MethodDef with Others]] = rep(method)
  override def method: Parser[MethodDef with Others] = 
    (id<~ws)~(id<~"("<~wso)~(params<~wso<~")"<~wso<~"{"<~
            wso<~"return"<~ws)~(expr<~wso<~";"<~wso<~"}")^^({
      case a~b~c~d => 
        alg.MethodDef(a, b.name, c, d, pos, TermSymbol())
    })

 lazy val abstractMethod: Parser[AbstractMethod with Others] = 
    (id<~ws)~(id<~"("<~wso)~(params<~wso<~")"<~wso<~";")^^({
      case a~b~c => alg.AbstractMethod(a, b.name, c, pos, TermSymbol())
    })

  lazy val implements: Parser[List[Ident with Others]] = 
    (("implements"~>rep1sep(id<~wso, ","))?) ^^ ({
      case x => x match {
        case None => Nil
        case Some(ls) => 
          ls
      }
    })

  def clazzes2: Parser[List[ClassDef with Others]]  = rep(clazz)
  override def clazzes: Parser[List[ClassDef with Others]] = rep(clazz|interface)
  override def clazz: Parser[ClassDef with Others] = {
    ("class"~>ws~>id<~ws)~(parent<~wso)~(implements<~"{"<~wso)~
        (fields<~wso)~(const<~wso)~(methods<~wso<~"}")^^({
      case a~b~c~d~e~f =>
        val cnstr = e match {
          case None => alg.ConstDef(a, Nil, 
                                    alg.Super(Nil, pos, UseSymbol(NoSymbol)),
                                    Nil,
                                    pos, TermSymbol())
          case Some(y) => y
        }
        alg.ClassDef(a.name, b, c, d, cnstr, f, pos, ClassSymbol())
    })
  }


  override lazy val keywords = ("interface" | "null" | "true" | "false" | 
                                "new" | "this" | "super" | "class" | 
                                "extends" | "return" | "implements")~ws


}


object IFJCompiler {
  def main3(args: Array[String]): Unit = {
    val files = args.size match {
      case 0 => 
        List("/Users/amanj/Documents/PhD/MyWork/Programming/ScalaFJ/Test.fj")
      case _ => 
        args.toList
    }
    val sources = files.toList.map((x) => {
      scala.io.Source.fromFile(x).mkString.map((x) => x match {
        case '\t' | '\r' => ' '
        case _ => x
      })
    }).mkString

    val program = new { 
      val compiler = new IFJCompilerTrait{}
    } with IFJParser{}.startParsing(sources)
    program.nameIt(NoSymbol)
    program.bind(NoSymbol)
    val r = program.check
    r match {
      case Success => 
        program.index
        println(program.eval(new Store))
      case Failure(s) => println(s)
    }
    val s = new java.io.PrintWriter("Test.fjc")

    s.print(program.show())
    s.close
  }
}
