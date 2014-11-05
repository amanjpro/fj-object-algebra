package ch.usi.inf.l3.fj.parser


import ch.usi.inf.l3._
import elang.ast.{Position, NoPosition}
import elang.namer._
import elang.analyzer._
import elang.eval._
import fj.ast._
import fj.lang._
import scala.util.parsing.combinator._
import scala.util.matching.Regex
import scala.language.postfixOps
import scala.util.parsing.input.CharArrayReader.EofCh





trait FJParser extends JavaTokenParsers with Parsers  {

  type CompilerType <: FJCompilerTrait
  val compiler: CompilerType
  val alg: compiler.FJLangAlg = compiler.FJLangAlgObject
  type Others = compiler.Others



  // This combinator should be able to get pos, lemme see 
  // if this is the case
  def ^^@[U, T](p: Parser[T],
      f: ((Int, Int), T) => U): Parser[U] = Parser { in =>
    val source = in.source
    val offset = in.offset
    val start = handleWhiteSpace(source, offset)
    val inwo = in.drop(start - offset)
    p(inwo) match {
      case Success(t, in1) =>
        {
          var a = 3
          var b = 4
          a = inwo.pos.line
          b = inwo.pos.column
          Success(f((a, b), t), in1)
        }
      case ns: NoSuccess => ns
    }
  } 

  val pos: Position = NoPosition

  val ws: Parser[Any] = rep1(elem("space char", ch => ch <= ' ' && ch != EofCh))
  val wso: Parser[Any] = rep(elem("space char", ch => ch <= ' ' && ch != EofCh))


  


  def startParsing(f: String): Program with Others = {
    val p = parseAll(program, f)
    p.successful match {
      case true =>
        p.get
      case false =>
        println(p)
        throw new Exception("Failed to parse the input")
    }
  }

  lazy val program: Parser[Program with Others] = clazzes~expr^^({
    case a~b => alg.Program(a, b)
  })
  def clazzes: Parser[List[ClassDef with Others]] = rep(clazz)
  def clazz: Parser[ClassDef with Others] = {
    ("class"~>ws~>id<~ws)~(parent<~wso<~"{"<~wso)~
        (fields<~wso)~(const<~wso)~(methods<~wso<~"}")^^({
      case a~b~c~d~e =>
        val cnstr = d match {
          case None => alg.ConstDef(a, Nil, 
                                    alg.Super(Nil, pos, UseSymbol(NoSymbol)),
                                    Nil,
                                    pos, TermSymbol())
          case Some(y) => y
        }
        alg.ClassDef(a.name, b, c, cnstr, e, pos, ClassSymbol())
    })
  }


  lazy val parent: Parser[Ident with Others] = ("extends"~>ws~>id)

  lazy val fields: Parser[List[ValDef with Others]] = 
    rep(param<~wso~";"~wso)^^({
      case ps => ps
    })

  lazy val const: Parser[Option[ConstDef with Others]] = const2?
    
  lazy val const2: Parser[ConstDef with Others] = 
    (id<~wso<~"("<~wso)~(params<~wso<~")"<~wso<~"{"<~wso)~(cbody<~wso<~"}")^^({
      case a~b~c => 
        alg.ConstDef(a, b, c._1, c._2, pos, TermSymbol())
    })

  def methods: Parser[List[MethodDef with Others]] = rep(method)
  def method: Parser[MethodDef with Others] = 
    (id<~ws)~(id<~"("<~wso)~(params<~wso<~")"<~wso<~"{"<~
            wso<~"return"<~ws)~(expr<~wso<~";"<~wso<~"}")^^({
      case a~b~c~d => 
        alg.MethodDef(a, b.name, c, d, pos, TermSymbol())
    })

  lazy val param: Parser[ValDef with Others] = (id<~ws)~id^^({
    case t => alg.ValDef(t._1, t._2.name, pos, TermSymbol())
  })

  lazy val params: Parser[List[ValDef with Others]] = repsep(param<~wso, ",")

  lazy val cbody: Parser[(Super with Others, List[FieldInit with Others])] = 
    ("super"~>wso~>args<~wso<~";"~wso)~(rep(finit))^^({
      case a~b => (alg.Super(a, pos, UseSymbol(NoSymbol)), b)
    })

  lazy val args: Parser[List[Expr with Others]] = 
    ("("~>wso~>repsep(expr<~wso, ",")<~wso~")")^^({
      case l => l
    })
  lazy val finit: Parser[FieldInit with Others] = 
    ("this"~>"."~>id<~wso<~"="<~wso)~(expr<~wso<~";")^^({
      case a~b => alg.FieldInit(a, b, pos, UseSymbol(NoSymbol))
    })

  def expr: Parser[Expr with Others] = 
    (ths | newTree | cast | id)~select~apply^^({
      case a~b~c =>
        (a, b, c) match {
          case (e, None, None) => e
          case (e, None, _) => throw new Exception("unexpected token (")
          case (e, Some(s), None) => 
            alg.Select(e, s.name, pos, UseSymbol(NoSymbol))
          case (e, Some(s), Some(as)) => 
            alg.Apply(e, s.name, as, pos, UseSymbol(NoSymbol)) 
        }
    })

 

  lazy val ths: Parser[This with Others] = 
    "this"^^(_ => alg.This(pos, UseSymbol(NoSymbol)))

  lazy val cast: Parser[Cast with Others] = 
    ("("~>wso~>id<~wso<~")"<~wso)~expr^^({
      case a~b => alg.Cast(a, b, pos, UseSymbol(NoSymbol))
    })
                          

  lazy val newTree: Parser[New with Others] = ("new"~>ws~>id<~wso)~(args)^^({
    case a~b => alg.New(a, b, pos, UseSymbol(NoSymbol))
  }) 

  def select: Parser[Option[Ident with Others]] = (("."~>id)?)

  def apply: Parser[Option[List[Expr with Others]]] = (args)?

  lazy val keywords = ("new" | "this" | "super" | "class" | 
                       "extends" | "return")~ws
  
  lazy val id: Parser[Ident with Others] = {
    not(keywords)~>ident^^((x) => {
      alg.Ident(x, pos, UseSymbol(NoSymbol))
    })
  }
}


object FJCompiler {
  import elang.typecheck._
  def main2(args: Array[String]): Unit = {
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
      val compiler = new FJCompilerTrait{}
    } with FJParser{
      type CompilerType = FJCompilerTrait
    }.startParsing(sources)
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
