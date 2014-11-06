package ch.usi.inf.l3.ifjexpr.parser


import ch.usi.inf.l3._
import elang.ast.{Position, NoPosition}
import elang.namer._
import elang.typecheck._
import elang.eval._
import fj.ast._
import fj.parser._
import fj.lang._
import ifj.ast._
import ifj.lang._
import ifj.parser._
import ifjexpr.lang._
import fjexpr.lang._
import fjexpr.parser._
import fjexpr.ast._
import scala.util.parsing.combinator._
import scala.language.postfixOps



trait IFJExprParser extends FJParser {
  override type CompilerType = IFJExprCompilerTrait 
  val compiler: CompilerType
  override val alg = compiler.IFJExprLangAlgObject

  // Copied from FJExprParser
  type FExpr = Expr with Others
  type FBinOp = BinOp with Others

  
  lazy val nl: Parser[Literal with Others] = 
    "null"^^(_ => alg.NullLiteral(pos))
  lazy val blit: Parser[Literal with Others] = 
    ("false" | "true")^^((x) => 
      alg.Literal(x.toBoolean, pos))
  lazy val slit: Parser[Literal with Others] = 
    stringLiteral^^(x => alg.Literal(x, pos))
  lazy val ilit: Parser[Literal with Others] = 
    decimalNumber^^(x => alg.Literal(x.toInt, pos))
  lazy val flit: Parser[Literal with Others] = 
    floatingPointNumber^^(x => alg.Literal(x.toDouble, pos))


  override def expr: Parser[FExpr] = term1 ~ rep(or) ^^ {
    case a~b => b.foldLeft(a)((z, y) => {
      y(z)
    })
  }

  lazy val term1: Parser[FExpr] = term2 ~ rep (and)  ^^ {
    case a~b => b.foldLeft(a)((z, y) => {
      y(z)
    })
  }

  lazy val term2: Parser[FExpr] = term3 ~ rep (equ | neq)  ^^ {
    case a~b => b.foldLeft(a)((z, y) => {
      y(z)
    })
  }

  lazy val term3: Parser[FExpr] = term4 ~ rep (lt | gt | le | ge)  ^^ {
    case a~b => b.foldLeft(a)((z, y) => {
      y(z)
    })
  }

  lazy val term4: Parser[FExpr] = term ~ rep (plus | minus)  ^^ {
    case a~b => b.foldLeft(a)((z, y) => {
      y(z)
    })
  }

  lazy val term: Parser[FExpr] = factor ~ rep (mod | mul | div) ^^ {
    case a~b => b.foldLeft(a)((z, y) => {
      y(z)
    })
  }

  def factor: Parser[FExpr] = 
    nl | blit | ilit | slit | flit | ("("~>wso~>expr<~wso<~")") | super.expr 



  lazy val or: Parser[FExpr => FBinOp] = wso~>"||"~>wso~>term1^^({
    case y => ((x: FExpr) => alg.BinOp(y, Or, x, pos, UseSymbol(NoSymbol)))
  })

  lazy val and: Parser[FExpr => FBinOp] = wso~>"&&"~wso~>term2^^({
    case y => ((x: FExpr) => alg.BinOp(y, And, x, pos, UseSymbol(NoSymbol)))
  })

  lazy val equ: Parser[FExpr => FBinOp] = wso~>"=="~>wso~>term3^^({
    case y => ((x: FExpr) => alg.BinOp(x, Eq, y, pos, UseSymbol(NoSymbol)))
  })
  lazy val neq: Parser[FExpr => FBinOp] = wso~>"!="~>wso~>term3^^({
    case y => ((x: FExpr) => alg.BinOp(x, Neq, y, pos, UseSymbol(NoSymbol)))
  })
  lazy val gt: Parser[FExpr => FBinOp] = wso~>">"~>wso~>term4^^({
    case y => ((x: FExpr) => alg.BinOp(x, Gt, y, pos, UseSymbol(NoSymbol)))
  })
  lazy val lt: Parser[FExpr => FBinOp] = wso~>"<"~>wso~>term4^^({
    case y => ((x: FExpr) => alg.BinOp(x, Lt, y, pos, UseSymbol(NoSymbol)))
  })
  lazy val ge: Parser[FExpr => FBinOp] = wso~>">="~>wso~>term4^^({
    case y => ((x: FExpr) => alg.BinOp(x, Geq, y, pos, UseSymbol(NoSymbol)))
  })
  lazy val le: Parser[FExpr => FBinOp] = wso~>"<="~>wso~>term4^^({
    case y => ((x: FExpr) => alg.BinOp(x, Leq, y, pos, UseSymbol(NoSymbol)))
  })
  lazy val plus: Parser[FExpr => FBinOp] = wso~>"+"~>wso~>term^^({
    case y => ((x: FExpr) => alg.BinOp(x, Add, y, pos, UseSymbol(NoSymbol)))
  })
  lazy val minus: Parser[FExpr => FBinOp] = wso~>"-"~>wso~>term^^({
    case y => ((x: FExpr) => alg.BinOp(x, Sub, y, pos, UseSymbol(NoSymbol)))
  })
  lazy val div: Parser[FExpr => FBinOp] = wso~>"/"~>wso~>factor^^({
    case y => ((x: FExpr) => alg.BinOp(x, Div, y, pos, UseSymbol(NoSymbol)))
  })
  lazy val mul: Parser[FExpr => FBinOp] = wso~>"*"~>wso~>factor^^({
    case y => ((x: FExpr) => alg.BinOp(x, Mul, y, pos, UseSymbol(NoSymbol)))
  })
  lazy val mod: Parser[FExpr => FBinOp] = wso~>"%"~>wso~>factor^^({
    case y => ((x: FExpr) => alg.BinOp(x, Mod, y, pos, UseSymbol(NoSymbol)))
  })


  lazy val uop: Parser[UniOp with Others] = ("!" | "-" | "+")~(wso~>expr)^^({
    case a~b => (a, b) match {
      case ("!", e) => alg.UniOp(Not, e, pos, UseSymbol(NoSymbol))
      case ("+", e) => alg.UniOp(Posi, e, pos, UseSymbol(NoSymbol))
      case ("-", e) => alg.UniOp(Neg, e, pos, UseSymbol(NoSymbol))
    }
  })




  // Copied from IFJParser
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
                                "extends" | "return" | "implements" | "null" | 
                                "true" | "false")~ws
}


object IFJExprCompiler {
  def main(args: Array[String]): Unit = {
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
      val compiler = new IFJExprCompilerTrait{}
    } with IFJExprParser{}.startParsing(sources)
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
