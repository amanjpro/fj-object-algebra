package ch.usi.inf.l3.fjexpr.parser


import ch.usi.inf.l3._
import elang.ast.{Position, NoPosition}
import elang.namer._
import elang.analyzer._
import elang.typecheck._
import elang.eval._
import fj.ast._
import fjexpr.ast._
import fj.parser._
import fj.lang._
import fjexpr.lang._
import fjexpr.namer._
import scala.util.parsing.combinator._
import scala.language.postfixOps



trait FJExprParser extends FJParser {
  type CompilerType = FJExprCompilerTrait 
  val compiler: CompilerType
  override val alg = compiler.FJExprLangAlgObject

  type FExpr = Expr with Others
  type FBinOp = BinOp with Others

  
  lazy val nl: Parser[Literal with Others] = 
    "null"^^(_ => alg.NullLiteral(pos, LitSymbol()))
  lazy val blit: Parser[Literal with Others] = 
    ("false" | "true")^^((x) => 
      alg.Literal(x.toBoolean, pos, LitSymbol()))
  lazy val slit: Parser[Literal with Others] = 
    stringLiteral^^(x => alg.Literal(x, pos, LitSymbol()))
  lazy val ilit: Parser[Literal with Others] = 
    decimalNumber^^(x => alg.Literal(x.toInt, pos, LitSymbol()))
  lazy val flit: Parser[Literal with Others] = 
    floatingPointNumber^^(x => alg.Literal(x.toDouble, pos, LitSymbol()))


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
    case y => ((x: FExpr) => alg.BinOp(y, Or, x, pos, LitSymbol()))
  })

  lazy val and: Parser[FExpr => FBinOp] = wso~>"&&"~wso~>term2^^({
    case y => ((x: FExpr) => alg.BinOp(y, And, x, pos, LitSymbol()))
  })

  lazy val equ: Parser[FExpr => FBinOp] = wso~>"=="~>wso~>term3^^({
    case y => ((x: FExpr) => alg.BinOp(x, Eq, y, pos, LitSymbol()))
  })
  lazy val neq: Parser[FExpr => FBinOp] = wso~>"!="~>wso~>term3^^({
    case y => ((x: FExpr) => alg.BinOp(x, Neq, y, pos, LitSymbol()))
  })
  lazy val gt: Parser[FExpr => FBinOp] = wso~>">"~>wso~>term4^^({
    case y => ((x: FExpr) => alg.BinOp(x, Gt, y, pos, LitSymbol()))
  })
  lazy val lt: Parser[FExpr => FBinOp] = wso~>"<"~>wso~>term4^^({
    case y => ((x: FExpr) => alg.BinOp(x, Lt, y, pos, LitSymbol()))
  })
  lazy val ge: Parser[FExpr => FBinOp] = wso~>">="~>wso~>term4^^({
    case y => ((x: FExpr) => alg.BinOp(x, Geq, y, pos, LitSymbol()))
  })
  lazy val le: Parser[FExpr => FBinOp] = wso~>"<="~>wso~>term4^^({
    case y => ((x: FExpr) => alg.BinOp(x, Leq, y, pos, LitSymbol()))
  })
  lazy val plus: Parser[FExpr => FBinOp] = wso~>"+"~>wso~>term^^({
    case y => ((x: FExpr) => alg.BinOp(x, Add, y, pos, LitSymbol()))
  })
  lazy val minus: Parser[FExpr => FBinOp] = wso~>"-"~>wso~>term^^({
    case y => ((x: FExpr) => alg.BinOp(x, Sub, y, pos, LitSymbol()))
  })
  lazy val div: Parser[FExpr => FBinOp] = wso~>"/"~>wso~>factor^^({
    case y => ((x: FExpr) => alg.BinOp(x, Div, y, pos, LitSymbol()))
  })
  lazy val mul: Parser[FExpr => FBinOp] = wso~>"*"~>wso~>factor^^({
    case y => ((x: FExpr) => alg.BinOp(x, Mul, y, pos, LitSymbol()))
  })
  lazy val mod: Parser[FExpr => FBinOp] = wso~>"%"~>wso~>factor^^({
    case y => ((x: FExpr) => alg.BinOp(x, Mod, y, pos, LitSymbol()))
  })


  lazy val uop: Parser[UniOp with Others] = ("!" | "-" | "+")~(wso~>expr)^^({
    case x => (x._1, x._2) match {
      case ("!", e) => alg.UniOp(Not, e, pos, LitSymbol())
      case ("+", e) => alg.UniOp(Posi, e, pos, LitSymbol())
      case ("-", e) => alg.UniOp(Neg, e, pos, LitSymbol())
    }
  })

  override lazy val keywords = ("null" | "true" | "false" | "new" | "this" |
                                "super" | "class" | "extends" | "return")~ws


}


object FJExprCompiler {
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
      val compiler = new FJExprCompilerTrait{}
    } with FJExprParser{}.startParsing(sources)
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
