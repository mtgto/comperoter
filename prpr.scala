import util.parsing.combinator.JavaTokenParsers
/*
object Main {
  def main(args:Array[String]) {
    println("hello");
  }
}
*/

/**
 * プログラム
 */
case class Program(stts:List[Stt])

/**
 * 文
 */
trait Stt {
  
}

case class Define(name:String, value:Exp) extends Stt // 変数宣言
case class Substitute(name:String, value:Exp) extends Stt // 代入
case class While(exp:Exp, stts:List[Stt]) extends Stt // whileループ
case class If(exp:Exp, stts:List[Stt]) extends Stt // if
case class PrintNum(exp:Exp) extends Stt // printNum
case class PrintChar(exp:Exp) extends Stt // printChar
/**
 * 式
 */
trait Exp {
}

case class Var(name:String) extends Exp // 変数
case class Num(digit:Float) extends Exp // 定数(数値)
case class Add(exp1:Exp, exp2:Exp) extends Exp
case class Sub(exp1:Exp, exp2:Exp) extends Exp
case class Multiply(exp1:Exp, exp2:Exp) extends Exp
case class Divide(exp1:Exp, exp2:Exp) extends Exp
case class Modulo(exp1:Exp, exp2:Exp) extends Exp
case class Call(name:String, args:List[Exp]) extends Exp // 関数呼び出し

class MyParser extends JavaTokenParsers {
  def program: Parser[Program] =
    rep(stat) ^^ {
      case stats => Program(stats)
    }

  def stat: Parser[Stt] =
    "var" ~> """[a-zA-Z]+""".r ~ ("=" ~> expr <~ ";") ^^ {
      case name ~ expr => Define(name, expr)
    } | """[a-zA-Z]+""".r ~ ("=" ~> expr <~ ";") ^^ {
      case name ~ expr => Substitute(name, expr)
    } | (("while" ~ "(") ~> expr <~ (")" ~ "{")) ~ rep(stat) <~ "}" ^^ {
      case expr ~ stats => While(expr, stats)
    } | (("if" ~ "(") ~> expr <~ (")" ~ "{")) ~ rep(stat) <~ "}" ^^ {
      case expr ~ stats => If(expr, stats)
    } | "printInt" ~> expr <~ ";" ^^ {
      case expr => PrintNum(expr)
    } | "printChar" ~> expr <~ ";" ^^ {
      case expr => PrintChar(expr)
    }
  
  def expr: Parser[Exp] =
    term ~ rep(("+" | "-") ~ term) ^^ {
      case a ~ rest => {
	rest.foldLeft(a)(
	  (left, right) => right match {
	    case "+"~right => Add(left, right)
	    case "-"~right => Sub(left, right)
	  }
	)
      }
    }
  
  def term: Parser[Exp] =
    factor ~ rep(("*" | "/" | "%") ~ factor) ^^ {
      case a ~ rest => {
	rest.foldLeft(a)(
	  (left, right) => right match {
	    case "*"~right => Multiply(left, right)
	    case "/"~right => Divide(left, right)
	    case "%"~right => Modulo(left, right)
	  }
	)
      }
    }

  def factor: Parser[Exp] =
    "("~>expr<~")" |
  """[1-9][0-9]*|0""".r ^^ {case a => Num(a.toFloat)} |
  """[a-zA-Z]+""".r~"("~repsep(expr, ",")~")"^^ {
    case funcName ~ "(" ~ args ~ ")" => Call(funcName, args)
  } | """[a-zA-Z]+""".r ^^ { Var(_) }

  def parse_expr(data:String) {
    println(data)
    println(parseAll(expr, data))
  }

  def parse_statement(data:String) {
    println(data)
    println(parseAll(stat, data))
  }

  def parse_program(data:String) {
    println(data)
    println(parseAll(program, data))
  }
}

class MyPrprCompiler {
  def convert(program: Program) {
    /*
    program.stats.foreach(
      stt => stt match {
	case Define(name, value) =>
	  // スタックにvalueを評価した結果をpushする
      }
    )
    */
  }

  // 評価結果をスタックトップにpushする表現を返す
  def convertExpr(expr: Exp, env: List[(String, Float)]): String = {
    expr match {
      case Var(name) =>
	env.find(_==name) match {
	  case Some(value) => value.toString
	  case _ => throw new RuntimeException(name+" is not defined")
	}
      case Num(digit) =>
	digit.toString
    }
  }
}

object prpr {
  def main(args: Array[String]) {
    println("hoge")
  }
}
val parser = new MyParser
val parseResult = parser.parseAll(parser.expr, "1")
val compiler = new MyPrprCompiler
parseResult match {
  case parser.Success(exp,_) =>
    println(compiler.convertExpr(exp, List()))
  case _ =>
    println("failed to parse")
}
/*
parser.parse_expr("1+(2+3)*4+5+(6/7)")
parser.parse_expr("1+2")
parser.parse_expr("1+2-3")
parser.parse_expr("1*2*3*4")
parser.parse_expr("((((1))))")
parser.parse_expr("12.3")
parser.parse_expr("func(1,2,3)")
parser.parse_expr("func()")
parser.parse_expr("1+func(1+2*3)")
parser.parse_expr("1+x")
parser.parse_expr("1+x*2")
parser.parse_statement("var x = 1 + 3 * 4;")
parser.parse_statement("x = y * 3;")
parser.parse_statement("while (y % 10) { y = y + 9; x = x + 3;}")
parser.parse_statement("if (10) { y = y + 9; x = x + 3;}")
parser.parse_statement("printInt 10;")
parser.parse_statement("printChar 48;")
parser.parse_program("var x = 48; x = x + 1; printChar x;")
*/
