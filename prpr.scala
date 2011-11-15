import util.parsing.combinator.JavaTokenParsers
/*
object Main {
  def main(args:Array[String]) {
    println("hello");
  }
}
*/

/**
 * 関数
 */
trait Func {

}

/**
 * 文
 */
trait Stt {
  
}

case class Define(name:String, value:Num) extends Stt // 変数宣言
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
  """[a-zA-Z]+""".r~"("~expr~rep("," ~> expr)~")"^^ {
    case funcName ~ "(" ~ a ~ rest ~ ")" => Call(funcName, a::rest)
  } |
  """[a-zA-Z]+""".r ^^ { Var(_) }

  def parse(data:String) {
    println(parseAll(expr, data))
  }
}

class MyEvaluator {

}

object prpr {
  def main(args: Array[String]) {
    println("hoge")
  }
}
val parser = new MyParser
parser.parse("1+(2+3)*4+5+(6/7)")
parser.parse("1+2")
parser.parse("1+2-3")
parser.parse("1*2*3*4")
parser.parse("((((1))))")
parser.parse("12.3")
parser.parse("func(1,2,3)")
parser.parse("1+func(1+2*3)")
parser.parse("1+x")
parser.parse("1+x*2")

