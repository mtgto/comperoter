import util.parsing.combinator.JavaTokenParsers
/*
object Main {
  def main(args:Array[String]) {
    println("hello");
  }
}
*/
/**
 * 文
 */
trait Stt {
  
}

case class Define(name:String, value:Num) extends Stt // 変数宣言
case class While(exp:Exp, stts:List[Stt]) extends Stt // whileループ
case class IF(exp:Exp, stts:List[Stt]) extends Stt // if
case class PRINT(exp:Exp) extends Stt // printNum

/**
 * 式
 */
trait Exp {
}

case class Var(name:String) extends Exp // 変数
case class Num(digit:Float) extends Exp // 定数(数値)
case class ParenExp(exp:Exp) extends Exp // カッコが付いた式
case class Add(exp1:Exp, exp2:Exp) extends Exp
case class Sub(exp1:Exp, exp2:Exp) extends Exp
case class Multiply(exp1:Exp, exp2:Exp) extends Exp
case class Divide(exp1:Exp, exp2:Exp) extends Exp
case class Modulo(exp1:Exp, exp2:Exp) extends Exp

class MyParser extends JavaTokenParsers {
  def expr: Parser[Exp] =
    (term ~ "+" ~ term) ^^ {case a~"+"~b => Add(a, b)} |
  (term ~ "-" ~ term) ^^ {case a~"-"~b => Sub(a, b)} |
  term

  def term: Parser[Exp] =
    (factor ~ "*" ~ factor) ^^ {case a~"*"~b => Multiply(a, b)} |
  (factor ~ "/" ~ factor) ^^ {case a~"/"~b => Divide(a, b)} |
  (factor ~ "%" ~ factor) ^^ {case a~"%"~b => Modulo(a, b)} |
  factor

  def factor: Parser[Exp] =
    "("~>expr<~")" |
  floatingPointNumber ^^ {case a => Num(a.toFloat)}

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
//parser.parse("1+(2+3)*4+5+(6/7)")
//parser.parse("1+2")
//parser.parse("1+2-3")
parser.parse("1*2*3*4")
