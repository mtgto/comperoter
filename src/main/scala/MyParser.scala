import util.parsing.combinator.JavaTokenParsers

/**
 * プログラム
 */
case class Program(stts:List[Stt])

/**
 * 文
 */
trait Stt

case class Define(name:String, value:Exp) extends Stt // 変数宣言
case class Substitute(name:String, value:Exp) extends Stt // 代入
case class While(exp:Cmp, stts:List[Stt]) extends Stt // whileループ
case class If(exp:Cmp, stts:List[Stt]) extends Stt // if
case class PrintNum(exp:Exp) extends Stt // printNum
case class PrintChar(exp:Exp) extends Stt // printChar
case class Function(name:String, args:List[String], stts:List[Stt]) extends Stt // 関数
case class Return(exp:Exp) extends Stt
/**
 * 式
 */
trait Exp

case class Var(name:String) extends Exp // 変数
case class Num(digit:Float) extends Exp // 定数(数値)
case class Add(exp1:Exp, exp2:Exp) extends Exp
case class Sub(exp1:Exp, exp2:Exp) extends Exp
case class Multiply(exp1:Exp, exp2:Exp) extends Exp
case class Divide(exp1:Exp, exp2:Exp) extends Exp
case class Modulo(exp1:Exp, exp2:Exp) extends Exp
case class Call(name:String, args:List[Exp]) extends Exp // 関数呼び出し

trait Cmp
case class CmpEq(exp1:Exp, exp2:Exp) extends Cmp // exp1 == exp2
case class CmpLT(exp1:Exp, exp2:Exp) extends Cmp // exp1 

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
    } | (("while" ~ "(") ~> comparator <~ (")" ~ "{")) ~ rep(stat) <~ "}" ^^ {
      case cmp ~ stats => While(cmp, stats)
    } | (("if" ~ "(") ~> comparator <~ (")" ~ "{")) ~ rep(stat) <~ "}" ^^ {
      case cmp ~ stats => If(cmp, stats)
    } | "printInt" ~> expr <~ ";" ^^ {
      case expr => PrintNum(expr)
    } | "printChar" ~> expr <~ ";" ^^ {
      case expr => PrintChar(expr)
    } | "return" ~> expr <~ ";" ^^ {
      case expr => Return(expr)
    } | "def" ~> """[a-zA-Z]+""".r ~ ("(" ~> repsep("""[a-zA-Z]+""".r, ",") <~ ")") ~ ("{" ~> rep(stat) <~ "}") ^^ {
      case name ~ args ~ stts => Function(name, args, stts)
    }
  
  def comparator: Parser[Cmp] =
    (expr <~ "==") ~ expr ^^ {
      case exp1 ~ exp2 => CmpEq(exp1, exp2)
    } | (expr <~ "<") ~ expr ^^ {
      case exp1 ~ exp2 => CmpLT(exp1, exp2)
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
