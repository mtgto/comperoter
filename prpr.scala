
class MyPrprCompiler {
  val prpr = "あずにゃん"
  val one = "ペロ"
  val zero = "ﾍﾟﾛ"
  val parser = new MyParser

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
  def convertExpr(expr: Exp, env: List[List[String]]): String = {
    expr match {
      case Var(name) =>
	env.find(_._1==name) match {
	  case Some(value) => prpr + one + floatToPrprString(value._2)
	  case _ => throw new RuntimeException(name+" is not defined")
	}
      case Num(num) =>
	prpr + one + floatToPrprString(num)
    }
  }

  def floatToPrprString(num: Float) = {
    val numInt = num.toInt
    if (numInt == 0)
      zero + prpr
    else
      intToBinList(numInt).reverse.map(a => if (a==0) zero else one).foldRight(prpr)(_+_)
  }

  def intToBinList(num: Int): List[Int] = {
    if (num == 0)
      List()
    else
      num%2 :: intToBinList(num/2)
  }

  def compile(data: String) = {
    val parseResult = parser.parseAll(parser.expr, data)
    val compiler = new MyPrprCompiler
    parseResult match {
      case parser.Success(exp,_) =>
	println(compiler.convertExpr(exp, List()))
      case _ =>
	println("failed to parse")
    }
  }
}

object prpr {
  def main(args: Array[String]) {
    println("hoge")
  }
}
val compiler = new MyPrprCompiler
/*
compiler.compile("0")
compiler.compile("1")
compiler.compile("2")
compiler.compile("3")
compiler.compile("4")
*/
/*
val parser = new MyParser
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
