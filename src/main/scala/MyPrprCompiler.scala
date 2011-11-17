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
	def find(list:List[String], remain:List[List[String]], idx:Int):Int = {
	  list match {
	    case hd::tl =>
	      if (name == hd) idx else find(tl, remain, idx+1)
	    case _ =>
	      remain match {
		case hd::tl => find(hd, tl, idx)
		case _ => throw new RuntimeException("variable " + name + "is not defined.")
	      }
	  }
	}
	val idx = find(List(), env, 0)
	// 1. 変数のアドレスをスタックに積む 2. スタックトップのアドレスから値を取得する
	(prpr + one + floatToPrprString(idx)) + (one + one + one)
      case Num(num) =>
	prpr + one + floatToPrprString(num)
      case Add(a, b) =>
	convertExpr(a, env) + convertExpr(b, env) + one + zero + prpr + prpr
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
