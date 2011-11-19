class MyPrprCompiler {
  val prpr = "あずにゃん"
  val one = "ペロ"
  val zero = "ﾍﾟﾛ"
  val parser = new MyParser
  
  // コンバートされた中間言語
  trait Converted {
    //def assemble():String
  }

  // コード後の位置に置かれるラベル
  case class Label(code: String) extends Converted
  // 関数へのジャンプ
  case class Jump(name: String) extends Converted
  // 上記以外
  case class Code(code: String) extends Converted

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

  def convertStatements(stts: List[Stt], env: List[String]): String = {
    stts match {
      case hd::tl => {
	hd match {
	  case Define(name, expr) =>
	    convertExpr(expr, env) + convertStatements(tl, name::env)
	}
      }
      case _ => ""
    }
  }

  // 評価結果をスタックトップにpushする表現を返す
  def convertExpr(expr: Exp, env: List[String]): String = {
    expr match {
      case Var(name) =>
	def find(list:List[String], idx:Int):Int = {
	  list match {
	    case hd::tl =>
	      if (name == hd) idx else find(tl, idx+1)
	    case _ =>
	      throw new RuntimeException("variable " + name + "is not defined.")
	  }
	}
	val idx = find(env, 0)
	// 1. 変数のアドレスをスタックに積む 2. スタックトップのアドレスから値を取得する
	(prpr + one + floatToPrprString(idx)) + (one + one + one)
      case Num(num) =>
	prpr + one + floatToPrprString(num)
      case Add(a, b) =>
	convertExpr(a, env) + convertExpr(b, env) + one + zero + prpr + prpr
      case Sub(a, b) =>
	convertExpr(a, env) + convertExpr(b, env) + one + zero + prpr + one
      case Multiply(a, b) =>
	convertExpr(a, env) + convertExpr(b, env) + one + zero + prpr + zero
      case Divide(a, b) =>
	convertExpr(a, env) + convertExpr(b, env) + one + zero + one + prpr
      case Modulo(a, b) =>
	convertExpr(a, env) + convertExpr(b, env) + one + zero + one + one
      case Call(name, args) =>
	// val label = generateLabel() // 副作用で関数から帰ってくる場所の新しいラベルを生成
	args.map(exp => convertExpr(exp, env)).reduceLeft(_+_) +
	// Call(name) + // 関数ラベルへのジャンプ
	// zero + prpr + prpr + floatToPrprString(label) + // ラベル
	(prpr + zero + zero) * args.length // 引数をpop
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
