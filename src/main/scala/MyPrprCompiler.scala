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

  def convert(program: Program) = {
    convertStatements(program.stts, List())
  }

  var label = 0
  def generateLabel() = {
    val newLabel = label
    label = label + 1
    newLabel
  }

  var functions = Array()
  def generateFuncLabel(name: String) = {
  }

  def convertStatements(stts: List[Stt], env: List[String]): String = {
    stts match {
      case hd::tl => {
	hd match {
	  case Define(name, expr) =>
	    convertExpr(expr, env) + convertStatements(tl, name::env)
	  case Substitute(name, expr) =>
	    val index = env.indexOf(name)
	    if (index >= 0)
	      convertExpr(expr, env) + // push expr
	      floatToPrprString(index) + // push index
	      (one + one + prpr) + // stack[index] = expr
	      convertStatements(tl, env)
	    else
	      throw new RuntimeException("variable " + name + " is not defined.")
	  case While(expr, whileStatements) =>
	    val start = generateLabel() // ループ開始位置
	    val goal = generateLabel() // ループ直後
	    (zero + prpr + prpr + floatToPrprString(start)) + // start:
	    convertExpr(expr, env) + // push expr
	    (zero + one + prpr + floatToPrprString(goal)) + // ifzero goal
	    convertStatements(whileStatements, env) +
	    (zero + prpr + zero + floatToPrprString(start)) + // jmp start
	    (zero + prpr + prpr + floatToPrprString(goal)) + // goal:
	    convertStatements(tl, env)
	  case If(expr, ifStatements) =>
	    val goal = generateLabel()
	    convertExpr(expr, env) + // push expr
	    (zero + one + prpr + floatToPrprString(goal)) + // ifzero goal
	    convertStatements(ifStatements, env) +
	    (zero + prpr + prpr + floatToPrprString(goal)) + // goal:
	    convertStatements(tl, env)
	  case PrintNum(expr) =>
	    println("PrintNum");
	    println(expr);
	    println(env);
	    convertExpr(expr, env) + // push expr
	    (one + prpr + prpr + one) + // print_num
	    convertStatements(tl, env)
	  case PrintChar(expr) =>
	    convertExpr(expr, env) + // push expr
	    (one + prpr + prpr + zero) + // print_char
	    convertStatements(tl, env)
	  case _ =>
	    throw new RuntimeException("not implemented.")
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
	      throw new RuntimeException("variable " + name + " is not defined.")
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
	val label = generateLabel() // 副作用で関数から帰ってくる場所の新しいラベルを生成
	args.map(exp => convertExpr(exp, env)).reduceLeft(_+_) +
	prpr + one + floatToPrprString(label) + // 戻り先をプッシュ
	// Call(name) + // 関数ラベルへのジャンプ
	zero + prpr + prpr + floatToPrprString(label) + // ラベル
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
