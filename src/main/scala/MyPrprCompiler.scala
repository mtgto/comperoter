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

  val push = (num: Int) => prpr + one + floatToPrprString(num)
  val pop = prpr + zero + zero
  val storebase = one + one + prpr
  val store = (index: Int) => push(0) + loadbase + push(index) + add + storebase
  val loadbase = one + one + one // load op
  val load = (index: Int) => push(0) + loadbase + push(index) + add + loadbase // push heap[heap[0]+index]
  val add = one + zero + prpr + prpr
  val sub = one + zero + prpr + one
  val mul = one + zero + prpr + zero
  val div = one + zero + one + prpr
  val mod = one + zero + one + one
  val printInt = one + prpr + prpr + one + pop
  val printChar = one + prpr + prpr + zero + pop
  val label = (num: Int) => zero + prpr + prpr + floatToPrprString(num)

  def convert(program: Program) = {
    push(1) + push(0) + storebase + convertStatements(program.stts, List())
  }

  var labelIndex = 0
  def generateLabel() = {
    val newLabel = labelIndex
    labelIndex = labelIndex + 1
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
	    if (env.indexOf(name) >= 0)
	      throw new RuntimeException("declared variable " + name + " is already defined.")
	    else {
	      val index = env.length
	      val nextEnv = env ::: List(name)
	      convertExpr(expr, env) + store(index) + convertStatements(tl, nextEnv)
	    }
	  case Substitute(name, expr) =>
	    val index = env.indexOf(name)
	    if (index >= 0)
	      convertExpr(expr, env) + // push expr
	      store(index) + // heap[heap[0] + index] = stack[1]
	      convertStatements(tl, env)
	    else
	      throw new RuntimeException("variable " + name + " is not defined.")
	  case While(expr, whileStatements) =>
	    val start = generateLabel() // ループ開始位置
	    val goal = generateLabel() // ループ直後
	    label(start) + // start:
	    convertExpr(expr, env) + // push expr
	    (zero + one + prpr + floatToPrprString(goal)) + // ifzero goal
	    convertStatements(whileStatements, env) +
	    (zero + prpr + zero + floatToPrprString(start)) + // jmp start
	    label(goal) + // goal:
	    convertStatements(tl, env)
	  case If(expr, ifStatements) =>
	    val goal = generateLabel()
	    convertExpr(expr, env) + // push expr
	    (zero + one + prpr + floatToPrprString(goal)) + // ifzero goal
	    convertStatements(ifStatements, env) +
	    label(goal) + // goal:
	    convertStatements(tl, env)
	  case PrintNum(expr) =>
	    convertExpr(expr, env) + // push expr
	    printInt + // print_num
	    convertStatements(tl, env)
	  case PrintChar(expr) =>
	    convertExpr(expr, env) + // push expr
	    printChar + // print_char
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
	load(idx)
      case Num(num) =>
	prpr + one + floatToPrprString(num)
      case Add(a, b) =>
	convertExpr(a, env) + convertExpr(b, env) + add
      case Sub(a, b) =>
	convertExpr(a, env) + convertExpr(b, env) + sub
      case Multiply(a, b) =>
	convertExpr(a, env) + convertExpr(b, env) + mul
      case Divide(a, b) =>
	convertExpr(a, env) + convertExpr(b, env) + div
      case Modulo(a, b) =>
	convertExpr(a, env) + convertExpr(b, env) + mod
      case Call(name, args) =>
	val label = generateLabel() // 副作用で関数から帰ってくる場所の新しいラベルを生成
	args.map(exp => convertExpr(exp, env)).reduceLeft(_+_) + // 変数のpush
	// Call(name) + // 関数ラベルへのジャンプ
	zero + prpr + prpr + floatToPrprString(label) + // ラベル
	(pop * args.length) // 引数をpop
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
