abstract class PrprCompiler extends CodeGenerator {
  val one = "ペロ"
  val zero = "ﾍﾟﾛ"
  val parser = new MyParser
    
  def convert(program: Program) = {
    labelIndex = 0
    functions = collection.mutable.Map[String, (Int, Int)]()
    backLabels = collection.mutable.Map[String, collection.mutable.Set[(Int)]]()
    push(1) + push(0) + storebase + convertStatements(program.stts, List(), -1)
  }

  var labelIndex = 0
  def generateLabel() = {
    val newLabel = labelIndex
    labelIndex = labelIndex + 1
    newLabel
  }

  // 関数名から始点ラベル、終点ラベルを保持するマップ
  var functions = collection.mutable.Map[String, (Int, Int)]()
  def generateFuncLabelTuple(name: String) = {
    functions.getOrElseUpdate(name, {
      val startLabel = generateLabel() + 100
      val goalLabel = generateLabel() + 100
      //Console.err.println("startLabel="+startLabel)
      (startLabel, goalLabel)
    })
  }
  // 関数名から戻りラベルの集合を保持するマップ
  var backLabels = collection.mutable.Map[String, collection.mutable.Set[(Int)]]()
  def addFuncBackLabel(funcName: String, label: Int) = {
    backLabels.getOrElseUpdate(funcName, collection.mutable.Set()) += label
  }
  def funcBackLabels(funcName: String) = {
    backLabels.getOrElse(funcName, Set())
  }

  /**
   * 文の配列をコンパイルする
   *
   * @param stts 文の配列
   * @param env 環境
   * @param escapeLabel 関数の中のコンパイル中の時にreturnで飛ぶ先
   */
  def convertStatements(stts: List[Stt], env: List[String], escapeLabel:Int): String = {
    stts match {
      case hd::tl => {
	hd match {
	  case Define(name, expr) =>
	    if (env.indexOf(name) >= 0)
	      throw new RuntimeException("declared variable " + name + " is already defined.")
	    else {
	      val index = env.length
	      val nextEnv = env ::: List(name)
	      convertExpr(expr, env) + store(index) + convertStatements(tl, nextEnv, escapeLabel)
	    }
	  case Substitute(name, expr) =>
	    val index = env.indexOf(name)
	    if (index >= 0)
	      convertExpr(expr, env) + // push expr
	      store(index) + // heap[heap[0] + index] = stack[1]
	      convertStatements(tl, env, escapeLabel)
	    else
	      throw new RuntimeException("variable " + name + " is not defined.")
	  case While(cmp, whileStatements) =>
	    val start = generateLabel() // ループ開始位置
	    val goal = generateLabel() // ループ直後
	    label(start) + // start:
	    convertCmp(cmp, env, goal) +
	    convertStatements(whileStatements, env, escapeLabel) +
	    jmp(start) + // jmp start
	    label(goal) + // goal:
	    convertStatements(tl, env, escapeLabel)
	  case If(cmp, ifStatements) =>
	    val goal = generateLabel()
	    convertCmp(cmp, env, goal) +
	    convertStatements(ifStatements, env, escapeLabel) +
	    label(goal) + // goal:
	    convertStatements(tl, env, escapeLabel)
	  case PrintNum(expr) =>
	    convertExpr(expr, env) + // push expr
	    printInt + // print_num
	    convertStatements(tl, env, escapeLabel)
	  case PrintChar(expr) =>
	    convertExpr(expr, env) + // push expr
	    printChar + // print_char
	    convertStatements(tl, env, escapeLabel)
	  case Return(expr) =>
	    // 親関数のゴール地点にジャンプする
	    convertExpr(expr, env) +
	    jmp(escapeLabel)
	  case Function(name, args, stts) =>
	    val converted = convertStatements(tl, env, escapeLabel) + end
	    val funcTuple = generateFuncLabelTuple(name)
	    val convertedStatements = convertStatements(stts, args, funcTuple._2)
	    //Console.err.println("backLabels="+funcBackLabels(name))
	    converted +
	    label(funcTuple._1) +
	    convertedStatements +
	    label(funcTuple._2) +
	    swap + 
	    (funcBackLabels(name).map(label => dup+push(label)+sub+jz(label)).reduceLeftOption(_+_) match {
	      case Some(a) => a
	      case _ => ""
	    })
	  case _ =>
	    throw new RuntimeException("not implemented.")
	}
      }
      case _ => ""
    }
  }

  // 比較を評価して真だったらtrueLabelにジャンプしない表現を返す
  def convertCmp(cmp: Cmp, env: List[String], trueLabel: Int) = {
    cmp match {
      case CmpEq(exp1, exp2) =>
	val falseLabel = generateLabel()
	convertExpr(exp1, env) +
	convertExpr(exp2, env) +
	sub +
	jz(falseLabel) +
	jmp(trueLabel) +
	label(falseLabel)
      case CmpLT(exp1, exp2) =>
	val falseLabel = generateLabel()
	convertExpr(exp1, env) +
	convertExpr(exp2, env) +
	sub +
	jneg(falseLabel) +
	jmp(trueLabel) +
	label(falseLabel)
      case _ =>
	throw new RuntimeException("not implemented.")
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
	push(num.toInt)
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
	val returnLabel = generateLabel() // 副作用で関数から帰ってくる場所の新しいラベルを生成
	addFuncBackLabel(name, returnLabel)
	val funcTuple = generateFuncLabelTuple(name)
	val converted = push(returnLabel) +
	jmp(funcTuple._1) + // 関数ラベルへのジャンプ
	label(returnLabel)
	if (args.length > 0) {
	  //Console.err.println("args="+args+",env="+env)
	  push(0) + loadbase +
	  args.map(dup+push(env.length)+add+convertExpr(_, env)+swap+storebase+push(1)+add).reduceLeft(_+_) +
	  pop + push(0) + loadbase + push(env.length) + add + push(0) + storebase +
	  converted +
	  pop + push(0) + loadbase + push(env.length) + sub + push(0) + storebase
	} else {
	  converted
	}
      case ReadInt() =>
	readInt
      case ReadChar() =>
	readChar
    }
  }
}

class MyPrprCompiler(target: String) extends {
  val prpr = target
} with PrprCompiler with MyCodeGenerator

object DebugPrprCompiler extends PrprCompiler with DebugCodeGenerator
