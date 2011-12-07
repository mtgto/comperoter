trait CodeGenerator {
  val push: Int=>String
  val dup: String
  val swap: String
  val pop: String
  val storebase: String
  val store: Int => String
  val loadbase: String
  val load: Int => String
  val add: String
  val sub: String
  val mul: String
  val div: String
  val mod: String
  val printInt: String
  val printChar: String
  val readInt: String
  val readChar: String
  val label: Int=>String
  val jmp: Int=>String
  val jz: Int=>String
  val jneg: Int=>String
  val end: String
  def floatToPrprString(a:Float):String
}

trait MyCodeGenerator extends CodeGenerator {
  val prpr: String
  val zero: String
  val one: String
  val push = (num: Int) => prpr + one + floatToPrprString(num)
  val dup = prpr + zero + prpr
  val swap = prpr + zero + one
  val pop = prpr + zero + zero
  val storebase = one + one + prpr
  val store = (index: Int) => push(0) + loadbase + push(index) + add + storebase
  val loadbase = one + one + one // load op
  val load = (index: Int) => push(0) + loadbase + push(index) + add + loadbase
  val add = one + zero + prpr + prpr
  val sub = one + zero + prpr + one
  val mul = one + zero + prpr + zero
  val div = one + zero + one + prpr
  val mod = one + zero + one + one
  val printInt = one + prpr + prpr + one + pop
  val printChar = one + prpr + prpr + zero + pop
  val readInt = one + prpr + one + zero
  val readChar = one + prpr + one + prpr
  val label = (num: Int) => zero + prpr + prpr + floatToPrprString(num)
  val jmp = (label: Int) => zero + prpr + zero + floatToPrprString(label)
  val jz = (label: Int) => zero + one + prpr + floatToPrprString(label)
  val jneg = (label: Int) => zero + one + one + floatToPrprString(label)
  val end = zero + zero + zero
  
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
}

trait DebugCodeGenerator extends CodeGenerator {
  val push = (num: Int) => "push(" + num + ");"
  val dup = "dup;"
  val swap = "swap;"
  val pop = "pop;"
  val storebase = "store;"
  val store = (index: Int) => "store(" + index + ");"
  val loadbase = "load;"
  val load = (index: Int) => ("load(" + index + ");")
  val add = "add;"
  val sub = "sub;"
  val mul = "mul;"
  val div = "div;"
  val mod = "mod;"
  val printInt = "printInt;"
  val printChar = "printChar;"
  val readInt = "readInt;"
  val readChar = "readChar;"
  val label = (num: Int) => "label(" + num + ");"
  val jmp = (num: Int) => "jmp(" + num + ");"
  val jz = (num: Int) => "jz(" + num + ");"
  val jneg = (num: Int) => "jneg(" + num + ");"
  val end = "end;"
  def floatToPrprString(num: Float) = {
    num + ";"
  }
}
