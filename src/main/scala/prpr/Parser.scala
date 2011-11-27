package prpr

import util.parsing.combinator.JavaTokenParsers

/**
 * ペログラム言語で記述されたプログラムのパーサ
 */
class Parser(prpr: String) extends JavaTokenParsers {
  val zero = "ﾍﾟﾛ"

  val one = "ペロ"
    
  def program: Parser[List[PrprCommand]] = {
    rep(command)
  }

  def command: Parser[PrprCommand] = {
    (prpr ~ one) ~> num ^^ {
      case num => Push(num)
    } ||| prpr ~ zero ~ prpr ^^ {
      _ => Dup()
    } ||| prpr ~ zero ~ one ^^ {
      _ => Swap()
    } ||| prpr ~ zero ~ zero ^^ {
      _ => Pop()
    } ||| one ~ zero ~ prpr ~ prpr ^^ {
      _ => Add()
    } ||| one ~ zero ~ prpr ~ one ^^ {
      _ => Sub()
    } ||| one ~ zero ~ prpr ~ zero ^^ {
      _ => Mul()
    } ||| one ~ zero ~ one ~ prpr ^^ {
      _ => Div()
    } ||| one ~ zero ~ one ~ one ^^ {
      _ => Mod()
    } ||| one ~ one ~ prpr ^^ {
      _ => Store()
    } ||| one ~ one ~ one ^^ {
      _ => Load()
    } ||| (zero ~ prpr ~ prpr) ~> num ^^ {
      label => Label(label)
    } ||| (zero ~ prpr ~ zero) ~> num ^^ {
      label => Jump(label)
    } ||| (zero ~ one ~ prpr) ~> num ^^ {
      label => JZero(label)
    } ||| (zero ~ one ~ one) ~> num ^^ {
      label => JNeg(label)
    } ||| zero ~ zero ~ zero ^^ {
      _ => End()
    } ||| one ~ prpr ~ prpr ~ zero ^^ {
      _ => PrintChar()
    } ||| one ~ prpr ~ prpr ~ one ^^ {
      _ => PrintNum()
    } ||| one ~ prpr ~ one ~ prpr ^^ {
      _ => ReadChar()
    } ||| one ~ prpr ~ one ~ zero ^^ {
      _ => ReadNum()
    }
  }

  def num: Parser[Float] = {
    ((zero | one)+) <~ prpr ^^ {
      _.foldLeft(0F)((num,bit) => num*2+(if (bit == zero) 0 else 1))
    }
  }
}
