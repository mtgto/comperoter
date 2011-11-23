import util.parsing.combinator.JavaTokenParsers

/**
 * ペログラム言語で記述されたプログラムのパーサ
 */
class PrprParser(prpr: String) extends JavaTokenParsers {
  val zero = "ﾍﾟﾛ"

  val one = "ペロ"
  
  sealed trait PrprCommand

  case class Push(num: Float) extends PrprCommand
  case class Dup() extends PrprCommand

  def program: Parser[List[PrprCommand]] = {
    rep(command)
  }

  def command: Parser[PrprCommand] = {
    (prpr ~ one) ~> num ^^ {
      case num => Push(num)
    } | """.+""".r ^^ { case _ => Dup() }
  }

  def num: Parser[Float] = {
    ((zero | one)+) <~ prpr ^^ {
      _.foldLeft(0F)((num,bit) => num*2+(if (bit == zero) 0 else 1))
    }
  }
}
