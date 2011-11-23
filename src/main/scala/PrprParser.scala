import util.parsing.combinator.JavaTokenParsers

/**
 * ペログラム言語で記述されたプログラムのパーサ
 */
class PrprParser(prpr: String) extends JavaTokenParsers {
  sealed trait PrprCommand

  //case class Push(num: Int) extends PrprCommand
  case class Dup() extends PrprCommand

  def program: Parser[List[PrprCommand]] = {
    rep(command)
  }

  def command: Parser[PrprCommand] = {
    //""".+""".r ^^ Push(1)
    """.+""".r ^^ { case _ => Dup() }
  }
}
