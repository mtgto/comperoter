package prpr

import org.scalatest.FunSuite
import scala.collection.mutable.Stack
 
class ParserSuite extends FunSuite {
  test("parser can parse encoded integer") {
    val prpr = "あずにゃん"
    val parser = new Parser(prpr)
    val parsedNum = (in:String) =>
      parser.parseAll(parser.num, in) match {
	case parser.Success(result, _) => Some(result)
	case _ => None
      }
    assert(parsedNum("") === None)
    assert(parsedNum(prpr) === None)
    assert(parsedNum(parser.zero + prpr + prpr) === None)
    assert(parsedNum(parser.zero + prpr) === Some(0))
    assert(parsedNum(parser.one + prpr) === Some(1))
    assert(parsedNum(parser.one + parser.one + prpr) === Some(3))
    assert(parsedNum(parser.one + parser.zero + prpr) === Some(2))
    assert(parsedNum(parser.one + (parser.zero * 32) + prpr) === Some(4294967296F))
  }

  test("parser can parse encoded command") {
    val prpr = "あずにゃん"
    val parser = new Parser(prpr)
    var zero = parser.zero
    var one = parser.one
    val parse = (in:String) =>
      parser.parseAll(parser.command, in) match {
	case parser.Success(result, _) => Some(result)
	case _ => None
      }
    assert(parse(prpr) === None)
    assert(parse(prpr + one + one + zero + one + zero + prpr) === Some(Push(10F)))
    assert(parse(zero + prpr + prpr + one + zero + one + prpr) === Some(Label(5F)))
  }

  test("parser can parse encoded program") {
    val prpr = "あずにゃん"
    val parser = new Parser(prpr)
    var zero = parser.zero
    var one = parser.one
    val parse = (in:String) =>
      parser.parseAll(parser.program, in) match {
	case parser.Success(result, _) => Some(result)
	case _ => None
      }
    assert(parse("") === Some(List()))
    assert(parse(prpr) === None)
    assert(parse(prpr + one + one + zero + one + zero + prpr) === Some(List(Push(10F))))
    assert(parse(zero + prpr + prpr + one + zero + one + prpr) === Some(List(Label(5F))))
  }
}
