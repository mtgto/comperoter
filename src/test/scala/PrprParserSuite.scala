import org.scalatest.FunSuite
import scala.collection.mutable.Stack
 
class PrprParserSuite extends FunSuite {
  test("expr") {
    val parser = new MyParser
    val parsedExpr = (in:String) =>
      parser.parseAll(parser.expr, in) match {
	case parser.Success(_, _) => true
	case _ => false
      }

    assert(parsedExpr("1"))
    assert(!parsedExpr("1.0"))
    assert(parsedExpr("-1"))
  }
}
