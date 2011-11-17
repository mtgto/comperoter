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
    assert(!parsedExpr("-1"))
  }
  
  test("compile") {
    val parser = new MyParser
    val parsedExpr = (in:String) =>
      parser.parseAll(parser.expr, in) match {
	case parser.Success(expr, _) => expr
	case _ => throw new RuntimeException("failed to parse expression: " + in)
      }
    
    val compiler = new MyPrprCompiler
    val prpr = compiler.prpr
    val zero = compiler.zero
    val one = compiler.one

    // 変数
    assert(prpr + one + zero + prpr + one + one + one == compiler.convertExpr(parsedExpr("a"), List(List("a", "b", "c"))))
    assert(prpr + one + one + prpr + one + one + one == compiler.convertExpr(parsedExpr("b"), List(List("a", "b", "c"))))

    // 整数
    assert(prpr + one + zero + prpr == compiler.convertExpr(parsedExpr("0"), List()))
    assert(prpr + one + one + prpr == compiler.convertExpr(parsedExpr("1"), List()))
    assert(prpr + one + one + zero + prpr == compiler.convertExpr(parsedExpr("2"), List()))
    assert(prpr + one + one + zero + zero + prpr == compiler.convertExpr(parsedExpr("4"), List()))

    // 加算
    assert((prpr + one + one + prpr) + // push 1
	   (prpr + one + one + zero + prpr) + // push 2
	   (one + zero + prpr + prpr) == // add
	     compiler.convertExpr(parsedExpr("1+2"), List()))
    
    println(prpr)
  }
}
