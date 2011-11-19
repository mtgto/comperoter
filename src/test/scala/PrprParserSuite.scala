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
    assert(prpr + one + zero + prpr + one + one + one == compiler.convertExpr(parsedExpr("a"), List("a", "b", "c")))
    assert(prpr + one + one + prpr + one + one + one == compiler.convertExpr(parsedExpr("b"), List("a", "b", "c")))

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

    // 減算
    assert((prpr + one + one + one + one + prpr) + // push 7
	   (prpr + one + one + zero + zero + prpr) + // push 4
	   (one + zero + prpr + one) == // sub
	     compiler.convertExpr(parsedExpr("7-4"), List()))

    // 乗算
    assert((prpr + one + one + one + prpr) + // push 3
	   (prpr + one + one + one + zero + prpr) + // push 6
	   (one + zero + prpr + zero) == // mul
	     compiler.convertExpr(parsedExpr("3*6"), List()))

    // 除算
    assert((prpr + one + one + zero + zero + one + prpr) + // push 9
	   (prpr + one + one + one + prpr) + // push 3
	   (one + zero + one + prpr) == // div
	     compiler.convertExpr(parsedExpr("9/3"), List()))

    // 剰余
    assert((prpr + one + one + zero + one + zero + prpr) + // push 10
	   (prpr + one + one + zero + zero + prpr) + // push 4
	   (one + zero + one + one) == // mod
	     compiler.convertExpr(parsedExpr("10%4"), List()))
  }

  test("compile program") {
    val parser = new MyParser
    val parsedProgram = (in:String) =>
      parser.parseAll(parser.program, in) match {
	case parser.Success(program, _) => program
	case _ => throw new RuntimeException("failed to parse a program: " + in)
      }
    
    val compiler = new MyPrprCompiler
    val prpr = compiler.prpr
    val zero = compiler.zero
    val one = compiler.one
    
    println("programs = " + compiler.convert(parsedProgram("printInt 10+20*30;")))
    println("programs = " + compiler.convert(parsedProgram("while (1) { printInt 42; }")))
    assert(false)
  }
}
