import org.scalatest.FunSuite
import scala.collection.mutable.Stack
 
class PrprParserSuite extends FunSuite {
  test("parser") {
    val prpr = "あずにゃん"
    val parser = new PrprParser(prpr)
    val isParsed = (in:String) =>
      parser.parseAll(parser.program, in) match {
	case parser.Success(_, _) => true
	case _ => false
      }
    assert(isParsed("あずにゃん"))
  }
  
  test("expr") {
    val parser = new MyParser
    val parsedExpr = (in:String) =>
      parser.parseAll(parser.expr, in) match {
	case parser.Success(_, _) => true
	case _ => false
      }
    val parsedStatement = (in:String) =>
      parser.parseAll(parser.stat, in) match {
	case parser.Success(_, _) => true
	case _ => false
      }

    assert(parsedExpr("1"))
    assert(!parsedExpr("1.0"))
    assert(!parsedExpr("-1"))
    assert(parsedExpr("func(1, 2, 3)"));
    assert(parsedExpr("func()"));
    assert(parsedExpr("func(a)"));
    assert(parsedStatement("return x;"))
    assert(parsedStatement("def f(a, b, c) { printInt a+b-c; return c; }"))
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
    
    println("programs[1] = " + compiler.convert(parsedProgram("printInt 10+20*30;")))
    println("programs[2] = " + compiler.convert(parsedProgram("while (1 < 2) { printInt 42; }")))
    println("programs[3] = " + compiler.convert(parsedProgram("var x = 10;")))
    println("programs[4] = " + compiler.convert(parsedProgram("var x = 10; printInt x;")))
    println("programs[5] = " + compiler.convert(parsedProgram("var x = 10; var y = 20; printInt y; printInt x;")))
    println("programs[6] = " + compiler.convert(parsedProgram("var x = 10; while (0 < x) { printInt x; x = x - 1;}")))
    println("programs[7] = " + compiler.convert(parsedProgram("var a = f(10);")))
    println("programs[8] = " + compiler.convert(parsedProgram("def f(a, b) { return a + b; }")))
    println("programs[9] = " + compiler.convert(parsedProgram("printInt f(10, 20); def f(a, b) { return a + b; }")))
    println("programs[10] = " + compiler.convert(parsedProgram("printInt f(10, 20); def f(a, b) { var c = 30; return a + b + c; }"))) // ok
    println("programs[11] = " + compiler.convert(parsedProgram("var a = 10; printInt f(20, a); def f(a, b) { var c = 30; var d = 40; return a + b + c; }"))) // ng
    println("programs[12] = " + compiler.convert(parsedProgram("var a = 10; printInt f(10, 20); def f(a, b) { var c = 30; var d = 40; return a + b + c; }"))) // ok
    println("programs[13] = " + compiler.convert(parsedProgram("var a = 10; var b = 20; var c = 30; var d = 40; var e = 50; printInt f(a, b, c, d, e); printInt 9999; printInt 9999; def f(a, b, c, d, e) { var f = 60; var g = 70; return a + b + c + d + e + f + g; }"))) // ok
    println("programs[14] = " + compiler.convert(parsedProgram("var a = 1; while (a < 100) { if (a%3 < 1) { printChar 70;} if (a%5 < 1) { printChar 66; } if (0 < (a%3)*(a%5)) { printChar 66; printInt a; } printChar 65; a = a + 1; }")))
    assert(false)
  }
}
