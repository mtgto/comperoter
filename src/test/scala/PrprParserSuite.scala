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
  
  test("compiler can compile expression") {
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
    val load = one + one + one
    val push0 = prpr + one + zero + prpr
    val push1 = prpr + one + one + prpr
    val add = one + zero + prpr + prpr
    assert(push0 + load + push0 + add + load === compiler.convertExpr(parsedExpr("a"), List("a", "b", "c")))
    assert(push0 + load + push1 + add + load === compiler.convertExpr(parsedExpr("b"), List("a", "b", "c")))

    // 整数
    assert(prpr + one + zero + prpr === compiler.convertExpr(parsedExpr("0"), List()))
    assert(prpr + one + one + prpr === compiler.convertExpr(parsedExpr("1"), List()))
    assert(prpr + one + one + zero + prpr === compiler.convertExpr(parsedExpr("2"), List()))
    assert(prpr + one + one + zero + zero + prpr === compiler.convertExpr(parsedExpr("4"), List()))

    // 加算
    assert((prpr + one + one + prpr) + // push 1
	   (prpr + one + one + zero + prpr) + // push 2
	   (one + zero + prpr + prpr) === // add
	     compiler.convertExpr(parsedExpr("1+2"), List()))

    // 減算
    assert((prpr + one + one + one + one + prpr) + // push 7
	   (prpr + one + one + zero + zero + prpr) + // push 4
	   (one + zero + prpr + one) === // sub
	     compiler.convertExpr(parsedExpr("7-4"), List()))

    // 乗算
    assert((prpr + one + one + one + prpr) + // push 3
	   (prpr + one + one + one + zero + prpr) + // push 6
	   (one + zero + prpr + zero) === // mul
	     compiler.convertExpr(parsedExpr("3*6"), List()))

    // 除算
    assert((prpr + one + one + zero + zero + one + prpr) + // push 9
	   (prpr + one + one + one + prpr) + // push 3
	   (one + zero + one + prpr) === // div
	     compiler.convertExpr(parsedExpr("9/3"), List()))

    // 剰余
    assert((prpr + one + one + zero + one + zero + prpr) + // push 10
	   (prpr + one + one + zero + zero + prpr) + // push 4
	   (one + zero + one + one) === // mod
	     compiler.convertExpr(parsedExpr("10%4"), List()))
  }

  test("compiler can compile executable program") {
    val parser = new MyParser
    val parsedProgram = (in:String) =>
      parser.parseAll(parser.program, in) match {
	case parser.Success(program, _) => program
	case _ => throw new RuntimeException("failed to parse a program: " + in)
      }
    val compiler = new MyPrprCompiler
    val id = compiler.prpr
    val zero = compiler.zero
    val one = compiler.one
    val runner = new prpr.Runner(id)
    val getOutput = (in:String) => {
      val bytes = new java.io.ByteArrayOutputStream()
      val out = new java.io.PrintStream(bytes)
      Console.withOut(out)(runner.execute(compiler.convert(parsedProgram(in))))
      new String(bytes.toByteArray)
    }
    assert("610" === getOutput("printInt 10+20*30;"))
    assert("15" === getOutput("def f(a,b){return a+b;}var c=3;printInt f(9,c)+c;"))
    assert("60" === getOutput("var a = 10; printInt f(20, a); def f(a, b) { var c = 30; var d = 40; return a + b + c; }"))
    assert("60" === getOutput("var a = 10; printInt f(10, 20); def f(a, b) { var c = 30; var d = 40; return a + b + c; }"))
    assert("28099999999" === getOutput("var a = 10; var b = 20; var c = 30; var d = 40; var e = 50; printInt f(a, b, c, d, e); printInt 9999; printInt 9999; def f(a, b, c, d, e) { var f = 60; var g = 70; return a + b + c + d + e + f + g; }"))
/*
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
*/
    assert("1 2 Fizz 4 Buzz Fizz 7 8 Fizz Buzz 11 Fizz 13 14 FizzBuzz 16 17 Fizz 19 Buzz Fizz 22 23 Fizz Buzz 26 Fizz 28 29 FizzBuzz 31 32 Fizz 34 Buzz Fizz 37 38 Fizz Buzz 41 Fizz 43 44 FizzBuzz 46 47 Fizz 49 Buzz Fizz 52 53 Fizz Buzz 56 Fizz 58 59 FizzBuzz 61 62 Fizz 64 Buzz Fizz 67 68 Fizz Buzz 71 Fizz 73 74 FizzBuzz 76 77 Fizz 79 Buzz Fizz 82 83 Fizz Buzz 86 Fizz 88 89 FizzBuzz 91 92 Fizz 94 Buzz Fizz 97 98 Fizz " === getOutput("var a = 1; while (a < 100) { if (a%3 < 1) { printChar 70; printChar 105; printChar 122; printChar 122; } if (a%5 < 1) { printChar 66; printChar 117; printChar 122; printChar 122; } if (0 < (a%3)*(a%5)) { printInt a; } printChar 32; a = a + 1; }"))
  }
}
