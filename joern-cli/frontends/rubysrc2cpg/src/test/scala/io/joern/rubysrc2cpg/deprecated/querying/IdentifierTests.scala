package io.joern.rubysrc2cpg.deprecated.querying

import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class IdentifierTests extends RubyCode2CpgFixture(useDeprecatedFrontend = true) {

  "CPG for code with a function call, arguments and function called from function " should {
    val cpg = code("""
        |
        |def extrareturn()
        |  ret = 6
        |  return ret
        |end
        |
        |def add_three_numbers(num1, num2, num3)
        |  sum = num1 + num2 + num3 + extrareturn()
        |  return sum
        |end
        |
        |a = 1
        |b = 2
        |c = 3
        |
        |sumOfThree = add_three_numbers( a, b, c )
        |""".stripMargin)

    "recognise all identifier nodes" in {
      cpg.identifier.name("a").size shouldBe 2
      cpg.identifier.name("b").size shouldBe 2
      cpg.identifier.name("c").size shouldBe 2
      cpg.identifier.name("sumOfThree").size shouldBe 1
      cpg.identifier.name("num1").size shouldBe 1
      cpg.identifier.name("num2").size shouldBe 1
      cpg.identifier.name("num3").size shouldBe 1
      cpg.identifier.name("sum").size shouldBe 2
      cpg.identifier.name("ret").size shouldBe 2
      cpg.identifier.size shouldBe 16 // 2 identifier node is for methodRef's assigment
    }

    "identify a single call node" in {
      cpg.call.name("add_three_numbers").size shouldBe 1
    }
  }

  "CPG for code with expressions of various types" should {
    val cpg = code("""
        |a = 1
        |b = 2 if a > 1
        |b = !a
        |c = ~a
        |e = +a
        |f = b**a
        |g = a*b
        |h = a+b
        |i = a >> b
        |j = a | b
        |k = a & b
        |l = a && b
        |m = a || b
        |n = a .. b
        |o = a ... b
        |p = ( a > b ) ? c : e
        |q = not p
        |r = p and q
        |s = p or q
        |""".stripMargin)

    "recognise all identifier nodes" in {
      cpg.identifier.name("a").size shouldBe 16
      cpg.identifier.name("b").size shouldBe 13 // unaryExpression
      cpg.identifier.name("c").size shouldBe 2  // unaryExpression
      cpg.identifier.name("e").size shouldBe 2  // unaryExpression
      cpg.identifier.name("f").size shouldBe 1  // powerExpression
      cpg.identifier.name("g").size shouldBe 1  // multiplicative Expression
      cpg.identifier.name("h").size shouldBe 1  // additive Expression
      cpg.identifier.name("i").size shouldBe 1  // bitwise shift Expression
      cpg.identifier.name("j").size shouldBe 1  // bitwise or Expression
      cpg.identifier.name("k").size shouldBe 1  // bitwise and Expression
      cpg.identifier.name("l").size shouldBe 1  // operator and Expression
      cpg.identifier.name("m").size shouldBe 1  // operator or Expression
      cpg.identifier.name("n").size shouldBe 1  // inclusive range Expression
      cpg.identifier.name("o").size shouldBe 1  // exclusive range Expression
      cpg.identifier.name("p").size shouldBe 4  // conditionalOperatorExpression
      cpg.identifier.name("q").size shouldBe 3  // notExpressionOrCommand
      cpg.identifier.name("r").size shouldBe 1  // orAndExpressionOrCommand and part
      cpg.identifier.name("s").size shouldBe 1  // orAndExpressionOrCommand or part
      cpg.identifier.size shouldBe 52
    }
  }

  "CPG for code with identifier and method name conflicts" should {
    val cpg = code("""
        |def create_conflict(id)
        |    puts id
        |end
        |
        |create_conflict = 123
        |
        |puts create_conflict
        |puts create_conflict + 1
        |puts create_conflict(1)
        |
        |""".stripMargin)

    "recognise all identifier nodes" in {
      cpg.identifier
        .name("create_conflict")
        .size shouldBe 4 // 1 identifier node is for methodRef's assignment
    }

    "recognise all call nodes" in {
      cpg.call
        .name("puts")
        .size shouldBe 4

      cpg.call
        .name("create_conflict")
        .size shouldBe 1
    }
  }
}
