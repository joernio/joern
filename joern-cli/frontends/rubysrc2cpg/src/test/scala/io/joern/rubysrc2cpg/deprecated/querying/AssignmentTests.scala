package io.joern.rubysrc2cpg.deprecated.querying

import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.semanticcpg.language.*

class AssignmentTests extends RubyCode2CpgFixture(useDeprecatedFrontend = true) {

  "CPG for code with identifiers and literals in simple assignments" should {
    val cpg = code("""
        |# call instance methods
        |a = 1
        |b = 2
        |a = 3
        |b = 4
        |c = a*b
        |puts "Multiplication is : #{c}"
        |""".stripMargin)

    "recognize all assignment nodes" in {
      cpg.assignment.size shouldBe 6 // One assignment is for `puts = typeRef(__builtin.puts)`
    }

    "have call nodes for <operator>.assignment as method name" in {
      cpg.assignment.foreach { assignment =>
        assignment.name shouldBe Operators.assignment
        assignment.methodFullName shouldBe Operators.assignment
      }
    }

    "should have identifiers as LHS for each assignment node" in {
      cpg.call.nameExact(Operators.assignment).argument.where(_.argumentIndex(1)).foreach { idx =>
        idx.isIdentifier shouldBe true
      }
    }

    "recognise all identifier nodes" in {
      cpg.identifier.name("a").size shouldBe 3
      cpg.identifier.name("b").size shouldBe 3
      cpg.identifier.name("c").size shouldBe 2
    }

    "recognise all literal nodes" in {
      cpg.literal.code("1").size shouldBe 1
      cpg.literal.code("2").size shouldBe 1
      cpg.literal.code("3").size shouldBe 1
      cpg.literal.code("4").size shouldBe 1
    }
  }

  "CPG for code with multiple assignments" should {
    val cpg = code("""
        |a, b, c = [1, 2, 3]
        |a, b, c = b, c, a
        |str1, str2 = ["hello", "world"]
        |p, q = [foo(), bar()]
        |""".stripMargin)

    "recognise all identifier nodes" in {
      cpg.identifier.name("a").size shouldBe 3
      cpg.identifier.name("b").size shouldBe 3
      cpg.identifier.name("c").size shouldBe 3
      cpg.identifier.name("str1").size shouldBe 1
      cpg.identifier.name("str2").size shouldBe 1
      cpg.identifier.name("p").size shouldBe 1
      cpg.identifier.name("q").size shouldBe 1
    }

    "recognise all literal nodes" in {
      cpg.literal.code("1").size shouldBe 1
      cpg.literal.code("2").size shouldBe 1
      cpg.literal.code("3").size shouldBe 1
      cpg.literal.code("\"hello\"").size shouldBe 1
      cpg.literal.code("\"world\"").size shouldBe 1
    }

    "recognize call nodes in RHS" in {
      cpg.call.codeExact("foo()").size shouldBe 1
      cpg.call.codeExact("bar()").size shouldBe 1
    }

    "recognise all assignment call nodes" in {
      /* here we are also checking the synthetic assignment nodes for each element on both sides */
      cpg.call.name(Operators.assignment).size shouldBe 10
    }
  }
}
