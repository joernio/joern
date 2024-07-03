package io.joern.javasrc2cpg.querying

import io.joern.javasrc2cpg.testfixtures.JavaSrcCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes.{Identifier, Literal}
import io.shiftleft.semanticcpg.language.*

class BooleanOperationsTests extends JavaSrcCode2CpgFixture {

  lazy val cpg = code("""
      | public class Foo {
      |   public static void main(String[] args) {
      |     boolean a = 1 == 2;
      |     boolean b = 3 != 4;
      |     boolean c = 5 > 6;
      |     boolean d = 7 < 8;
      |     boolean e = 9 >= 10;
      |     boolean f = 11 <= 12;
      |     boolean g = a && b;
      |     boolean h = c || d;
      |     boolean i = !h;
      |     boolean j = a && (b || c);
      |     boolean k = true;
      |   }
      | }
      |""".stripMargin)

  val vars = Seq(
    ("a", "boolean"),
    ("b", "boolean"),
    ("c", "boolean"),
    ("d", "boolean"),
    ("e", "boolean"),
    ("f", "boolean"),
    ("g", "boolean"),
    ("h", "boolean"),
    ("i", "boolean"),
    ("j", "boolean"),
    ("k", "boolean")
  )

  "should contain call nodes with <operation>.assignment for all variables" in {
    val assignments = cpg.assignment.map(x => (x.target.code, x.typeFullName)).l
    assignments.size shouldBe vars.size
    assignments.toSet shouldBe (vars.toSet)
    vars.foreach(x => {
      withClue(s"Assignment to `${x._1}`:") {
        assignments contains x shouldBe true
      }
    })
  }

  "should contain a call node for the equals operator" in {
    val List(op)                     = cpg.call.nameExact(Operators.equals).l
    val List(a: Literal, b: Literal) = op.astOut.l: @unchecked
    a.code shouldBe "1"
    b.code shouldBe "2"
  }

  "should contain a call node for the notEquals operator" in {
    val List(op)                     = cpg.call.nameExact(Operators.notEquals).l
    val List(a: Literal, b: Literal) = op.astOut.l: @unchecked
    a.code shouldBe "3"
    b.code shouldBe "4"
  }

  "should contain a call node for the greaterThan operator" in {
    val List(op)                     = cpg.call.nameExact(Operators.greaterThan).l
    val List(a: Literal, b: Literal) = op.astOut.l: @unchecked
    a.code shouldBe "5"
    b.code shouldBe "6"
  }

  "should contain a call node for the lessThan operator" in {
    val List(op)                     = cpg.call.nameExact(Operators.lessThan).l
    val List(a: Literal, b: Literal) = op.astOut.l: @unchecked
    a.code shouldBe "7"
    b.code shouldBe "8"
  }

  "should contain a call node for the greaterEqualsThan operator" in {
    val List(op)                     = cpg.call.nameExact(Operators.greaterEqualsThan).l
    val List(a: Literal, b: Literal) = op.astOut.l: @unchecked
    a.code shouldBe "9"
    b.code shouldBe "10"
  }

  "should contain a call node for the lessEqualsThan operator" in {
    val List(op)                     = cpg.call.nameExact(Operators.lessEqualsThan).l
    val List(a: Literal, b: Literal) = op.astOut.l: @unchecked
    a.code shouldBe "11"
    b.code shouldBe "12"
  }

  "should contain a call node for the logicalAnd operator" in {
    val op                                 = cpg.call.nameExact(Operators.logicalAnd).head
    val List(a: Identifier, b: Identifier) = op.astOut.l: @unchecked
    a.name shouldBe "a"
    b.name shouldBe "b"
  }

  "should contain a call node for the logicalOr operator" in {
    val op                                 = cpg.call.nameExact(Operators.logicalOr).head
    val List(a: Identifier, b: Identifier) = op.astOut.l: @unchecked
    a.name shouldBe "c"
    b.name shouldBe "d"
  }

  "should contain a call node for the logicalNot operator" in {
    val List(op)            = cpg.call.nameExact(Operators.logicalNot).l
    val List(a: Identifier) = op.astOut.l: @unchecked
    a.name shouldBe "h"
  }
}
