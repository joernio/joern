package io.joern.jimple2cpg.querying

import io.joern.jimple2cpg.testfixtures.JimpleCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes.Identifier
import io.shiftleft.semanticcpg.language.{toNodeTypeStarters, _}

class ArithmeticOperationsTests extends JimpleCode2CpgFixture {

  lazy val cpg: Cpg = code("""
      | class Foo {
      |   static void main(int argc, char argv) {
      |     int a = 3;
      |     double b = 2.0;
      |     double c = a + b;
      |     double d = c - a;
      |     double e = a * b;
      |     double f = b / a;
      |     long g = 1L;
      |     float h = 3.4f;
      |   }
      | }
      |""".stripMargin).cpg

  private val vars = Seq(
    ("a", "byte"),
    ("b", "double"),
    ("c", "double"),
    ("d", "double"),
    ("e", "double"),
    ("f", "double"),
    ("g", "long"),
    ("h", "float")
  )

  "should contain call nodes with <operation>.assignment for all variables" in {
    val assignments = cpg.assignment
      .filterNot(_.target.code.startsWith("$"))
      .filterNot(x => List("argc", "argv", "this").contains(x.target.code))
      .map(x => (x.target.code, x.typeFullName))
      .l
    assignments.size shouldBe 8 // includes casting and 3-address code manipulations
    vars.foreach(x => {
      assignments contains x shouldBe true
    })
  }

  "should contain a call node for the addition operator" in {
    val List(op)                           = cpg.call.nameExact(Operators.addition).l
    val List(a: Identifier, b: Identifier) = op.astOut.l: @unchecked
    a.name shouldBe "$stack16"
    b.name shouldBe "b"
  }

  "should contain a call node for the subtraction operator" in {
    val List(op)                           = cpg.call(Operators.subtraction).l
    val List(c: Identifier, a: Identifier) = op.astOut.l: @unchecked
    c.name shouldBe "c"
    a.name shouldBe "$stack17"
  }

  "should contain a call node for the multiplication operator" in {
    val List(op)                           = cpg.call(Operators.multiplication).l
    val List(a: Identifier, b: Identifier) = op.astOut.l: @unchecked
    a.name shouldBe "$stack18"
    b.name shouldBe "b"
  }

  "should contain a call node for the division operator" in {
    val List(op)                           = cpg.call(Operators.division).l
    val List(b: Identifier, a: Identifier) = op.astOut.l: @unchecked
    a.name shouldBe "$stack19"
    b.name shouldBe "b"
  }
}
