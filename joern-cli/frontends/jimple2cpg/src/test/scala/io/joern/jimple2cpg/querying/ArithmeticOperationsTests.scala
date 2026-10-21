package io.joern.jimple2cpg.querying

import io.joern.jimple2cpg.testfixtures.JimpleCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes.{Identifier, Literal, Local}
import io.shiftleft.semanticcpg.language.{toNodeTypeStarters, _}

class ArithmeticOperationsTests extends JimpleCode2CpgFixture {
  lazy val cpg: Cpg = code("""
      |import java.util.Random;
      |
      |class Foo {
      |  public static void main(String[] args) {
      |    Random rand = new Random();
      |    double a = rand.nextDouble();
      |    double b = rand.nextDouble();
      |    double c = a + b;
      |    double d = c - a;
      |    double e = a * b;
      |    double f = b / a;
      |    System.out.printf("do not optimize away: %f %f %f %f", c, d, e, f);
      | }
      |}
      |""".stripMargin).cpg

  private val vars =
    Seq(("a", "double"), ("b", "double"), ("c", "double"), ("d", "double"), ("e", "double"), ("f", "double"))

  "should contain call nodes with <operation>.assignment for all variables" in {
    val assignments = cpg.assignment
      .filterNot(_.target.code.startsWith("$"))
      .filterNot(x => List("argc", "argv", "this").contains(x.target.code))
      .map(x => (x.target.code, x.typeFullName))
      .l
    assignments.size shouldBe 7
    vars.foreach { x =>
      assignments contains x shouldBe true
    }
  }

  "should contain a call node for the addition operator" in {
    val List(op)                           = cpg.call.nameExact(Operators.addition).l
    val List(a: Identifier, b: Identifier) = op.astOut.l: @unchecked
    a.name shouldBe "a"
    b.name shouldBe "b"
  }

  "should contain a call node for the subtraction operator" in {
    val List(op)                           = cpg.call(Operators.subtraction).l
    val List(c: Identifier, a: Identifier) = op.astOut.l: @unchecked
    c.name shouldBe "c"
    a.name shouldBe "a"
  }

  "should contain a call node for the multiplication operator" in {
    val List(op)                           = cpg.call(Operators.multiplication).l
    val List(a: Identifier, b: Identifier) = op.astOut.l: @unchecked
    a.name shouldBe "a"
    b.name shouldBe "b"
  }

  "should contain a call node for the division operator" in {
    val List(op)                           = cpg.call(Operators.division).l
    val List(b: Identifier, a: Identifier) = op.astOut.l: @unchecked
    b.name shouldBe "b"
    a.name shouldBe "a"
  }
}
