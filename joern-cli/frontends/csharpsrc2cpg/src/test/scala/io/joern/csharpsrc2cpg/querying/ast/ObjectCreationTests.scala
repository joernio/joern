package io.joern.csharpsrc2cpg.querying.ast

import io.joern.csharpsrc2cpg.testfixtures.CSharpCode2CpgFixture
import io.joern.x2cpg.Defines
import io.shiftleft.codepropertygraph.generated.nodes.Identifier
import io.shiftleft.semanticcpg.language.*

class ObjectCreationTests extends CSharpCode2CpgFixture {

  "assignment to an object creation for a known class" should {
    val cpg = code("""
        |using System.Text;
        |var x = new StringBuilder();
        |""".stripMargin)

    "have correct constructor call properties" in {
      inside(cpg.call.nameExact(Defines.ConstructorMethodName).headOption) {
        case Some(ctor) =>
          ctor.typeFullName shouldBe "System.Text.StringBuilder"
          ctor.methodFullName shouldBe "System.Text.StringBuilder.<init>"
        case None => fail(s"Expected a constructor call")
      }
    }

    "have correct typeFullName for the assigned variable" in {
      cpg.assignment.target.isIdentifier.nameExact("x").typeFullName.l shouldBe List("System.Text.StringBuilder")
    }
  }

  "assignment to a fully-qualified object creation for a known class" should {
    val cpg = code("""
        |var x = new System.Text.StringBuilder();
        |""".stripMargin)

    "have correct constructor call properties" in {
      inside(cpg.call.nameExact(Defines.ConstructorMethodName).headOption) {
        case Some(ctor) =>
          ctor.typeFullName shouldBe "System.Text.StringBuilder"
          ctor.methodFullName shouldBe "System.Text.StringBuilder.<init>"
        case None => fail(s"Expected a constructor CALL")
      }
    }

    "have correct typeFullName for the assigned variable" in {
      cpg.assignment.target.isIdentifier.nameExact("x").typeFullName.l shouldBe List("System.Text.StringBuilder")
    }
  }
}
