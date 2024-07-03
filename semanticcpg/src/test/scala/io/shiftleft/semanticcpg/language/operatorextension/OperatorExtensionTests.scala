package io.shiftleft.semanticcpg.language.operatorextension

import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes.Identifier
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.operatorextension.OpNodes.ArrayAccess
import io.shiftleft.semanticcpg.testing.MockCpg
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class OperatorExtensionTests extends AnyWordSpec with Matchers {

  private val methodName = "method"

  def mockCpgWithCallAndCode(name: String, code: String): Cpg =
    MockCpg()
      .withMethod(methodName)
      .withCallInMethod(methodName, name, Some(code))
      .withIdentifierArgument(name, "x", 1)
      .cpg

  "NodeTypeStarters" should {
    "allow retrieving assignments" in {
      val cpg     = mockCpgWithCallAndCode(Operators.assignment, "x = 10")
      val List(x) = cpg.assignment.l
      x.name shouldBe Operators.assignment
      x.code shouldBe "x = 10"
    }

    "allow retrieving arithmetic expressions" in {
      val cpg     = mockCpgWithCallAndCode(Operators.addition, "10 + 20")
      val List(x) = cpg.arithmetic.l
      x.name shouldBe Operators.addition
      x.code shouldBe "10 + 20"
    }

    "include '+=' in both assignments and arithmetics" in {
      val cpg     = mockCpgWithCallAndCode(Operators.assignmentPlus, "x += 10")
      val List(y) = cpg.arithmetic.l
      val List(x) = cpg.assignment.l
      x.id shouldBe y.id
      x.name shouldBe Operators.assignmentPlus
      x.code shouldBe "x += 10"
    }

    "allow traversing to array accesses" in {
      val cpg     = mockCpgWithCallAndCode(Operators.indexAccess, "x[i]")
      val List(x) = cpg.arrayAccess.l
      x.name shouldBe Operators.indexAccess
      x.code shouldBe "x[i]"
    }

    "allow traversing to field accesses" in {
      val cpg     = mockCpgWithCallAndCode(Operators.fieldAccess, "x.y")
      val List(x) = cpg.fieldAccess.l
      x.name shouldBe Operators.fieldAccess
      x.code shouldBe "x.y"
    }

  }

  "OpAstNode" should {
    "allow traversing to assignments" in {
      val cpg     = mockCpgWithCallAndCode(Operators.assignment, "x = 10")
      val List(x) = cpg.method.assignment.l
      x.name shouldBe Operators.assignment
      x.code shouldBe "x = 10"
    }

    "allow traversing to arithmetic expressions" in {
      val cpg     = mockCpgWithCallAndCode(Operators.addition, "10 + 20")
      val List(x) = cpg.method.arithmetic.l
      x.name shouldBe Operators.addition
      x.code shouldBe "10 + 20"
    }

    "allow traversing to array accesses" in {
      val cpg     = mockCpgWithCallAndCode(Operators.indexAccess, "x[i]")
      val List(x) = cpg.method.arrayAccess.l
      x.name shouldBe Operators.indexAccess
      x.code shouldBe "x[i]"
    }

    "allow traversing to field accesses" in {
      val cpg     = mockCpgWithCallAndCode(Operators.fieldAccess, "x.y")
      val List(x) = cpg.method.fieldAccess.l
      x.name shouldBe Operators.fieldAccess
      x.code shouldBe "x.y"
    }

    "allow traversing to enclosing assignment" in {
      val cpg     = mockCpgWithCallAndCode(Operators.assignment, "x = 10")
      val List(x) = cpg.identifier("x").inAssignment.l
      x.name shouldBe Operators.assignment
      x.code shouldBe "x = 10"
    }

    "allow traversing to enclosing arithmetic expressions" in {
      val cpg     = mockCpgWithCallAndCode(Operators.addition, "x + 10")
      val List(x) = cpg.identifier("x").inArithmetic.l
      x.name shouldBe Operators.addition
      x.code shouldBe "x + 10"
    }

    "allow traversing to enclosing array access" in {
      val cpg     = mockCpgWithCallAndCode(Operators.indexAccess, "y[x]")
      val List(x) = cpg.identifier("x").inArrayAccess.l
      x.name shouldBe Operators.indexAccess
      x.code shouldBe "y[x]"
    }

    "allow traversing to enclosing field access" in {
      val cpg     = mockCpgWithCallAndCode(Operators.fieldAccess, "y->x")
      val List(x) = cpg.identifier("x").inFieldAccess.l
      x.name shouldBe Operators.fieldAccess
      x.code shouldBe "y->x"
    }

  }

  "Assignment" should {
    "allow traversing to target/source for assignment with two arguments" in {
      val cpg = MockCpg()
        .withMethod(methodName)
        .withCallInMethod(methodName, Operators.assignment)
        .withIdentifierArgument(Operators.assignment, "x", 1)
        .withIdentifierArgument(Operators.assignment, "y", 2)
        .cpg

      val List(target) = cpg.assignment.target.isIdentifier.l
      target.name shouldBe "x"
      val List(source) = cpg.assignment.source.isIdentifier.l
      source.name shouldBe "y"
    }

    "allow traversing to target/source from assignment with one argument (e.g., 'i++')" in {
      val cpg = MockCpg()
        .withMethod(methodName)
        .withCallInMethod(methodName, Operators.postIncrement)
        .withIdentifierArgument(Operators.postIncrement, "x", 1)
        .cpg

      val List(target) = cpg.assignment.target.isIdentifier.l
      target.name shouldBe "x"
      val List(source) = cpg.assignment.source.isIdentifier.l
      source.name shouldBe "x"
    }

  }

  "Assignment Target" should {
    "only traverse to outer most array access for a target.arrayAccess" in {
      val cpg = MockAssignmentCpg()
        .withCallArgument(Operators.assignment, Operators.indexAccess, "x[y[1]]", 1)
        .withIdentifierArgument(Operators.indexAccess, "x", 1)
        .withCallArgument(Operators.indexAccess, Operators.indexAccess, "y[1]", 2)
        .withIdentifierArgument(Operators.assignment, "y", 2)
        .cpg

      val List(x) = cpg.assignment.target.arrayAccess.l
      x.name shouldBe Operators.indexAccess
      x.code shouldBe "x[y[1]]"
    }

    "allow traversing to pointer" in {
      val cpg = MockAssignmentCpg()
        .withCallArgument(Operators.assignment, Operators.indirection, "*(ptr)")
        .withIdentifierArgument(Operators.indirection, "ptr")
        .cpg
      val List(x) = cpg.assignment.target.pointer.isIdentifier.l
      x.name shouldBe "ptr"
    }

    def MockAssignmentCpg(): MockCpg =
      MockCpg()
        .withMethod(methodName)
        .withCallInMethod(methodName, Operators.assignment)
  }

}
