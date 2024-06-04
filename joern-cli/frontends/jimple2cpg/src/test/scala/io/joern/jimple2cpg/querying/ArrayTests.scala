package io.joern.jimple2cpg.querying

import io.joern.jimple2cpg.testfixtures.JimpleCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Identifier, Literal}
import io.shiftleft.semanticcpg.language._
import org.scalatest.Failed

class ArrayTests extends JimpleCode2CpgFixture {

  lazy val cpg: Cpg = code("""
      |class Foo {
      |  public void foo() {
      |    int[] x = {0, 1, 2};
      |  }
      |
      |  public void bar() {
      |    int[][] x = new int[5][2];
      |  }
      |
      |  public void baz() {
      |    int[] x = new int[2];
      |    x[0] = 1;
      |    x[1] = x[0] + 2;
      |  }
      |}
      |""".stripMargin).cpg

  "should initialize array with three address code initialization expressions" in {
    def m = cpg.method(".*foo.*")

    val List(placeholderArg: Identifier, arrayInit: Call) =
      m.assignment.codeExact("$stack2 = new int[3]").argument.l: @unchecked
    placeholderArg.code shouldBe "$stack2"
    placeholderArg.typeFullName shouldBe "int[]"

    arrayInit.code shouldBe "new int[3]"
    arrayInit.methodFullName shouldBe Operators.alloc
    arrayInit.astChildren.headOption match {
      case Some(alloc) =>
        alloc shouldBe a[Literal]
        alloc.code shouldBe "3"
      case None => Failed("arrayInitializer should have a literal with the value of 3")
    }

    val List(stackAt0: Call, arg0: Literal) = m.assignment.codeExact("$stack2[0] = 0").argument.l: @unchecked

    arg0.code shouldBe "0"
    arg0.typeFullName shouldBe "int"

    stackAt0.code shouldBe "$stack2[0]"
    stackAt0.methodFullName shouldBe Operators.indexAccess
    val List(stackPointerAt0: Identifier, zero: Literal) = stackAt0.astChildren.l: @unchecked
    stackPointerAt0.code shouldBe "$stack2"
    zero.code shouldBe "0"

    val List(stackAt1: Call, arg1: Literal) = m.assignment.codeExact("$stack2[1] = 1").argument.l: @unchecked

    arg1.code shouldBe "1"
    arg1.typeFullName shouldBe "int"

    stackAt1.code shouldBe "$stack2[1]"
    stackAt1.methodFullName shouldBe Operators.indexAccess
    val List(stackPointerAt1: Identifier, one: Literal) = stackAt1.astChildren.l: @unchecked
    stackPointerAt1.code shouldBe "$stack2"
    one.code shouldBe "1"

    val List(stackAt2: Call, arg2: Literal) = m.assignment.codeExact("$stack2[2] = 2").argument.l: @unchecked

    arg2.code shouldBe "2"
    arg2.typeFullName shouldBe "int"

    stackAt2.code shouldBe "$stack2[2]"
    stackAt2.methodFullName shouldBe Operators.indexAccess
    val List(stackPointerAt2: Identifier, two: Literal) = stackAt2.astChildren.l: @unchecked
    stackPointerAt2.code shouldBe "$stack2"
    two.code shouldBe "2"
  }

  "should initialize an array with empty initialization expression" in {
    def m = cpg.method(".*bar.*")

    val List(arg1: Identifier, arg2: Call) =
      m.assignment.codeExact("x = new int[5][2]").argument.l: @unchecked

    arg1.typeFullName shouldBe "int[][]"

    arg2.code shouldBe "new int[5][2]"
    val List(lvl1: Literal, lvl2: Literal) = arg2.argument.l: @unchecked
    lvl1.code shouldBe "5"
    lvl2.code shouldBe "2"
  }

  "should handle arrayIndexAccesses correctly (3-address code form)" in {
    def m = cpg.method(".*baz.*")

    val List(indexAccess: Call, rhsStub: Identifier) = m.assignment.codeExact("x[1] = $stack3").argument.l: @unchecked
    indexAccess.name shouldBe Operators.indexAccess
    indexAccess.methodFullName shouldBe Operators.indexAccess

    withClue("indexAccess on LHS of assignment") {
      val List(arg1: Identifier, arg2: Literal) = indexAccess.argument.l: @unchecked
      arg1.code shouldBe "x"
      arg1.name shouldBe "x"
      arg1.typeFullName shouldBe "int[]"
      arg2.code shouldBe "1"
    }

    withClue("placeholder in expr on RHS of assignment") {
      rhsStub.name shouldBe "$stack3"
      rhsStub.typeFullName shouldBe "int"
      rhsStub.code shouldBe "$stack3"
    }
  }
}
