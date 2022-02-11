package io.joern.javasrc2cpg.querying

import io.joern.javasrc2cpg.testfixtures.JavaSrcCodeToCpgFixture
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Identifier, Literal}
import io.shiftleft.semanticcpg.language.{ICallResolver, NoResolve}
import io.shiftleft.semanticcpg.language._

class ArrayTests extends JavaSrcCodeToCpgFixture {

  implicit val resolver: ICallResolver = NoResolve

  override val code: String =
    """
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
      |""".stripMargin

  "should initialize array with constant initialization expression" in {
    def m = cpg.method(".*foo.*")

    val List(arg1: Identifier, arg2: Call) = m.assignment.argument.l

    arg1.code shouldBe "x"
    arg1.typeFullName shouldBe "int[]"

    arg2.code shouldBe "{ 0, 1, 2 }"
    arg2.methodFullName shouldBe "<operator>.arrayInitializer"
    arg2.astChildren.zipWithIndex.foreach { case (arg, idx) =>
      arg shouldBe a[Literal]
      arg.code shouldBe idx.toString
    }
  }

  "should initialize an array with empty initialization expression" in {
    def m = cpg.method(".*bar.*")

    val List(arg1: Identifier, arg2: Call) = m.assignment.argument.l

    arg1.typeFullName shouldBe "int[][]"

    arg2.code shouldBe "new int[5][2]"
    val List(lvl1: Literal, lvl2: Literal) = arg2.argument.l
    lvl1.code shouldBe "5"
    lvl2.code shouldBe "2"
  }

  "should handle arrayIndexAccesses correctly" in {
    def m = cpg.method(".*baz.*")

    val List(_, lhsAccess, rhsAccess) = m.assignment.l

    withClue("indexAccess on LHS of assignment") {
      val List(indexAccess: Call, _: Literal) = lhsAccess.argument.l
      indexAccess.name shouldBe Operators.indexAccess
      indexAccess.methodFullName shouldBe Operators.indexAccess
      val List(arg1: Identifier, arg2: Literal) = indexAccess.argument.l
      arg1.code shouldBe "x"
      arg1.name shouldBe "x"
      arg1.typeFullName shouldBe "int[]"
      arg2.code shouldBe "0"
    }

    withClue("indexAccess in expr on RHS of assignment") {
      val List(_, add: Call)                           = rhsAccess.argument.l
      val List(access: Call, _: Literal)               = add.argument.l
      val List(identifier: Identifier, index: Literal) = access.argument.l
      identifier.name shouldBe "x"
      identifier.typeFullName shouldBe "int[]"
      index.code shouldBe "0"
    }
  }
}
