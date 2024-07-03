package io.joern.javasrc2cpg.querying

import io.joern.javasrc2cpg.testfixtures.JavaSrcCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Identifier, Literal}
import io.shiftleft.semanticcpg.language.*

class ArrayTests extends JavaSrcCode2CpgFixture {

  "constant array initializer expressions" should {
    "take preference in separated declaration and initializations" in {
      val cpg = code("""
                       |class Foo {
                       |  public static void foo() {
                       |    int[] xs;
                       |    xs = new int[] {1, 2, 3};
                       |  }
                       |}
                       |""".stripMargin)

      cpg.method.name("foo").assignment.argument.l match {
        case List(target: Identifier, arrayInitializer: Call) =>
          target.name shouldBe "xs"
          target.code shouldBe "xs"
          target.argumentIndex shouldBe 1
          target.typeFullName shouldBe "int[]"

          arrayInitializer.name shouldBe Operators.arrayInitializer
          arrayInitializer.methodFullName shouldBe Operators.arrayInitializer
          arrayInitializer.code shouldBe "new int[] { 1, 2, 3 }"
          arrayInitializer.typeFullName shouldBe "int[]"

        case result => fail(s"Expected array initializer assignment args but got $result")
      }
    }
    "take preference in combined declaration and initializations" in {
      val cpg = code("""
                       |class Foo {
                       |  public static void foo() {
                       |    int[] xs = new int[] { 1, 2, 3 };
                       |  }
                       |}
                       |""".stripMargin)

      cpg.method.name("foo").assignment.argument.l match {
        case List(target: Identifier, arrayInitializer: Call) =>
          target.name shouldBe "xs"
          target.code shouldBe "xs"
          target.argumentIndex shouldBe 1
          target.typeFullName shouldBe "int[]"

          arrayInitializer.name shouldBe Operators.arrayInitializer
          arrayInitializer.methodFullName shouldBe Operators.arrayInitializer
          arrayInitializer.code shouldBe "new int[] { 1, 2, 3 }"
          arrayInitializer.typeFullName shouldBe "int[]"

        case result => fail(s"Expected array initializer assignment args but got $result")
      }
    }

    "initialize arrays with constant initialization expression" in {
      val cpg = code("""
                       |class Foo {
                       |  public void foo() {
                       |    int[] x = {0, 1, 2};
                       |	 }
                       |}
                       |""".stripMargin)
      def m = cpg.method(".*foo.*")

      val List(arg1: Identifier, arg2: Call) = m.assignment.argument.l: @unchecked

      arg1.code shouldBe "x"
      arg1.typeFullName shouldBe "int[]"

      arg2.code shouldBe "{ 0, 1, 2 }"
      arg2.methodFullName shouldBe "<operator>.arrayInitializer"
      arg2.astChildren.zipWithIndex.foreach { case (arg, idx) =>
        arg shouldBe a[Literal]
        arg.code shouldBe idx.toString
      }
    }
  }

  "should initialize an array with empty initialization expression" in {
    val cpg = code("""
                     |public class Foo {
                     |  public void bar() {
                     |    int[][] x = new int[5][2];
                     |  }
                     |}
                     |""".stripMargin)

    def m = cpg.method(".*bar.*")

    val List(arg1: Identifier, arg2: Call) = m.assignment.argument.l: @unchecked

    arg1.typeFullName shouldBe "int[][]"

    arg2.code shouldBe "new int[5][2]"
    val List(lvl1: Literal, lvl2: Literal) = arg2.argument.l: @unchecked
    lvl1.code shouldBe "5"
    lvl2.code shouldBe "2"
  }

  "arrayIndexAccesses" should {
    val cpg = code("""
                     |class Foo {
                     |  public void baz() {
                     |    int[] x = new int[2];
                     |    x[0] = 1;
                     |    x[1] = x[0] + 2;
                     |  }
                     |}
                     |""".stripMargin)

    "be handled correctly on the LHS of an assignment" in {
      def m                     = cpg.method(".*baz.*")
      val List(_, lhsAccess, _) = m.assignment.l

      val List(indexAccess: Call, _: Literal) = lhsAccess.argument.l: @unchecked
      indexAccess.name shouldBe Operators.indexAccess
      indexAccess.methodFullName shouldBe Operators.indexAccess
      val List(arg1: Identifier, arg2: Literal) = indexAccess.argument.l: @unchecked
      arg1.code shouldBe "x"
      arg1.name shouldBe "x"
      arg1.typeFullName shouldBe "int[]"
      arg2.code shouldBe "0"
    }

    "be handled correctly on the RHS of an assignment" in {
      def m                     = cpg.method(".*baz.*")
      val List(_, _, rhsAccess) = m.assignment.l

      val List(_, add: Call)                           = rhsAccess.argument.l: @unchecked
      val List(access: Call, _: Literal)               = add.argument.l: @unchecked
      val List(identifier: Identifier, index: Literal) = access.argument.l: @unchecked
      identifier.name shouldBe "x"
      identifier.typeFullName shouldBe "int[]"
      index.code shouldBe "0"
    }
  }
}
