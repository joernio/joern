package io.joern.c2cpg

import io.joern.c2cpg.testfixtures.CCodeToCpgSuite
import io.shiftleft.semanticcpg.language._

class MethodInternalLinkageTests extends CCodeToCpgSuite {

  "REF edges" should {
    val cpg = code("""
        |void method1() {
        |  int x;
        |  x = 1;
        |}
        |
        |void method2(int x) {
        |  x = 1;
        |}
        |
        |void method3(int x) {
        |  int y;
        |  {
        |    int x;
        |    int y;
        |
        |    x = 1;
        |    y = 1;
        |  }
        |
        |  x = 1;
        |  y = 1;
        |}
        |""".stripMargin)

    "be correct for local x in method1" in {
      val List(method)       = cpg.method.nameExact("method1").l
      val List(indentifierX) = method.block.ast.isIdentifier.l
      indentifierX.name shouldBe "x"

      val Some(localX) = indentifierX._localViaRefOut
      localX.name shouldBe "x"
    }

    "be correct for parameter x in method2" in {
      val List(method)       = cpg.method.nameExact("method2").l
      val List(indentifierX) = method.block.ast.isIdentifier.l
      indentifierX.name shouldBe "x"

      val Some(parameterX) = indentifierX._methodParameterInViaRefOut
      parameterX.name shouldBe "x"
    }

    "be correct for all identifiers x, y in method3" in {
      val List(method)           = cpg.method.nameExact("method3").l
      val List(outerIdentifierX) = method.block.astChildren.astChildren.isIdentifier.nameExact("x").l

      val Some(parameterX) = outerIdentifierX._methodParameterInViaRefOut
      parameterX.name shouldBe "x"

      val List(expectedParameterX) = method.parameter.l
      expectedParameterX.name shouldBe "x"
      parameterX shouldBe expectedParameterX

      val List(outerIdentifierY) = method.block.astChildren.astChildren.isIdentifier.nameExact("y").l

      val Some(outerLocalY) = outerIdentifierY._localViaRefOut
      outerLocalY.name shouldBe "y"

      val List(expectedOuterLocalY) = method.block.astChildren.isLocal.l
      expectedOuterLocalY.name shouldBe "y"
      outerLocalY shouldBe expectedOuterLocalY

      val List(nestedBlock) = method.block.astChildren.isBlock.l

      val List(nestedIdentifierX) = nestedBlock.ast.isIdentifier.nameExact("x").l
      nestedIdentifierX.name shouldBe "x"

      val Some(nestedLocalX) = nestedIdentifierX._localViaRefOut
      nestedLocalX.name shouldBe "x"

      val List(expectedNestedLocalX) = nestedBlock.ast.isLocal.nameExact("x").l
      nestedLocalX shouldBe expectedNestedLocalX

      val List(nestedIdentifierY) = nestedBlock.ast.isIdentifier.nameExact("y").l
      nestedIdentifierY.name shouldBe "y"

      val Some(nestedLocalY) = nestedIdentifierY._localViaRefOut
      nestedLocalY.name shouldBe "y"

      val List(expectedNestedLocalY) = nestedBlock.ast.isLocal.nameExact("y").l
      nestedLocalY shouldBe expectedNestedLocalY
    }
  }

}
