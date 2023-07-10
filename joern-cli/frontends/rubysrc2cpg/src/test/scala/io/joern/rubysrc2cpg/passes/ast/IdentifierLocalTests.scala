package io.joern.rubysrc2cpg.passes.ast

import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.semanticcpg.language._

class IdentifierLocalTests extends RubyCode2CpgFixture {

  val cpg = code("""
      |def method1()
      |  x = 1
      |  x = 2
      |end
      |
      |def method2(x)
      |  x = 2
      |end
      |
      |def method3(x)
      |  y = 0
      |
      |  if true
      |    innerx = 0
      |    innery = 0
      |
      |    innerx = 1
      |    innery = 1
      |  end
      |
      |  x = 1
      |  y = 1
      |end
      |
      |""".stripMargin)

  // TODO: Need to be fixed.
  "be correct for local x in method1" ignore {
    val List(method) = cpg.method.nameExact("method1").l
    method.block.ast.isIdentifier.l.size shouldBe 2
    val List(indentifierX, _) = method.block.ast.isIdentifier.l
    indentifierX.name shouldBe "x"

    val localX = indentifierX._localViaRefOut.get
    localX.name shouldBe "x"
  }

  // TODO: Need to be fixed
  "be correct for parameter x in method2" ignore {
    val List(method)       = cpg.method.nameExact("method2").l
    val List(indentifierX) = method.block.ast.isIdentifier.l
    indentifierX.name shouldBe "x"

    indentifierX.refsTo.l.size shouldBe 1
    val List(paramx) = indentifierX.refsTo.l
    paramx.name shouldBe "x"

    val parameterX = indentifierX._methodParameterInViaRefOut.get
    parameterX.name shouldBe "x"
  }

  // TODO: Need to be fixed.
  "Reach parameter from last identifer" ignore {
    val List(method)           = cpg.method.nameExact("method3").l
    val List(outerIdentifierX) = method.ast.isIdentifier.lineNumber(22).l
    val parameterX             = outerIdentifierX._methodParameterInViaRefOut.get
    parameterX.name shouldBe "x"
  }

  // TODO: Need to be fixed.
  "inner block test" ignore {
    val List(method) = cpg.method.nameExact("method3").l
    method.block.astChildren.isBlock.l.size shouldBe 1
    val List(nestedBlock) = method.block.astChildren.isBlock.l
    nestedBlock.ast.isIdentifier.nameExact("innerx").l.size shouldBe 2
  }

  // TODO: Need to be fixed.
  "nested block identifer to local taversal" ignore {
    val List(method) = cpg.method.nameExact("method3").l
    method.block.astChildren.isBlock.l.size shouldBe 1
    val List(nestedBlock) = method.block.astChildren.isBlock.l
    nestedBlock.ast.isIdentifier.nameExact("innerx").l.size shouldBe 2
    val List(nestedIdentifierX, _) = nestedBlock.ast.isIdentifier.nameExact("innerx").l

    val nestedLocalX = nestedIdentifierX._localViaRefOut.get
    nestedLocalX.name shouldBe "innerx"
  }
}
