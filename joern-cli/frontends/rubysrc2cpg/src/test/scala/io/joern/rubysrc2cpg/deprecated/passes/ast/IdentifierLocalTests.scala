package io.joern.rubysrc2cpg.deprecated.passes.ast

import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class IdentifierLocalTests extends RubyCode2CpgFixture(useDeprecatedFrontend = true) {

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
    val List(identifierX, _) = method.block.ast.isIdentifier.l
    identifierX.name shouldBe "x"

    val localX = identifierX._localViaRefOut.get
    localX.name shouldBe "x"
  }

  "be correct for parameter x in method2" in {
    val List(method)      = cpg.method.nameExact("method2").l
    val List(identifierX) = method.block.ast.isIdentifier.l
    identifierX.name shouldBe "x"

    identifierX.refsTo.l.size shouldBe 1
    val List(paramx) = identifierX.refsTo.l
    paramx.name shouldBe "x"

    val parameterX = identifierX._methodParameterInViaRefOut.get
    parameterX.name shouldBe "x"
  }

  "Reach parameter from last identifier" in {
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
  "nested block identifier to local traversal" ignore {
    val List(method) = cpg.method.nameExact("method3").l
    method.block.astChildren.isBlock.l.size shouldBe 1
    val List(nestedBlock) = method.block.astChildren.isBlock.l
    nestedBlock.ast.isIdentifier.nameExact("innerx").l.size shouldBe 2
    val List(nestedIdentifierX, _) = nestedBlock.ast.isIdentifier.nameExact("innerx").l

    val nestedLocalX = nestedIdentifierX._localViaRefOut.get
    nestedLocalX.name shouldBe "innerx"
  }
}
