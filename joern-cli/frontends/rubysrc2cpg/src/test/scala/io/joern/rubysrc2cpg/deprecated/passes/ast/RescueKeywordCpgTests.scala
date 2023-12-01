package io.joern.rubysrc2cpg.deprecated.passes.ast

import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.{EvaluationStrategies, NodeTypes, DispatchTypes, Operators, nodes}
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal

class RescueKeywordCpgTests extends RubyCode2CpgFixture(useDeprecatedFrontend = true) {
  "rescue in the immediate scope of a `def` block" in {
    val cpg = code("""def foo
                     |1/0
                     |rescue ZeroDivisionError => e
                     |end""".stripMargin)

    val methodNode = cpg.method.name("foo").head
    methodNode.name shouldBe "foo"
    methodNode.numberOfLines shouldBe 4
    methodNode.astChildren.isBlock.astChildren.code.contains("try") shouldBe true

    val zeroDivisionErrorIdentifier = cpg.identifier("ZeroDivisionError").head
    zeroDivisionErrorIdentifier.code shouldBe "ZeroDivisionError"
    zeroDivisionErrorIdentifier.astSiblings.isIdentifier.head.name shouldBe "e"
    zeroDivisionErrorIdentifier.astParent.isBlock shouldBe true
  }

  "rescue in the immediate scope of a `do` block" ignore {
    val cpg = code("""foo x do |y|
        |y/0
        |rescue ZeroDivisionError => e
        |end""".stripMargin)

    val zeroDivisionErrorIdentifier = cpg.identifier("ZeroDivisionError").head
    zeroDivisionErrorIdentifier.code shouldBe "ZeroDivisionError"
    zeroDivisionErrorIdentifier.astSiblings.isIdentifier.head.name shouldBe "e"
    zeroDivisionErrorIdentifier.astParent.isBlock shouldBe true
  }

  "rescue in the immediate scope of a `begin` block" in {
    val cpg = code("""begin
        |1/0
        |rescue ZeroDivisionError => e
        |end""".stripMargin)

    val zeroDivisionErrorIdentifier = cpg.identifier("ZeroDivisionError").head
    zeroDivisionErrorIdentifier.code shouldBe "ZeroDivisionError"
    zeroDivisionErrorIdentifier.astSiblings.isIdentifier.head.name shouldBe "e"
    zeroDivisionErrorIdentifier.astParent.isBlock shouldBe true
  }

}
