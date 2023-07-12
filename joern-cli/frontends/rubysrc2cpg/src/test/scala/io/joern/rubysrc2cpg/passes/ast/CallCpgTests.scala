package io.joern.rubysrc2cpg.passes.ast

import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.{EvaluationStrategies, NodeTypes, DispatchTypes, Operators, nodes}
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal

class CallCpgTests extends RubyCode2CpgFixture {
  "call on identifier with named argument" should {
    val cpg = code("""foo("a", b)""".stripMargin)

    "test call node properties" in {
      val callNode = cpg.call.name("foo").head
      callNode.code shouldBe """foo("a", b)"""
      callNode.signature shouldBe ""
      callNode.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      callNode.lineNumber shouldBe Some(1)
    }

    "test call arguments" in {
      val callNode = cpg.call.name("foo").head
      val arg1     = callNode.argument(1)
      arg1.code shouldBe "\"a\""

      val arg2 = callNode.argument(2)
      arg2.code shouldBe "b"
    }

    "test astChildren" in {
      val callNode = cpg.call.name("foo").head
      val children = callNode.astChildren
      children.size shouldBe 2

      val firstChild  = children.head
      val secondChild = children.last

      firstChild.code shouldBe "\"a\""
      secondChild.code shouldBe "b"
    }
  }

}
