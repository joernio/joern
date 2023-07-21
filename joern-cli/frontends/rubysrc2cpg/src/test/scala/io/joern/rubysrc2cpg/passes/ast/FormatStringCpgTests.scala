package io.joern.rubysrc2cpg.passes.ast

import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.{EvaluationStrategies, NodeTypes, DispatchTypes, Operators, nodes}
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal

class FormatStringCpgTests extends RubyCode2CpgFixture {
  "#string interpolation" should {
    val cpg = code("""pre#{x}post""".stripMargin)
    "test formatString operator node" ignore {
      val callNode = cpg.call.methodFullName(Operators.formatString).head
      callNode.code shouldBe """pre#{x}post"""
      callNode.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      callNode.lineNumber shouldBe Some(1)
    }

    "test formatString operator node arguments" ignore {
      val callNode = cpg.call.methodFullName(Operators.formatString).head

      val child1 = callNode.astChildren.order(1).isLiteral.head
      child1.code shouldBe "pre"
      child1.argumentIndex shouldBe 1

      val child2 = callNode.astChildren.order(2).isCall.head
      child2.code shouldBe "#{x}"
      child2.argumentIndex shouldBe 2
      child2.methodFullName shouldBe "<operator>.formattedValue"
      child2.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH

      val child3 = callNode.astChildren.order(3).isLiteral.head
      child3.code shouldBe "post"
      child3.argumentIndex shouldBe 3
    }

    "test formattedValue operator child" ignore {
      val callNode = cpg.call.methodFullName("<operator>.formattedValue").head

      val child1 = callNode.astChildren.order(1).isIdentifier.head
      child1.code shouldBe "x"
      child1.argumentIndex shouldBe 1
    }
  }

  "test format string with multiple replacement fields" ignore {
    val cpg      = code("""puts "The number #{a} is less than #{b}"""".stripMargin)
    val callNode = cpg.call.methodFullName("<operator>.formatString").head
    callNode.code shouldBe """puts "The number #{a} is less than #{b}"""""
  }

}
