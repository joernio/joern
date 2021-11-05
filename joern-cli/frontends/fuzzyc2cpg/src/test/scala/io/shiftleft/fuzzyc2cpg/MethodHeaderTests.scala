package io.shiftleft.fuzzyc2cpg

import io.shiftleft.codepropertygraph.generated._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import overflowdb._

class MethodHeaderTests extends AnyWordSpec with Matchers {
  val fixture = CpgTestFixture("methodheader")

  "Method header" should {

    "have correct METHOD node for method foo" in {
      val methods = fixture.traversalSource.label(NodeTypes.METHOD).has(Properties.NAME -> "foo").l
      methods.size shouldBe 1
      val method = methods.head
      method.property(Properties.IS_EXTERNAL) shouldBe false
      method.property(Properties.FULL_NAME) shouldBe "foo"
      method.property(Properties.SIGNATURE) shouldBe "int foo (int,int)"
      method.property(Properties.LINE_NUMBER) shouldBe 1
      method.property(Properties.COLUMN_NUMBER) shouldBe 0
      method.property(Properties.LINE_NUMBER_END) shouldBe 3
      method.property(Properties.COLUMN_NUMBER_END) shouldBe 0
      method.property(Properties.CODE) shouldBe "int foo (int x,int y)"
    }

    "have correct METHOD_PARAMETER_IN nodes for method foo" in {
      val parameters = fixture.traversalSource
        .label(NodeTypes.METHOD)
        .has(Properties.NAME -> "foo")
        .out(EdgeTypes.AST)
        .hasLabel(NodeTypes.METHOD_PARAMETER_IN)
        .l

      parameters.size shouldBe 2
      val param1Option = parameters.find(_.property(Properties.ORDER) == 1)
      param1Option.isDefined shouldBe true
      param1Option.get.property(Properties.CODE) shouldBe "int x"
      param1Option.get.property(Properties.NAME) shouldBe "x"
      param1Option.get.property(Properties.EVALUATION_STRATEGY) shouldBe EvaluationStrategies.BY_VALUE
      param1Option.get.property(Properties.LINE_NUMBER) shouldBe 1
      param1Option.get.property(Properties.COLUMN_NUMBER) shouldBe 8

      val param2Option = parameters.find(_.property(PropertyNames.ORDER) == 2)
      param2Option.isDefined shouldBe true
      param2Option.isDefined shouldBe true
      param2Option.get.property(Properties.CODE) shouldBe "int y"
      param2Option.get.property(Properties.NAME) shouldBe "y"
      param2Option.get.property(Properties.EVALUATION_STRATEGY) shouldBe EvaluationStrategies.BY_VALUE
      param2Option.get.property(Properties.LINE_NUMBER) shouldBe 1
      param2Option.get.property(Properties.COLUMN_NUMBER) shouldBe 15
    }

    "have correct METHOD_RETURN node for method foo" in {
      val methodReturn = fixture.traversalSource
        .label(NodeTypes.METHOD)
        .has(Properties.NAME -> "foo")
        .out(EdgeTypes.AST)
        .hasLabel(NodeTypes.METHOD_RETURN)
        .l

      methodReturn.size shouldBe 1
      methodReturn.head.property(Properties.CODE) shouldBe "int"
      methodReturn.head.property(Properties.EVALUATION_STRATEGY) shouldBe EvaluationStrategies.BY_VALUE
      methodReturn.head.property(Properties.LINE_NUMBER) shouldBe 1
      methodReturn.head.property(Properties.COLUMN_NUMBER) shouldBe 0
    }

  }

}
