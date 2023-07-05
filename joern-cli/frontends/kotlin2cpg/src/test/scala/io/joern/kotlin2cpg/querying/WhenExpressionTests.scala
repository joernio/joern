package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.codepropertygraph.generated.nodes.{Block, Identifier, Literal}
import io.shiftleft.semanticcpg.language._

class WhenExpressionTests extends KotlinCode2CpgFixture(withOssDataflow = false) {
  "CPG for code with simple `when`-expression" should {
    val cpg = code("""
        |package mypkg
        |
        |fun myfun() {
        |  val x =  Random.nextInt(0, 3)
        |  val foo = when (x) {
        |      1 -> 123
        |      2 -> 234
        |      else -> {
        |          456
        |      }
        |  }
        |  println(foo)
        |}
        | """.stripMargin)

    "should contain a call node with the correct props and children set" in {
      val List(c) = cpg.call("<operator>.when").l
      c.argumentIndex shouldBe 2
      c.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH

      val List(arg1: Block, arg2: Block, arg3: Block, arg4: Block) = c.argument.l: @unchecked
      arg1.argumentIndex shouldBe 1
      arg2.argumentIndex shouldBe 2
      arg3.argumentIndex shouldBe 3
      arg4.argumentIndex shouldBe 4
      val List(arg1First: Identifier) = arg1.astChildren.l: @unchecked
      arg1First.code shouldBe "x"
      val List(arg2First: Literal, arg2Second: Literal) = arg2.astChildren.l: @unchecked
      arg2First.code shouldBe "1"
      arg2Second.code shouldBe "123"
      val List(arg3First: Literal, arg3Second: Literal) = arg3.astChildren.l: @unchecked
      arg3First.code shouldBe "2"
      arg3Second.code shouldBe "234"
      val List(arg4First: Block)    = arg4.astChildren.l: @unchecked
      val List(arg4First1: Literal) = arg4First.astChildren.l: @unchecked
      arg4First1.code shouldBe "456"
    }
  }
}
