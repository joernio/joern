package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.nodes.{FieldIdentifier, Identifier}
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}
import io.shiftleft.semanticcpg.language._

class CallsToFieldAccessTests extends KotlinCode2CpgFixture(withOssDataflow = false) {
  "CPG for code with class method referencing member in a call" should {
    val cpg = code("""
        |package mypkg
        |
        |class AClass(private val x: String) {
        |    fun printX() {
        |        println(x)
        |    }
        |}
        |
        |fun main() {
        |    val a = AClass("A_MESSAGE")
        |    a.printX()
        |}
        |""".stripMargin)

    "should contain a CALL node for the referenced member with the correct props set" in {
      val List(c) = cpg.call.codeExact("println(x)").argument.isCall.l
      c.code shouldBe "this.x"
      c.name shouldBe Operators.fieldAccess
      c.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      c.lineNumber shouldBe Some(6)
      c.columnNumber shouldBe Some(16)

      val List(firstArg: Identifier, secondArg: FieldIdentifier) =
        cpg.call.codeExact("println(x)").argument.isCall.argument.l: @unchecked
      firstArg.argumentIndex shouldBe 1
      firstArg.code shouldBe "this"
      firstArg.typeFullName shouldBe "mypkg.AClass"

      secondArg.argumentIndex shouldBe 2
      secondArg.code shouldBe "x"
      secondArg.canonicalName shouldBe "x"
    }
  }
}
