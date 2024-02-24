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

    "Create only one fieldAccess AST Node" ignore {
      cpg.call(Operators.fieldAccess).size shouldBe 1
    }
  }

  "Variable access outside class" ignore {
    val cpg = code("""
        |package mypkg
        |class BClass(var y: String){
        |}
        |
        |fun main() {
        |    val a = BClass("A_MESSAGE")
        |    val m = a.y
        |}
        |""".stripMargin)

    "Create only one fieldAccess AST Node" in {
      cpg.call(Operators.fieldAccess).size shouldBe 1
    }

    "Create require field access nodes for members accessed outside the class properly" in {
      val List(x) = cpg.call.methodFullName(Operators.fieldAccess).l
      x.code shouldBe "a.y"
      x.name shouldBe Operators.fieldAccess
    }
  }

  "Chained field access " ignore {
    val cpg = code("""
        |package mypkg
        |class AClass(var x: String){
        |}
        |class BClass(var acls: AClass){
        |}
        |
        |fun main() {
        |    val a = AClass("A_MESSAGE")
        |    val b = BClass(a)
        |    val m = b.acls.x
        |}
        |""".stripMargin)
    "Create only two fieldAccess nodes" in {
      cpg.call(Operators.fieldAccess).size shouldBe 2
    }

    "2nd level Chained field access CALL node should have name <operator>.fieldAccess" in {
      val List(x, y) = cpg.call.methodFullName(Operators.fieldAccess).l
      x.name shouldBe Operators.fieldAccess
      y.name shouldBe Operators.fieldAccess
    }
  }
}
