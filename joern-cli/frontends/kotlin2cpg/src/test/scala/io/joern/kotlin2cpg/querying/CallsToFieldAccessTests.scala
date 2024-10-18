package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.nodes.{FieldIdentifier, Identifier}
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}
import io.shiftleft.semanticcpg.language.*

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
      c.typeFullName shouldBe "java.lang.String"
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

    "Create only two fieldAccess AST Nodes" in {
      cpg.call(Operators.fieldAccess).size shouldBe 2
      val List(println, ctor) = cpg.call(Operators.fieldAccess).l
      // The one created representing initialisation of member variable inside constructor
      ctor.lineNumber shouldBe None
      // The one created for member access inside println method
      println.lineNumber shouldBe Some(6)
    }
  }

  "Variable access outside class" should {
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

    "Create two fieldAccess AST Node" in {
      cpg.call(Operators.fieldAccess).size shouldBe 2
      val List(ctor, insidemain) = cpg.call(Operators.fieldAccess).l
      // The one created representing initialisation of member variable inside constructor
      ctor.lineNumber shouldBe None
      // The one created for member access inside println method
      insidemain.lineNumber shouldBe Some(8)
    }

    "Create require field access nodes for members accessed outside the class properly" in {
      val List(ctory, insidemainy) = cpg.call.methodFullNameExact(Operators.fieldAccess).l
      ctory.code shouldBe "this.y"
      ctory.name shouldBe Operators.fieldAccess
      insidemainy.code shouldBe "a.y"
      insidemainy.name shouldBe Operators.fieldAccess
    }
  }

  "Chained field access " should {
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
    "Create four fieldAccess nodes" in {
      cpg.call(Operators.fieldAccess).size shouldBe 4
    }

    "2nd level Chained field access CALL node should have name <operator>.fieldAccess" in {
      val List(ctora, ctorb, insidemainx, insidemainacls) = cpg.call.methodFullNameExact(Operators.fieldAccess).l
      ctora.name shouldBe Operators.fieldAccess
      ctorb.name shouldBe Operators.fieldAccess
      insidemainacls.name shouldBe Operators.fieldAccess
      insidemainx.name shouldBe Operators.fieldAccess
    }
  }

  "Chained method call" should {
    val cpg = code("""
                     |package mypkg
                     |class AClass(var x: String){
                     |    fun printX() {
                     |        println(x)
                     |    }
                     |}
                     |class BClass(var acls: AClass){
                     |}
                     |
                     |fun main() {
                     |    val a = AClass("A_MESSAGE")
                     |    val b = BClass(a)
                     |    val m = b.acls.printX()
                     |}
                     |""".stripMargin)
    "Create four fieldAccess nodes" in {
      cpg.call(Operators.fieldAccess).size shouldBe 4
      val List(one, two, three, four) = cpg.call(Operators.fieldAccess).l
      one.code shouldBe "this.x"
      two.code shouldBe "this.x"
      three.code shouldBe "this.acls"
      four.code shouldBe "b.acls"
    }

    "Create CALL nodes for printX" in {
      val List(x) = cpg.call("printX").l
      x.methodFullName shouldBe "mypkg.AClass.printX:void()"
    }
  }

  "Field access after array/map access" should {
    "have correct arguments" in {
      val cpg = code("""
          |val m = LinkedHashMap<Int, User>()
          |val x = m[1].aaa
          |""".stripMargin)

      inside(cpg.call.methodFullNameExact(Operators.fieldAccess).argument.l) { case List(arg1, arg2) =>
        arg1.code shouldBe "m[1]"
        arg1.argumentIndex shouldBe 1
        arg2.code shouldBe "aaa"
        arg2.argumentIndex shouldBe 2
      }
    }
  }

}
