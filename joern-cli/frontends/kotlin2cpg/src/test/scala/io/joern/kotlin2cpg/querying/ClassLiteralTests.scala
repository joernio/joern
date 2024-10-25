package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}
import io.shiftleft.semanticcpg.language.*

class ClassLiteralTests extends KotlinCode2CpgFixture(withOssDataflow = false) {

  "CPG for code with simple class literals" should {

    val cpg = code("""
        |package mypkg
        |
        |class Bar {}
        |class Baz {}
        |
        |fun foo() {
        |  println(Bar::class)
        |  println(Baz::class.java)
        |}
        |""".stripMargin)

    "should contain a CALL node for the class literal expression" in {
      val List(c) = cpg.call.code("Bar.*").l
      c.argument.size shouldBe 0
      c.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      c.code shouldBe "Bar::class"
      c.columnNumber shouldBe Some(10)
      c.lineNumber shouldBe Some(8)
      c.signature shouldBe "kotlin.reflect.KClass()"
      c.typeFullName shouldBe "kotlin.reflect.KClass"
      c.methodFullName shouldBe "<operator>.class"
    }

    "should contain a CALL node for the class literal expression inside dot-qualified expression" in {
      val List(c) = cpg.call.code("Baz.*class").l
      c.argument.size shouldBe 0
      c.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      c.code shouldBe "Baz::class"
      c.columnNumber shouldBe Some(10)
      c.lineNumber shouldBe Some(9)
      c.signature shouldBe "kotlin.reflect.KClass()"
      c.typeFullName shouldBe "kotlin.reflect.KClass"
      c.methodFullName shouldBe "<operator>.class"
    }
  }
}
