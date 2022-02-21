package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.Kt2CpgTestContext
import io.shiftleft.semanticcpg.language._

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class MethodAtPackageLevelTests extends AnyFreeSpec with Matchers {

  "CPG for code with simple method defined at package-level" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
        |fun double(x: Int): Int {
        |  return x * 2
        |}
        |
        |fun main(args : Array<String>) {
        |  println("The double of 2 is: " + double(2))
        |}
        |""".stripMargin)

    "should contain exactly two non-external methods" in {
      cpg.method.isExternal(false).size shouldBe 2
    }

    "should contain method nodes with the correct fields" in {
      val List(x) = cpg.method.name("double").isExternal(false).l
      x.size shouldBe 1
      x.fullName shouldBe "double:java.lang.Integer(java.lang.Integer)"
      x.code shouldBe "Int(x:Int)"
      x.signature shouldBe "java.lang.Integer(java.lang.Integer)"
      x.isExternal shouldBe false
      x.lineNumber shouldBe Some(1)
      x.columnNumber shouldBe Some(4)
      x.filename.endsWith(".kt") shouldBe true

      val List(y) = cpg.method.name("main").isExternal(false).l
      y.size shouldBe 1
      y.fullName shouldBe "main:kotlin.Unit(kotlin.Array)"
      y.code shouldBe "(args:Array<String>)"
      y.signature shouldBe "kotlin.Unit(kotlin.Array)"
      y.isExternal shouldBe false
      y.lineNumber shouldBe Some(5)
      x.columnNumber shouldBe Some(4)
      y.filename.endsWith(".kt") shouldBe true
    }

    "should allow traversing to parameters" in {
      cpg.method.name("double").isExternal(false).parameter.name.toSet shouldBe Set("x")
      cpg.method.name("main").isExternal(false).parameter.name.toSet shouldBe Set("args")
    }

    "should allow traversing to methodReturn" in {
      cpg.method.name("double").isExternal(false).methodReturn.typeFullName.l shouldBe List("java.lang.Integer")
      cpg.method.name("main").isExternal(false).methodReturn.typeFullName.l shouldBe List("kotlin.Unit")
    }

    "should allow traversing to file" in {
      cpg.method.name("double").isExternal(false).file.name.l should not be empty
      cpg.method.name("main").isExternal(false).file.name.l should not be empty
    }

    "should allow traversing to block" in {
      cpg.method.name("double").isExternal(false).block.l should not be empty
      cpg.method.name("main").isExternal(false).block.l should not be empty
    }
  }
}
