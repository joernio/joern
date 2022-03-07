package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.Kt2CpgTestContext
import io.shiftleft.semanticcpg.language._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class MemberTests extends AnyFreeSpec with Matchers {

  "CPG for code with simple member" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
        |class Foo {
        |  val bar: Int = 1
        |}
        |""".stripMargin)

    "should contain MEMBER node with correct properties" in {
      val List(x) = cpg.member("bar").l
      x.name shouldBe "bar"
      x.code shouldBe "bar"
      x.typeFullName shouldBe "java.lang.Integer"
      x.lineNumber shouldBe Some(2)
      x.columnNumber shouldBe Some(6)
      x.order shouldBe 2
    }

    "should allow traversing from MEMBER to TYPE_DECL" in {
      val List(x) = cpg.member.typeDecl.l
      x.name shouldBe "Foo"
    }
  }

  "CPG for code with member defined in primary constructor" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
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

    "should contain MEMBER node with correct properties" in {
      val List(x) = cpg.member("x").l
      x.code shouldBe "private val x: String"
      x.typeFullName shouldBe "java.lang.String"
    }
  }

  "CPG for code with member which gets its init from constructor arg" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
        |class Foo(p: String) {
        |  val bar: Int = p
        |}
        |""".stripMargin)

    "should contain MEMBER node with correct properties" in {
      val List(x) = cpg.member("bar").l
      x.name shouldBe "bar"
      x.code shouldBe "bar"
      x.typeFullName shouldBe "java.lang.Integer"
      x.lineNumber shouldBe Some(2)
      x.columnNumber shouldBe Some(6)
      x.order shouldBe 2
    }

    "should allow traversing from MEMBER to TYPE_DECL" in {
      val List(x) = cpg.member.typeDecl.l
      x.name shouldBe "Foo"
    }

    // TODO: test lowering of setting member to expression
  }

  "CPG for code with member which gets its init from constructor arg with expression" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
        |class Foo(p: String) {
        |  val bar: Int = p + "baz"
        |}
        |""".stripMargin)

    "should contain MEMBER node with correct properties" in {
      val List(x) = cpg.member("bar").l
      x.name shouldBe "bar"
      x.code shouldBe "bar"
      x.typeFullName shouldBe "java.lang.Integer"
      x.lineNumber shouldBe Some(2)
      x.columnNumber shouldBe Some(6)
      x.order shouldBe 2
    }

    "should allow traversing from MEMBER to TYPE_DECL" in {
      val List(x) = cpg.member.typeDecl.l
      x.name shouldBe "Foo"
    }
  }

}
