package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.Kt2CpgTestContext
import io.shiftleft.semanticcpg.language._

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class TypeAliasTests extends AnyFreeSpec with Matchers {
  "CPG for code with simple typealias to Int" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
        |package mypkg
        |
        |typealias MyInt = Int
        |
        |fun foo() {
        |  val x: MyInt = 1
        |  val y: Int = 2
        |  val bar = x + y
        |  println(bar)
        |}
        |""".stripMargin)

    "should contain type decl for alias `FooList` of `List<Int>`" in {
      val List(x) = cpg.typeDecl(".*MyInt.*").take(1).l
      x.code shouldBe "typealias MyInt = Int"
      x.name shouldBe "MyInt"
      x.fullName shouldBe "mypkg.MyInt"
      x.isExternal shouldBe false
      x.inheritsFromTypeFullName shouldBe List()
      x.aliasTypeFullName shouldBe Some("kotlin.Int")
    }
  }

  "CPG for code with simple typealias to ListInt" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
        |package mypkg
        |
        |typealias Foo = List<Int>
        |
        |fun main(args : Array<String>) {
        |  val myList: Foo = listOf(1, 2, 3)
        |  myList.forEach {
        |    println("entry: " + it)
        |  }
        |}
        |""".stripMargin)

    "should contain type decl for alias Foo" in {
      val List(x) = cpg.typeDecl(".*Foo.*").l
      x.code shouldBe "typealias Foo = List<Int>"
      x.name shouldBe "Foo"
      x.fullName shouldBe "mypkg.Foo"
      x.isExternal shouldBe false
      x.inheritsFromTypeFullName shouldBe List()
      x.aliasTypeFullName shouldBe Some("kotlin.collections.List<kotlin.Int>")
    }
  }
}
