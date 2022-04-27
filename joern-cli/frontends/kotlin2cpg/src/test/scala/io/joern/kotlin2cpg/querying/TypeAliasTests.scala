package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.TestContext
import io.shiftleft.semanticcpg.language._

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class TypeAliasTests extends AnyFreeSpec with Matchers {
  "CPG for code with simple typealias to Int" - {
    lazy val cpg = TestContext.buildCpg("""
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
      x.code shouldBe "MyInt"
      x.name shouldBe "MyInt"
      x.fullName shouldBe "mypkg.MyInt"
      x.isExternal shouldBe false
      x.inheritsFromTypeFullName shouldBe List()
      x.aliasTypeFullName shouldBe Some("java.lang.Integer")
    }
  }

  // _seemingly_ because adding the springframework jar to the classpath will give the
  // compiler enough information to detect that there is no recursion in the typealias definition
  "CPG for code with seemingly-recursive type alias" - {
    lazy val cpg = TestContext.buildCpg("""
        |package mypkg
        |import org.springframework.data.annotation.Id
        |actual typealias Id = Id
        |fun main() {}
        |""".stripMargin)

    "should contain a type decl for alias `MyInt` with the correct props set" in {
      val List(x) = cpg.typeDecl(".*Id.*").l
      x.name shouldBe "Id"
      x.fullName shouldBe "mypkg.Id"
      x.isExternal shouldBe false
      x.inheritsFromTypeFullName shouldBe List()
      x.aliasTypeFullName shouldBe Some("codepropertygraph.Unresolved")
    }
  }

  "CPG for code with typealias containing other type alias in its RHS" - {
    lazy val cpg = TestContext.buildCpg("""
        |package mypkg
        |
        |
        |typealias MyInt = Int
        |typealias MyArray = Array<MyInt>
        |
        |fun main() {
        |    val out = MyArray(4, {x -> x % 2 })
        |    out.forEach { x -> println(x) }
        |//prints:
        |//```
        |//0
        |//1
        |//0
        |//1
        |//```
        |}
        |""".stripMargin)

    "should contain a type decl for alias `MyInt` with the correct props set" in {
      val List(x) = cpg.typeDecl(".*MyInt.*").take(1).l
      x.code shouldBe "MyInt"
      x.name shouldBe "MyInt"
      x.fullName shouldBe "mypkg.MyInt"
      x.isExternal shouldBe false
      x.inheritsFromTypeFullName shouldBe List()
      x.aliasTypeFullName shouldBe Some("java.lang.Integer")
    }
  }

  "CPG for code with simple typealias to ListInt" - {
    lazy val cpg = TestContext.buildCpg("""
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
      x.code shouldBe "Foo"
      x.name shouldBe "Foo"
      x.fullName shouldBe "mypkg.Foo"
      x.isExternal shouldBe false
      x.inheritsFromTypeFullName shouldBe List()
      x.aliasTypeFullName shouldBe Some("java.util.List")
    }
  }

  "CPG for code with typealias of type from external library" - {
    lazy val cpg = TestContext.buildCpg(
      """
        |package org.http4k.core.body
        |
        |import org.http4k.core.Parameters
        |import org.http4k.core.Request
        |
        |typealias Form = Parameters
        |fun Request.form(): Form = bodyString().toParameters()
        |""".stripMargin,
      includeAllJars = true
    )

    "should contain a TYPE_DECL with the correct ALIAS_TYPE_FULL_NAME set" in {
      cpg.typeDecl.nameExact("Form").aliasTypeFullName.head shouldBe "java.util.List"
    }
  }

  "CPG for code with call to ctor of typealiased user-defined class" - {
    lazy val cpg = TestContext.buildCpg("""
        |package mypkg
        |
        |class AClass(val x: String)
        |typealias ATypeAlias = AClass
        |
        |fun doSomething(p1: String): String {
        |    val aClass = ATypeAlias(p1)
        |    return aClass.x
        |}
        |
        |fun main() {
        |    val aMessage = "AMESSAGE"
        |    val out = doSomething(aMessage)
        |    println(out)
        |//prints:
        |//```
        |//AMESSAGE
        |//```
        |}
        |""".stripMargin)

    "should contain a CALL node for the ctor invocation with the name of the aliased type in it" in {
      cpg.call.methodFullName(".*<init>.*").methodFullName.l shouldBe List("mypkg.AClass.<init>:void(java.lang.String)")
    }
  }
}
