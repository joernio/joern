package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.TestContext
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes.Binding
import io.shiftleft.semanticcpg.language._
import io.shiftleft.semanticcpg.language.types.structure.FileTraversal
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class TypeDeclTests extends AnyFreeSpec with Matchers {

  "CPG for simple class" - {
    lazy val cpg = TestContext.buildCpg("""
        |package mypkg
        |
        |import java.lang.Object
        |
        |class Foo: Object {
        |  val z: Int = 1
        |
        |  fun add1(x: Int): Int {
        |    return x + 1
        |  }
        |}
        | """.stripMargin)

    "should contain a TYPE_DECL node for `Foo` with correct fields" in {
      val List(x) = cpg.typeDecl.isExternal(false).name("Foo").l
      x.name shouldBe "Foo"
      x.code shouldBe "Foo"
      x.fullName shouldBe "mypkg.Foo"
      x.inheritsFromTypeFullName shouldBe List("java.lang.Object")
      x.isExternal shouldBe false
      x.lineNumber shouldBe Some(5)
      x.columnNumber shouldBe Some(6)
    }

    "should contain a TYPE_DECL node for `Foo` with two method nodes" in {
      cpg.typeDecl
        .isExternal(false)
        .name("Foo")
        .method
        .fullName
        .toSet shouldBe Set("mypkg.Foo.<init>:void()", "mypkg.Foo.add1:java.lang.Integer(java.lang.Integer)")
    }

    "should contain a TYPE_DECL node for `Foo` with a correct member node" in {
      val List(m) = cpg.typeDecl.isExternal(false).name("Foo").member.l
      m.name shouldBe "z"
      m.typeFullName shouldBe "java.lang.Integer"
    }

    "should contain TYPE_DECL node for the external type `Int`" in {
      val List(x) = cpg.typeDecl.fullNameExact("java.lang.Integer").l
      x.name shouldBe "Integer"
      x.isExternal shouldBe true
      x.inheritsFromTypeFullName shouldBe List()
      x.aliasTypeFullName shouldBe None
      x.filename shouldBe FileTraversal.UNKNOWN
    }
  }

  "CPG for code with user-defined class which has no specific superclasses" - {
    lazy val cpg = TestContext.buildCpg("""
        |package main
        |
        |class AClass
        |
        |fun main() {
        |    val aClass = AClass()
        |    println(aClass.toString())
        |}
        | """.stripMargin)

    "should contain TYPE_DECL node with a value of `java.lang.Object` in its INHERITS_FROM_TYPE_FULL_NAME prop" in {
      val List(x) = cpg.typeDecl.nameExact("AClass").l
      x.inheritsFromTypeFullName shouldBe List("java.lang.Object")
    }
  }

  "class with multiple initializers" - {
    lazy val cpg = TestContext.buildCpg("""
        |package baz
        |
        |import kotlin.io.println
        |
        |open class Foo(x: Int)
        |
        |class Bar(x: Int) : Foo(x) {
        |   val method: Int = 1 + 1
        |
        |   init {
        |     println("initBlock1")
        |   }
        |
        |   init {
        |     println("initBlock2")
        |   }
        |}
        | """.stripMargin)

    /*
    "should contain calls from both initializer blocks" in {
      cpg.call.codeExact("println(\"initBlock1\")").size shouldBe 1
      cpg.call.codeExact("println(\"initBlock2\")").size shouldBe 1
    }
     */
  }

  "CPG for code with simple class declaration and usage" - {
    lazy val cpg = TestContext.buildCpg("""
        |package mypkg
        |
        |class Foo {
        |  fun add1(x: Int): Int {
        |    return x + 1
        |  }
        |}
        |
        |fun main(argc: Int): Int {
        |  val x = Foo()
        |  val y = x.add1(argc)
        |  return y
        |}
        |""".stripMargin)

    "should contain a BINDING node for X with the correct props set" in {
      val List(b) = cpg.all.collect { case b: Binding => b }.l
      b.name shouldBe "add1"
      b.signature shouldBe "java.lang.Integer(java.lang.Integer)"
    }
  }

  "CPG for code with usage of setter of simple user-defined class" - {
    lazy val cpg = TestContext.buildCpg("""
      |package mypkg
      |
      |class Simple {
      |    var message = "HELLO"
      |}
      |
      |fun action(msg: String): String {
      |    val simple = Simple()
      |    //println("before: " + simple.message)
      |    simple.message = msg
      |    //println("after: " + simple.message)
      |    println(simple.message)
      |    return simple.message
      |}
      |
      |fun main() {
      |    action("HELLO, WORLD")
      |}
      | """.stripMargin)

    "should contain a CALL node for the field access inside the assignment with the correct properties set" in {
      val List(c) = cpg.call.methodFullName(Operators.assignment).argument(1).isCall.code("simple.*").l
      c.code shouldBe "simple.message"
      c.methodFullName shouldBe Operators.fieldAccess
    }
  }
}
