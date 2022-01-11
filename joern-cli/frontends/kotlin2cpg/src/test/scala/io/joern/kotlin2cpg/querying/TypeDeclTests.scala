package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.Kt2CpgTestContext
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.semanticcpg.language._
import io.shiftleft.semanticcpg.language.types.structure.FileTraversal
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class TypeDeclTests extends AnyFreeSpec with Matchers {

  "CPG for simple class" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
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
        .toSet shouldBe Set("mypkg.Foo:ANY()", "mypkg.Foo.add1:kotlin.Int(kotlin.Int)")
    }

    "should contain a TYPE_DECL node for `Foo` with a correct member node" in {
      val List(m) = cpg.typeDecl.isExternal(false).name("Foo").member.l
      m.name shouldBe "z"
      m.typeFullName shouldBe "kotlin.Int"
    }

    "should contain TYPE_DECL node for the external type `Int`" in {
      val List(x) = cpg.typeDecl.fullNameExact("kotlin.Int").l
      x.name shouldBe "Int"
      x.isExternal shouldBe true
      x.inheritsFromTypeFullName shouldBe List()
      x.aliasTypeFullName shouldBe None
      x.filename shouldBe FileTraversal.UNKNOWN
    }
  }

  "class with multiple initializers" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
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

  "CPG for code with usage of setter of simple user-defined class" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
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
