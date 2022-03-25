package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.TestContext
import io.shiftleft.semanticcpg.language._

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class TypeTests extends AnyFreeSpec with Matchers {

  "CPG for code with simple class" - {
    lazy val cpg = TestContext.buildCpg("""
        |package com.test.PackageFoo
        |
        |class Foo {
        |  val bar: Long = 1
        |
        |  fun baz(x: Any): Int {
        |    val l: Double = 2.0
        |    return 1
        |  }
        |}
        |""".stripMargin)

    "should create TYPE node with correct fields for return type" in {
      val List(x) = cpg.typ.name("Integer").l
      x.fullName shouldBe "java.lang.Integer"
      x.typeDeclFullName shouldBe "java.lang.Integer"
    }

    "should create TYPE node with correct fields for class member" in {
      val List(x) = cpg.typ.name("Long").l
      x.fullName shouldBe "java.lang.Long"
      x.typeDeclFullName shouldBe "java.lang.Long"
    }

    "should create TYPE node with correct fields for parameter type" in {
      val List(x) = cpg.typ.name("Object").l
      x.fullName shouldBe "java.lang.Object"
      x.typeDeclFullName shouldBe "java.lang.Object"
    }

    "should create TYPE node with correct fields for local type" in {
      val List(x) = cpg.typ.name("Double").l
      x.fullName shouldBe "java.lang.Double"
      x.typeDeclFullName shouldBe "java.lang.Double"
    }

    "should allow traversing from member's TYPE to member" in {
      val List(x) = cpg.typ("java.lang.Long").memberOfType.l
      x.name shouldBe "bar"
    }

    "should allow traversing from return params TYPE to return param" in {
      val List(x) = cpg.typ("java.lang.Integer").methodReturnOfType.l
      x.typeFullName shouldBe "java.lang.Integer"
    }

    "should allow traversing from params TYPE to param" in {
      val List(x) = cpg.typ("java.lang.Object").parameterOfType.l
      x.name shouldBe "x"
    }

    "should allow traversing from local's TYPE to local" in {
      val List(x) = cpg.typ("java.lang.Double").localOfType.l
      x.name shouldBe "l"
    }
  }

  "CPG for code with Android SDK fn" - {
    lazy val cpg = TestContext.buildCpg(
      """
        |package mypkg
        |
        |import android.util.Log
        |
        |fun mine() {
        |  Log.d("foo", "bar")
        |}
        |""".stripMargin,
      includeAllJars = true
    )

    "should have type for Log" in {
      val List(x) = cpg.typ.typeDeclFullName(".*Log.*").l
      x.typeDeclFullName shouldBe "android.util.Log"
    }
  }

  "CPG for code with call to a static method from Java's stdlib with a return type different from its receiver type " - {
    lazy val cpg = TestContext.buildCpg("""
        |package mypkg
        |
        |import javax.crypto.Cipher
        |
        |fun main() {
        |    println(Cipher.getMaxAllowedParameterSpec("AES"))
        |}
        |""".stripMargin)

    "should contain TYPE nodes for both the type of the receiver and the type of the return value" in {
      cpg.typ.fullName("javax.crypto.Cipher").size shouldBe 1
      cpg.typ.fullName("java.security.spec.AlgorithmParameterSpec").size shouldBe 1
    }
  }
}
