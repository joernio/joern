package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.shiftleft.semanticcpg.language._

class TypeTests extends KotlinCode2CpgFixture(withOssDataflow = false) {

  "CPG for code with simple class with one method and one member" should {
    lazy val cpg = code("""
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

    "should contain a TYPE node for the return type of the method with the correct props set" in {
      val List(t) = cpg.typ.name("int").l
      t.fullName shouldBe "int"
      t.typeDeclFullName shouldBe "int"
    }

    "should contain a TYPE node for the member with the correct props set" in {
      val List(t) = cpg.typ.name("long").l
      t.fullName shouldBe "long"
      t.typeDeclFullName shouldBe "long"
    }

    "should contain a TYPE node for the parameter type of the method with the correct props set" in {
      val List(t) = cpg.typ.name("Object").l
      t.fullName shouldBe "java.lang.Object"
      t.typeDeclFullName shouldBe "java.lang.Object"
    }

    "should contain a TYPE node for the type of the local inside the method with the correct props set" in {
      val List(t) = cpg.typ.name("double").l
      t.fullName shouldBe "double"
      t.typeDeclFullName shouldBe "double"
    }

    "should allow traversing from member's TYPE to member" in {
      val List(m) = cpg.typ("long").memberOfType.l
      m.name shouldBe "bar"
      m.typeFullName shouldBe "long"
    }

    "should allow traversing from return params TYPE to return param" in {
      val List(x) = cpg.typ("int").methodReturnOfType.l
      x.typeFullName shouldBe "int"
    }

    "should allow traversing from params TYPE to param" in {
      val List(x) = cpg.typ("java.lang.Object").parameterOfType.l
      x.name shouldBe "x"
    }

    "should allow traversing from local's TYPE to local" in {
      val List(x) = cpg.typ("double").localOfType.l
      x.name shouldBe "l"
    }
  }

  "CPG for code with call to a static method from Java's stdlib with a return type different from its receiver type " should {
    lazy val cpg = code("""
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
