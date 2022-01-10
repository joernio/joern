package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.Kt2CpgTestContext
import io.shiftleft.semanticcpg.language._

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class TypeTests extends AnyFreeSpec with Matchers {

  "CPG for code with simple class" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg(
      """
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
        |""".stripMargin
    )

    "should create TYPE node with correct fields for return type" in {
      val List(x) = cpg.typ.name("Int").l
      x.name shouldBe "Int"
      x.fullName shouldBe "kotlin.Int"
      x.typeDeclFullName shouldBe "kotlin.Int"
    }

    "should create TYPE node with correct fields for class member" in {
      val List(x) = cpg.typ.name("Long").l
      x.name shouldBe "Long"
      x.fullName shouldBe "kotlin.Long"
      x.typeDeclFullName shouldBe "kotlin.Long"
    }

    "should create TYPE node with correct fields for parameter type" in {
      val List(x) = cpg.typ.name("Any").l
      x.name shouldBe "Any"
      x.fullName shouldBe "kotlin.Any"
      x.typeDeclFullName shouldBe "kotlin.Any"
    }

    "should create TYPE node with correct fields for local type" in {
      val List(x) = cpg.typ.name("Double").l
      x.name shouldBe "Double"
      x.fullName shouldBe "kotlin.Double"
      x.typeDeclFullName shouldBe "kotlin.Double"
    }

    "should allow traversing from member's TYPE to member" in {
      val List(x) = cpg.typ("kotlin.Long").memberOfType.l
      x.name shouldBe "bar"
    }

    "should allow traversing from return params TYPE to return param" in {
      val List(x) = cpg.typ("kotlin.Int").methodReturnOfType.l
      x.typeFullName shouldBe "kotlin.Int"
    }

    "should allow traversing from params TYPE to param" in {
      val List(x) = cpg.typ("kotlin.Any").parameterOfType.l
      x.name shouldBe "x"
    }

    "should allow traversing from local's TYPE to local" in {
      val List(x) = cpg.typ("kotlin.Double").localOfType.l
      x.name shouldBe "l"
    }
  }

  "CPG for code with Android SDK fn" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg(
      """
        |package mypkg
        |
        |import android.util.Log
        |
        |fun mine() {
        |  Log.d("foo", "bar")
        |}
        |""".stripMargin
    )

    "should have type for Log" in {
      val List(x) = cpg.typ.typeDeclFullName(".*Log.*").l
      x.typeDeclFullName shouldBe "android.util.Log"
    }
  }
}
