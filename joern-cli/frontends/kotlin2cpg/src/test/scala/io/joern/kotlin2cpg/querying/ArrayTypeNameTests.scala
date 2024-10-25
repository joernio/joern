package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class ArrayTypeNameTests extends KotlinCode2CpgFixture(withOssDataflow = true) {
  "test array type full name in method signature" in {
    val cpg = code("""
                     |package mypkg
                     |fun method(param: Array<String>) {}
                     |""".stripMargin)

    val List(method) = cpg.method.name("method").l
    method.signature shouldBe "void(java.lang.String[])"
  }

  "test array type full name in method signature for nested array" in {
    val cpg = code("""
                     |package mypkg
                     |fun method(param: Array<Array<String>>) {}
                     |""".stripMargin)

    val List(method) = cpg.method.name("method").l
    method.signature shouldBe "void(java.lang.String[][])"
  }

  "test array type full name in method signature for builtin type array" in {
    val cpg = code("""
                     |package mypkg
                     |fun method(param: ByteArray) {}
                     |""".stripMargin)

    val List(method) = cpg.method.name("method").l
    method.signature shouldBe "void(byte[])"
  }

  "test array type full name in method signature for nested builtin type array" in {
    val cpg = code("""
                     |package mypkg
                     |fun method(param: Array<ByteArray>) {}
                     |""".stripMargin)

    val List(method) = cpg.method.name("method").l
    method.signature shouldBe "void(byte[][])"
  }

  "test array type full name in method signature for array of kotlin type" in {
    val cpg = code("""
                     |package mypkg
                     |fun method(param: Array<kotlin.Boolean>) {}
                     |""".stripMargin)

    val List(method) = cpg.method.name("method").l
    method.signature shouldBe "void(boolean[])"
  }
}
