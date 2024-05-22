package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.nodes.{Block, Call, Return}
import io.shiftleft.semanticcpg.language._

class MethodTests extends KotlinCode2CpgFixture(withOssDataflow = false) {
  "CPG for code with simple method defined at package-level" should {
    val cpg = code("""
       |fun double(x: Int): Int {
       |  return x * 2
       |}
       |
       |fun main(args : Array<String>) {
       |  println("The double of 2 is: " + double(2))
       |}
       |""".stripMargin)

    "should contain exactly three non-external methods" in {
      cpg.method.isExternal(false).size shouldBe 3
    }

    "should contain method nodes with the correct fields" in {
      val List(x) = cpg.method.name("double").isExternal(false).l
      x.fullName shouldBe "double:int(int)"
      x.code shouldBe "double"
      x.signature shouldBe "int(int)"
      x.isExternal shouldBe false
      x.lineNumber shouldBe Some(2)
      x.columnNumber shouldBe Some(4)
      x.filename.endsWith(".kt") shouldBe true

      val List(y) = cpg.method.name("main").isExternal(false).l
      y.fullName shouldBe "main:void(kotlin.Array)"
      y.code shouldBe "main"
      y.signature shouldBe "void(kotlin.Array)"
      y.isExternal shouldBe false
      y.lineNumber shouldBe Some(6)
      x.columnNumber shouldBe Some(4)
      y.filename.endsWith(".kt") shouldBe true
    }

    "should contain MODIFIER nodes attached to the METHOD nodes" in {
      val List(mod1) = cpg.method.nameExact("double").modifier.l
      mod1.modifierType shouldBe "PUBLIC"

      val List(mod2) = cpg.method.nameExact("main").modifier.l
      mod2.modifierType shouldBe "PUBLIC"
    }

    "should allow traversing to parameters" in {
      cpg.method.name("double").isExternal(false).parameter.name.toSet shouldBe Set("x")
      cpg.method.name("main").isExternal(false).parameter.name.toSet shouldBe Set("args")
    }

    "should allow traversing to methodReturn" in {
      cpg.method.name("double").isExternal(false).methodReturn.typeFullName.l shouldBe List("int")
      cpg.method.name("main").isExternal(false).methodReturn.typeFullName.l shouldBe List("void")
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

  "CPG for code with simple class declaration" should {
    val cpg = code("""
        |package com.test.pkg
        |
        |class Foo {
        |  fun bar(x: Int): Int {
        |    return x * 2
        |  }
        |}
        |""".stripMargin)

    "should contain a METHOD node for `bar` with the props set" in {
      val List(m) = cpg.method.name("bar").l
      m.name shouldBe "bar"
      m.fullName shouldBe "com.test.pkg.Foo.bar:int(int)"
      m.code shouldBe "bar"
      m.signature shouldBe "int(int)"
      m.isExternal shouldBe false
      m.lineNumber shouldBe Some(5)
      m.columnNumber shouldBe Some(6)
      m.lineNumberEnd shouldBe Some(7)
      m.columnNumberEnd shouldBe Some(2)
      m.order shouldBe 1
      m.filename.endsWith(".kt") shouldBe true
    }

    "should allow traversing to parameters" in {
      cpg.method.name("bar").parameter.name.toSet shouldBe Set("this", "x")
    }

    "should allow traversing to methodReturn" in {
      cpg.method.name("bar").methodReturn.l.size shouldBe 1
    }

    "should allow traversing to file" in {
      cpg.method.name("bar").file.name.l should not be empty
    }

    "should allow traversing to block" in {
      cpg.method.name("bar").block.l should not be empty
    }
  }

  "CPG for code with method without a body-block" should {
    val cpg = code("fun printX(x: String) = println(x)")

    "should contain a METHOD node with one expression in its corresponding BLOCK" in {
      cpg.method.nameExact("printX").block.expressionDown.size shouldBe 1
    }
  }

  "CPG for code with method with single-expression body" should {
    val cpg = code("""
        |package main
        |class AClass(var x: String)
        |fun f1(p: String): AClass = AClass(p ?: "message")
        ||""".stripMargin)

    "should contain a RETURN node as the child of the METHOD's BLOCK" in {
      val List(m)         = cpg.method.nameExact("f1").l
      val List(r: Return) = m.block.astChildren.l: @unchecked
      val List(_: Block)  = r.astChildren.l: @unchecked
    }
  }

  "CPG for code with call with argument with type with upper bound" should {
    val cpg = code("""
      |package mypkg
      |open class Base
      |fun <S:Base>doSomething(one: S) {
      |    println(one)
      |}
      |""".stripMargin)

    "should contain a METHOD node with correct FULL_NAME set" in {
      val List(m) = cpg.method.nameExact("doSomething").l
      m.fullName shouldBe "mypkg.doSomething:void(mypkg.Base)"
    }
  }

  "a higher-function defined from a closure" should {
    val cpg = code("""
        |class Foo {
        |    fun Collection<ByteArray>.sorted(): List<ByteArray> = sortedWith { a, b ->
        |        operator fun ByteArray.compareTo(other: ByteArray): Int {
        |            var result: Int? = null
        |            val minSize = kotlin.math.min(this.size, other.size)
        |            for (index in 0 until minSize) {
        |                val thisByte = this[index]
        |                val otherByte = other[index]
        |                val comparedResult = thisByte.compareTo(otherByte)
        |                if (comparedResult != 0 && result == null) {
        |                    result = comparedResult
        |                }
        |            }
        |
        |            return result ?: this.size.compareTo(other.size)
        |        }
        |
        |        return a.compareTo(b)
        |    }
        |}
        |""".stripMargin)

    "pass the lambda to a `sortedWith` call which is then under the method `sorted`" in {
      inside(cpg.methodRef(".*<lambda>.*").inCall.l) {
        case sortedWith :: Nil =>
          sortedWith.name shouldBe "sortedWith"
          sortedWith.method.name shouldBe "sorted"
        case xs => fail(s"Expected a single call with the method reference argument. Instead got [$xs]")
      }
    }
  }
}
