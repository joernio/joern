package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.Kt2CpgTestContext
import io.shiftleft.semanticcpg.language._

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class NamespaceBlockTests extends AnyFreeSpec with Matchers {

  lazy val cpg = Kt2CpgTestContext.buildCpg("""
      |package com.test.PackageFoo
      |
      |class ClassFoo {
      |  fun methodFoo(x: Int) {
      |    return x * 2
      |  }
      |}
      |
      |fun add(x: Int, y: Int): Int {
      |  return x + y
      |}
      |""".stripMargin)

  "should contain two namespace blocks in total (<global>, PackageFoo)" in {
    cpg.namespaceBlock.size shouldBe 2
    cpg.namespaceBlock.name.l.toSet shouldBe Set("<global>", "PackageFoo")
  }

  "should contain correct namespace block for known file" in {
    val List(x) = cpg.namespaceBlock.filename(".*.kt").l
    x.name shouldBe "PackageFoo"
    x.filename should not be ""
    x.fullName shouldBe s"com.test.PackageFoo"
    x.order shouldBe 1
  }

  "should allow traversing from namespace block to namespace" in {
    cpg.namespaceBlock.filename(".*kt").namespace.name.l shouldBe List("PackageFoo")
  }

  "should allow traversing from namespace block to type declaration" in {
    cpg.namespaceBlock.filename(".*kt").typeDecl.name.l shouldBe List("ClassFoo")
  }

  "should allow traversing from namespace block to file" in {
    cpg.namespaceBlock.filename(".*kt").file.size shouldBe 1
  }

  "should allow traversing from namespace block to method" in {
    cpg.namespaceBlock.filename(".*kt").method.size shouldBe 1
  }
}
