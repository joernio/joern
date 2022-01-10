package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.Kt2CpgTestContext
import io.shiftleft.semanticcpg.language._
import io.shiftleft.semanticcpg.language.types.structure.FileTraversal

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import java.io.{File => JFile}

class FileTests extends AnyFreeSpec with Matchers {

  lazy val cpg = Kt2CpgTestContext.buildCpg("""
      |package mypkg.bar
      |
      |class Foo {
      |  fun baz(x: Int): Int {
      |   return x * 2
      |  }
      |}
      |
      |fun add(x: Int, y: Int): Int {
      |  return x + y
      |}
      |""".stripMargin)

  "should contain two file nodes with order set" in {
    cpg.file.order.l shouldBe List(0, 0)
    cpg.file.name(FileTraversal.UNKNOWN).size shouldBe 1
    cpg.file.nameNot(FileTraversal.UNKNOWN).size shouldBe 1
  }

  "should allow traversing from file to its namespace blocks" in {
    cpg.file.nameNot(FileTraversal.UNKNOWN).namespaceBlock.name.toSet shouldBe Set(
      "bar"
    )
  }

  "should allow traversing from file to its methods" in {
    cpg.file
      .name(".*.kt".replace("/", s"\\${JFile.separator}"))
      .method
      .name
      .toSet shouldBe Set("baz", "Foo", "add")
  }

  "should allow traversing from file to its type declarations" in {
    cpg.file.nameNot(FileTraversal.UNKNOWN).typeDecl.name.toSet shouldBe Set(
      "Foo"
    )
  }
}
