package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.shiftleft.semanticcpg.language._
import io.shiftleft.semanticcpg.language.types.structure.FileTraversal

import java.io.File

class FileTests extends KotlinCode2CpgFixture(withOssDataflow = false) {

  "CPG for code with simple class declaration and one method" should {
    val cpg = code("""
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
      cpg.file.order.l shouldBe List(1, 0)
      cpg.file.name(FileTraversal.UNKNOWN).size shouldBe 1
      cpg.file.nameNot(FileTraversal.UNKNOWN).size shouldBe 1
    }

    "should allow traversing from file to its namespace blocks" in {
      cpg.file.nameNot(FileTraversal.UNKNOWN).namespaceBlock.name.toSet shouldBe Set("bar")
    }

    "should allow traversing from file to its methods" in {
      cpg.file
        .name(".*.kt".replace("/", s"\\${File.separator}"))
        .method
        .name
        .toSet shouldBe Set("<global>", "baz", io.joern.x2cpg.Defines.ConstructorMethodName, "add")
    }

    "should allow traversing from file to its type declarations" in {
      cpg.file.nameNot(FileTraversal.UNKNOWN).typeDecl.name.sorted.toSet shouldBe Set("add", "baz", "Foo", "<global>")
    }
  }
}
