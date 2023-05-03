package io.joern.javasrc2cpg.querying

import io.joern.javasrc2cpg.testfixtures.JavaSrcCode2CpgFixture
import io.shiftleft.semanticcpg.language._
import io.shiftleft.semanticcpg.language.types.structure.FileTraversal
import org.scalatest.Ignore

import java.io.{File => JFile}
import java.io.File

class FileTests extends JavaSrcCode2CpgFixture {

  val cpg = code(
    """
      | package a.b;
      | class Foo { int bar() { return 1; } }
      |""".stripMargin,
    fileName = "a/b/Foo.java"
  )

  "should contain two file nodes in total with correct order" in {
    cpg.file.order.l shouldBe List(0, 0)
    cpg.file.name(FileTraversal.UNKNOWN).size shouldBe 1
    cpg.file.nameNot(FileTraversal.UNKNOWN).size shouldBe 1
  }

  "should allow traversing from file to its namespace blocks" in {
    cpg.file.nameNot(FileTraversal.UNKNOWN).namespaceBlock.name.toSetMutable shouldBe Set("a.b")
  }

  "should allow traversing from file to its methods via namespace block" in {
    cpg.file
      .name(".*.java".replace("/", s"\\${JFile.separator}"))
      .method
      .name
      .toSetMutable shouldBe Set("bar", io.joern.x2cpg.Defines.ConstructorMethodName)
  }

  "should allow traversing from file to its type declarations via namespace block" in {
    cpg.file.nameNot(FileTraversal.UNKNOWN).typeDecl.name.toSetMutable shouldBe Set("Foo")
  }

  "the filename should be the path relative to the project root" in {
    inside(cpg.file.nameNot(FileTraversal.UNKNOWN).l) { case List(foo) =>
      foo.name shouldBe "a/b/Foo.java"
      foo.hash.isDefined shouldBe false
    }
  }
}
