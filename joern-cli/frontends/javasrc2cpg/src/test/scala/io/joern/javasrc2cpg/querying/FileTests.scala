package io.joern.javasrc2cpg.querying

import io.joern.javasrc2cpg.testfixtures.JavaSrcCode2CpgFixture
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.types.structure.FileTraversal
import org.scalatest.Ignore

import java.io.{File => JFile}
import java.io.File
import java.nio.file.Paths

class FileTests extends JavaSrcCode2CpgFixture {

  "a cpg generated without ignored files" should {
    val fileName = Paths.get("a", "b", "Foo.java").toString

    val cpg = code(
      """
        | package a.b;
        | class Foo { int bar() { return 1; } }
        |""".stripMargin,
      fileName = fileName
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
        foo.name shouldBe fileName
        foo.hash.isDefined shouldBe false
      }
    }
  }

  ".git, .mvn, and test directories should be ignored by default" in {
    val includedFileName = Paths.get("a", "b", "Foo.java").toString()

    val ignoredFileNames = List(
      Paths.get(".git", "Foo.java"),
      Paths.get(".mvn", "Foo.java"),
      Paths.get("test", "Foo.java"),
      Paths.get("a", "test", "Foo.java")
    ).map(_.toString())

    val cpg = code("package a.b;", fileName = includedFileName)

    ignoredFileNames.foreach { case fileName =>
      cpg.moreCode("package ignored;", fileName)
    }

    cpg.file.name.toSet shouldBe Set(includedFileName, "<unknown>")
  }
}
