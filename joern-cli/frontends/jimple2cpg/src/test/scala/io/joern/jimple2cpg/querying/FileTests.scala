package io.joern.jimple2cpg.querying

import io.joern.jimple2cpg.testfixtures.JimpleCodeToCpgFixture
import io.shiftleft.semanticcpg.language._
import io.shiftleft.semanticcpg.language.types.structure.FileTraversal

import java.io.{File => JFile}
import java.io.File

class FileTests extends JimpleCodeToCpgFixture {

  override val code: String =
    """
      | package a.b;
      | class Foo { int bar() { return 1; } }
      |""".stripMargin

  "should contain two file nodes in total with correct order" in {
    cpg.file.order.l shouldBe List(0, 0)
    cpg.file.name(FileTraversal.UNKNOWN).size shouldBe 1
    cpg.file.nameNot(FileTraversal.UNKNOWN).size shouldBe 1
  }

  "should contain exactly one non-placeholder file with absolute path in `name`" in {
    val List(x) = cpg.file.nameNot(FileTraversal.UNKNOWN).l
    x.name should (
      startWith(File.separator) or // Unix
        startWith regex "[A-Z]:"   // Windows
    )
    x.hash.isDefined shouldBe false
  }

  "should allow traversing from file to its namespace blocks" in {
    cpg.file.nameNot(FileTraversal.UNKNOWN).namespaceBlock.name.toSetMutable shouldBe Set("b")
  }

  "should allow traversing from file to its methods via namespace block" in {
    cpg.file
      .name(".*.class".replace("/", s"\\${JFile.separator}"))
      .method
      .name
      .toSetMutable shouldBe Set("bar", "<init>")
  }

  "should allow traversing from file to its type declarations via namespace block" in {
    cpg.file.nameNot(FileTraversal.UNKNOWN).typeDecl.name.toSetMutable shouldBe Set("Foo")
  }

}
