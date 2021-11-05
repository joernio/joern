package io.joern.jimple2cpg.querying

import io.joern.jimple2cpg.testfixtures.JimpleCodeToCpgFixture
import io.shiftleft.semanticcpg.language._
import io.shiftleft.semanticcpg.language.types.structure.FileTraversal
import org.scalatest.Ignore

import java.io.{File => JFile}

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

  "should contain exactly two non-placeholder file with absolute path in `name`" in {
    val List(u, v) = cpg.file.nameNot(FileTraversal.UNKNOWN).l
    u.name should startWith(JFile.separator)
    u.hash.isDefined shouldBe false
    v.name should startWith(JFile.separator)
    v.hash.isDefined shouldBe false
  }

  "should allow traversing from file to its namespace blocks" in {
    cpg.file.nameNot(FileTraversal.UNKNOWN).namespaceBlock.name.toSet shouldBe Set(
      "b"
    )
  }

  "should allow traversing from file to its methods via namespace block" in {
    cpg.file
      .name(".*.class".replace("/", s"\\${JFile.separator}"))
      .method
      .name
      .toSet shouldBe Set("bar", "<init>")
  }

  "should allow traversing from file to its type declarations via namespace block" in {
    cpg.file.nameNot(FileTraversal.UNKNOWN).typeDecl.name.toSet shouldBe Set(
      "Foo"
    )
  }

}
