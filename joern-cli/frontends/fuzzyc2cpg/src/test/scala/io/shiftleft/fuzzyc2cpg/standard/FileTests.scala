package io.shiftleft.fuzzyc2cpg.standard

import io.shiftleft.fuzzyc2cpg.testfixtures.FuzzyCCodeToCpgSuite
import io.shiftleft.semanticcpg.language._
import io.shiftleft.semanticcpg.language.types.structure.FileTraversal

class FileTests extends FuzzyCCodeToCpgSuite {

  override val code: String =
    """
      | int foo() {}
      | int bar() {}
      | struct my_struct { int x; };
      |""".stripMargin

  "should contain two file nodes in total, both with order=0" in {
    cpg.file.order.l shouldBe List(0, 0)
    cpg.file.name(FileTraversal.UNKNOWN).size shouldBe 1
    cpg.file.nameNot(FileTraversal.UNKNOWN).size shouldBe 1
  }

  "should contain exactly one placeholder file node with `name=\"<unknown>\"/order=0`" in {
    cpg.file(FileTraversal.UNKNOWN).order.l shouldBe List(0)
    cpg.file(FileTraversal.UNKNOWN).hash.l shouldBe List()
  }

  "should contain exactly one non-placeholder file with absolute path in `name`" in {
    val List(x) = cpg.file.nameNot(FileTraversal.UNKNOWN).l
    x.name should startWith("/")
    // C-frontend currently does not set hash but should do so
    // in the future
    x.hash shouldBe None
  }

  "should allow traversing from file to its namespace blocks" in {
    cpg.file.nameNot(FileTraversal.UNKNOWN).namespaceBlock.name.toSet shouldBe Set("<global>")
  }

  "should allow traversing from file to its methods via namespace block" in {
    cpg.file.nameNot(FileTraversal.UNKNOWN).method.name.toSet shouldBe Set("foo", "bar")
  }

  "should allow traversing from file to its type declarations via namespace block" in {
    cpg.file.nameNot(FileTraversal.UNKNOWN).typeDecl.name.toSet shouldBe Set("my_struct")
  }

  "should allow traversing to namespaces" in {
    cpg.file.namespace.name("<global>").l.size shouldBe 2
  }

}
