package io.joern.c2cpg.passes.ast

import io.joern.c2cpg.testfixtures.CCodeToCpgSuite
import io.shiftleft.semanticcpg.language._
import io.shiftleft.semanticcpg.language.types.structure.FileTraversal
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal

class FileTests extends CCodeToCpgSuite {

  private val cpg = code("""
      | int foo() {}
      | int bar() {}
      | struct my_struct { int x; };
      |""".stripMargin)

  "should contain two file nodes in total, both with order=0" in {
    cpg.file.order.l shouldBe List(0, 0)
    cpg.file.name(FileTraversal.UNKNOWN).size shouldBe 1
    cpg.file.nameNot(FileTraversal.UNKNOWN).size shouldBe 1
  }

  "should contain exactly one placeholder file node with `name=\"<unknown>\"/order=0`" in {
    cpg.file(FileTraversal.UNKNOWN).order.l shouldBe List(0)
    cpg.file(FileTraversal.UNKNOWN).hash.l shouldBe List()
  }

  "should allow traversing from file to its namespace blocks" in {
    cpg.file.nameNot(FileTraversal.UNKNOWN).namespaceBlock.name.toSetMutable shouldBe Set(
      NamespaceTraversal.globalNamespaceName
    )
  }

  "should allow traversing from file to its methods via namespace block" in {
    cpg.file.nameNot(FileTraversal.UNKNOWN).method.name.toSetMutable shouldBe Set(
      NamespaceTraversal.globalNamespaceName,
      "foo",
      "bar"
    )
  }

  "should allow traversing from file to its type declarations via namespace block" in {
    cpg.file
      .nameNot(FileTraversal.UNKNOWN)
      .typeDecl
      .nameNot(NamespaceTraversal.globalNamespaceName)
      .name
      .toSetMutable shouldBe Set("my_struct")
  }

  "should allow traversing to namespaces" in {
    cpg.file.namespace.name(NamespaceTraversal.globalNamespaceName).l.size shouldBe 2
  }

}
