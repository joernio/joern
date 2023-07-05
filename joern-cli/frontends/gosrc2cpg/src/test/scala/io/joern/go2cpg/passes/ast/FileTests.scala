package io.joern.go2cpg.passes.ast

import io.joern.go2cpg.testfixtures.GoCodeToCpgSuite
import io.shiftleft.semanticcpg.language._
import io.shiftleft.semanticcpg.language.types.structure.FileTraversal
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal

class FileTests extends GoCodeToCpgSuite {
  private val cpg = code("""
      |package main
      |func foo() {}
      |func bar() {}
      |type Sample struct {
      |	name int
      |}
      |""".stripMargin)

  // TODO: Fix this unit test
  "should contain two file nodes in total, both with order=0" ignore {
    cpg.file.order.l shouldBe List(0, 0)
    cpg.file.name(FileTraversal.UNKNOWN).size shouldBe 1
    cpg.file.nameNot(FileTraversal.UNKNOWN).size shouldBe 1
  }

  "should contain exactly one placeholder file node with `name=\"<unknown>\"/order=0`" in {
    cpg.file(FileTraversal.UNKNOWN).order.l shouldBe List(0)
    cpg.file(FileTraversal.UNKNOWN).hash.l shouldBe List()
  }

  "should allow traversing from file to its namespace blocks" in {
    cpg.file.nameNot(FileTraversal.UNKNOWN).namespaceBlock.name.toSetMutable shouldBe Set("main")
  }

  "should allow traversing from file to its methods via namespace block" in {
    cpg.file.nameNot(FileTraversal.UNKNOWN).method.name.toSetMutable shouldBe Set(
      "main." + NamespaceTraversal.globalNamespaceName,
      "foo",
      "bar"
    )
  }

  // TODO: TypeDecl fix the unit test
  "should allow traversing from file to its type declarations via namespace block" ignore {
    cpg.file
      .nameNot(FileTraversal.UNKNOWN)
      .typeDecl
      .name
      .l
      .sorted shouldBe List("main.<global>", "int", "Sample")
  }

  "should allow traversing to namespaces" in {
    val List(ns1, ns2) = cpg.file.namespaceBlock.l
    ns1.filename shouldBe "<unknown>"
    ns1.fullName shouldBe "<global>"
    ns2.filename shouldBe "Test0.go"
    ns2.fullName shouldBe "Test0.go:main"
    cpg.file.namespace.l.size shouldBe 2
  }
}
