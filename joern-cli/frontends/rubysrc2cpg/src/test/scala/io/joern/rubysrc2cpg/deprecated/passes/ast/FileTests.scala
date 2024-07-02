package io.joern.rubysrc2cpg.deprecated.passes.ast

import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.types.structure.FileTraversal
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal

class FileTests extends RubyCode2CpgFixture(useDeprecatedFrontend = true) {
  val cpg = code("""
      |def foo()
      |end
      |def bar()
      |end
      |class MyClass
      |end
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
    cpg.file.nameNot(FileTraversal.UNKNOWN).namespaceBlock.name.toSetMutable shouldBe Set(
      NamespaceTraversal.globalNamespaceName
    )
  }

  "should allow traversing from file to its methods via namespace block" in {
    cpg.file.nameNot(FileTraversal.UNKNOWN).method.name.toSetMutable shouldBe Set("foo", "bar", "<init>", ":program")
  }

  // TODO: TypeDecl fix this unit test
  "should allow traversing from file to its type declarations via namespace block" ignore {
    cpg.file
      .nameNot(FileTraversal.UNKNOWN)
      .typeDecl
      .nameNot(NamespaceTraversal.globalNamespaceName)
      .name
      .l
      .sorted shouldBe List("MyClass")
  }

  // TODO: Need to fix this test.
  "should allow traversing to namespaces" ignore {
    val List(ns1, ns2) = cpg.file.namespaceBlock.l
    // At present it returning full file system path. It should return relative path
    ns1.filename shouldBe "Test0.rb"
    // At present it returning full file system path. It should return relative path
    ns1.fullName shouldBe "Test0.rb:<global>"
    ns2.filename shouldBe "<unknown>"
    ns2.fullName shouldBe "<global>"
    cpg.file.namespace.name(NamespaceTraversal.globalNamespaceName).l.size shouldBe 2
  }
}
