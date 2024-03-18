package io.joern.c2cpg.passes.ast

import io.joern.c2cpg.testfixtures.C2CpgSuite
import io.shiftleft.semanticcpg.language._
import io.shiftleft.semanticcpg.language.types.structure.FileTraversal
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal

class FileTests extends C2CpgSuite {

  private val cpg = code("""
      | int foo() {}
      | int bar() {}
      | struct my_struct { int x; };
      |""".stripMargin)

  "should contain the correct file nodes" in {
    val List(fileTest, fileUnknown) = cpg.file.nameNot("<includes>").l
    fileTest.name shouldBe "Test0.c"
    fileTest.order shouldBe 0
    fileUnknown.name shouldBe FileTraversal.UNKNOWN
    fileUnknown.order shouldBe 0
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
      .l
      .sorted shouldBe List("ANY", "int", "my_struct", "void")
  }

  "should allow traversing to namespaces" in {
    val List(ns1, ns2, ns3) = cpg.file.namespaceBlock.l
    ns1.filename shouldBe "Test0.c"
    ns1.fullName shouldBe "Test0.c:<global>"
    ns2.filename shouldBe "<includes>"
    ns2.fullName shouldBe "<includes>:<global>"
    ns3.filename shouldBe "<unknown>"
    ns3.fullName shouldBe "<global>"
    cpg.file.namespace.name(NamespaceTraversal.globalNamespaceName).l.size shouldBe 3
  }

}
