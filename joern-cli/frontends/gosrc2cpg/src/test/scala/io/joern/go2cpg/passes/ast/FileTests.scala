package io.joern.go2cpg.passes.ast

import io.joern.go2cpg.testfixtures.GoCodeToCpgSuite
import io.shiftleft.semanticcpg.language._
import io.shiftleft.semanticcpg.language.types.structure.FileTraversal
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal
import java.io.File

class FileTests extends GoCodeToCpgSuite {
  "be correct for single file nodes" should {
    val cpg = code("""
        |package main
        |func foo() {}
        |func bar() {}
        |type Sample struct {
        |	name int
        |}
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
      cpg.file.nameNot(FileTraversal.UNKNOWN).namespaceBlock.name.toSetMutable shouldBe Set("main")
    }

    "should allow traversing from file to its methods via namespace block" in {
      cpg.file.nameNot(FileTraversal.UNKNOWN).method.name.toSetMutable shouldBe Set(
        s"main.${NamespaceTraversal.globalNamespaceName}",
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
      ns1.filename shouldBe FileTraversal.UNKNOWN
      ns1.fullName shouldBe NamespaceTraversal.globalNamespaceName
      ns2.filename shouldBe "Test0.go"
      ns2.fullName shouldBe "Test0.go:main"
      cpg.file.namespace.l.size shouldBe 2
    }
  }

  "be correct for multiple files" should {
    val cpg = code(
      """
        |module joern.io/sample
        |go 1.18
        |""".stripMargin,
      "go.mod"
    ).moreCode(
      """
        |package fpkg
        |type Person struct {
        |  name string
        |}
        |""".stripMargin,
      Seq("fpkg", "mainlib.go").mkString(File.separator)
    ).moreCode(
      """
        |package main
        |import "joern.io/sample/fpkg"
        |func foo() {
        |  var c = fpkg.Person{"FirstName"}
        |}
        |""".stripMargin,
      "main.go"
    )

    "should contain two file nodes in total, both with order=0" in {
      cpg.file.order.l shouldBe List(0, 0, 0)
      cpg.file.name(FileTraversal.UNKNOWN).size shouldBe 1
      cpg.file.nameNot(FileTraversal.UNKNOWN).size shouldBe 2
    }

    "traversal from file to typedecl should work" in {
      cpg.file(".*mainlib.go").size shouldBe 1
      cpg.file(".*mainlib.go").typeDecl.fullName.l shouldBe List(
        s"${Seq("fpkg", "mainlib.go").mkString(File.separator)}:joern.io/sample/fpkg.<global>",
        "joern.io/sample/fpkg.Person"
      )
    }

    "traversal from file to method should work" in {
      cpg.file("main.go").size shouldBe 1
      cpg.file("main.go").method.fullName.l shouldBe List("main.go:main.<global>", "main.foo")
    }
  }
}
