package io.joern.go2cpg.passes.ast

import io.joern.go2cpg.testfixtures.GoCodeToCpgSuite
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.types.structure.FileTraversal
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal

class NamespaceBlockTests extends GoCodeToCpgSuite {

  "Simple use case to check NameSpaceBlock node and its childs" should {
    val cpg = code("""
        |package main
        |func foo() {}
        |""".stripMargin)

    "should contain a correct global namespace block for the `<unknown>` file" in {
      val List(x) = cpg.namespaceBlock.filename(FileTraversal.UNKNOWN).l
      x.name shouldBe NamespaceTraversal.globalNamespaceName
      x.fullName shouldBe NamespaceTraversal.globalNamespaceName
      x.order shouldBe 1
    }

    "should contain correct namespace block for known file" in {
      val List(x) = cpg.namespaceBlock.filenameNot(FileTraversal.UNKNOWN).l
      x.name shouldBe "main"
      x.filename should not be empty
      x.fullName shouldBe "main"
      x.order shouldBe 1
    }

    "should allow traversing from namespace block to method" in {
      cpg.namespaceBlock.filenameNot(FileTraversal.UNKNOWN).ast.isMethod.fullName.l shouldBe List(
        "main.foo",
        "Test0.go:main.Test0.go"
      )
    }

    "should allow traversing from namespace block to type declaration" in {
      cpg.namespaceBlock
        .filenameNot(FileTraversal.UNKNOWN)
        .ast
        .isTypeDecl
        .nameNot(NamespaceTraversal.globalNamespaceName)
        .fullName
        .l
        .sorted shouldBe List("main")
    }

    "should allow traversing from namespace block to namespace" in {
      cpg.namespaceBlock.filenameNot(FileTraversal.UNKNOWN).namespace.name.l shouldBe List("main")
    }

    "Traversal from Package Type Decl to its child methods" in {
      cpg.typeDecl("main").method.fullName.l shouldBe List("main.foo", "Test0.go:main.Test0.go")
    }
  }

  "create one NamespaceBlock per package" in {
    val cpg = code(
      """
        |package main
        |func foo() {}
        |""".stripMargin,
      "foo.go"
    ).moreCode(
      """
        |package main
        |func woo() {}
        |""".stripMargin,
      "woo.go"
    )
    val List(expected) = cpg.namespaceBlock.fullNameNot(NamespaceTraversal.globalNamespaceName).fullName.l
    expected shouldBe "main"
  }

  "Simple sample with go.mod and main package" in {
    val cpg = code(
      """
        |module joern.io/sample
        |go 1.18
        |""".stripMargin,
      "go.mod"
    ).moreCode(
      """
        |package main
        |func woo() {}
        |""".stripMargin,
      "woo.go"
    )
    cpg.namespaceBlock.filenameNot(FileTraversal.UNKNOWN).namespace.name.l shouldBe List("main")
  }

  "Simple sample with go.mod and matching package" in {
    val cpg = code(
      """
        |module joern.io/sample
        |go 1.18
        |""".stripMargin,
      "go.mod"
    ).moreCode(
      """
        |package sample
        |func woo() {}
        |""".stripMargin,
      "woo.go"
    )
    cpg.namespaceBlock.filenameNot(FileTraversal.UNKNOWN).namespace.name.l shouldBe List("joern.io/sample")
  }
}
