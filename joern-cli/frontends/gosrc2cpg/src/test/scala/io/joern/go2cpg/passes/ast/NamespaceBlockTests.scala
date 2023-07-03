package io.joern.go2cpg.passes.ast

import io.joern.go2cpg.testfixtures.GoCodeToCpgSuite
import io.shiftleft.semanticcpg.language._
import io.shiftleft.semanticcpg.language.types.structure.FileTraversal
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal

class NamespaceBlockTests extends GoCodeToCpgSuite {
  private val cpg = code("""
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
    x.fullName shouldBe s"Test0.go:main"
    x.order shouldBe 1
  }

  // TODO: Once the method node creation is done, this unit test needs to be fixed to add fullname for "func foo()" as well, along with dummy method representing the file.
  "should allow traversing from namespace block to method" ignore {
    cpg.namespaceBlock.filenameNot(FileTraversal.UNKNOWN).ast.isMethod.fullName.l shouldBe List("Test0.go:main")
  }

  "should allow traversing from namespace block to type declaration" in {
    cpg.namespaceBlock
      .filenameNot(FileTraversal.UNKNOWN)
      .ast
      .isTypeDecl
      .nameNot(NamespaceTraversal.globalNamespaceName)
      .fullName
      .l
      .sorted shouldBe List(s"Test0.go:main")
  }

  "should allow traversing from namespace block to namespace" in {
    cpg.namespaceBlock.filenameNot(FileTraversal.UNKNOWN).namespace.name.l shouldBe List("main")
  }

  "create one NamespaceBlock per file" in {
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
    val expectedFilenames          = Seq("foo.go", "woo.go")
    val expectedNamespaceFullNames = expectedFilenames.map(f => s"$f:main")
    val allNamespaceBlockFullNames = cpg.namespaceBlock.fullNameNot(NamespaceTraversal.globalNamespaceName).fullName.l
    allNamespaceBlockFullNames.zip(expectedNamespaceFullNames).foreach { case (actual, expected) =>
      actual should endWith(expected)
    }
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
