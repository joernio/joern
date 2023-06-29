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

  "should allow traversing from namespace block to method" in {
    cpg.namespaceBlock.filenameNot(FileTraversal.UNKNOWN).ast.isMethod.fullName.l shouldBe List(s"Test0.go:main")
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
}
