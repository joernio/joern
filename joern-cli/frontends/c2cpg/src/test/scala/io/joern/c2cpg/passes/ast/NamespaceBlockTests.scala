package io.joern.c2cpg.passes.ast

import io.joern.c2cpg.testfixtures.C2CpgSuite
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.types.structure.FileTraversal
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal

class NamespaceBlockTests extends C2CpgSuite {

  // We place symbols that can't be associated in a file into the namespace "<global>", and
  // those which can in `filename:<global>`

  private val cpg = code("""
      |int foo() {}
      |struct my_struct{};
      |""".stripMargin)

  "should contain three namespace blocks in total" in {
    cpg.namespaceBlock.size shouldBe 3
  }

  "should contain a correct global namespace block for the `<unknown>` file" in {
    val List(x) = cpg.namespaceBlock.filename(FileTraversal.UNKNOWN).l
    x.name shouldBe NamespaceTraversal.globalNamespaceName
    x.fullName shouldBe NamespaceTraversal.globalNamespaceName
    x.order shouldBe 1
  }

  "should contain correct namespace block for known file" in {
    val List(x) = cpg.namespaceBlock.filenameNot(FileTraversal.UNKNOWN, "<includes>").l
    x.name shouldBe NamespaceTraversal.globalNamespaceName
    x.filename should not be empty
    x.fullName shouldBe s"${x.filename}:${NamespaceTraversal.globalNamespaceName}"
    x.order shouldBe 1
  }

  "should allow traversing from namespace block to method" in {
    cpg.namespaceBlock.filenameNot(FileTraversal.UNKNOWN, "<includes>").ast.isMethod.name.l shouldBe List(
      NamespaceTraversal.globalNamespaceName,
      "foo"
    )
  }

  "should allow traversing from namespace block to type declaration" in {
    cpg.namespaceBlock
      .filenameNot(FileTraversal.UNKNOWN)
      .ast
      .isTypeDecl
      .nameNot(NamespaceTraversal.globalNamespaceName)
      .name
      .l
      .sorted shouldBe List("ANY", "int", "my_struct", "void")
  }

  "should allow traversing from namespace block to namespace" in {
    cpg.namespaceBlock.filenameNot(FileTraversal.UNKNOWN, "<includes>").namespace.name.l shouldBe List(
      NamespaceTraversal.globalNamespaceName
    )
  }

}
