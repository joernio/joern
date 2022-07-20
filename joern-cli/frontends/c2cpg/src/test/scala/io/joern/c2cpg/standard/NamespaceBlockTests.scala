package io.joern.c2cpg.standard

import io.joern.c2cpg.testfixtures.CCodeToCpgSuite
import io.shiftleft.semanticcpg.language._
import io.shiftleft.semanticcpg.language.types.structure.{FileTraversal, NamespaceTraversal}

class NamespaceBlockTests extends CCodeToCpgSuite {

  // We place symbols that can't be associated in a file into the namespace "<global>", and
  // those which can in `filename:<global>`

  private val cpg = code("""
      |int foo() {}
      |struct my_struct{};
      |""".stripMargin)

  "should contain two namespace blocks in total" in {
    cpg.namespaceBlock.size shouldBe 2
  }

  "should contain a correct global namespace block for the `<unknown>` file" in {
    val List(x) = cpg.namespaceBlock.filename(FileTraversal.UNKNOWN).l
    x.name shouldBe NamespaceTraversal.globalNamespaceName
    x.fullName shouldBe NamespaceTraversal.globalNamespaceName
    x.order shouldBe 1
  }

  "should contain correct namespace block for known file" in {
    val List(x) = cpg.namespaceBlock.filenameNot(FileTraversal.UNKNOWN).l
    x.name shouldBe NamespaceTraversal.globalNamespaceName
    x.filename should not be ""
    x.fullName shouldBe s"${x.filename}:${NamespaceTraversal.globalNamespaceName}"
    x.order shouldBe 1
  }

  "should allow traversing from namespace block to method" in {
    cpg.namespaceBlock.filenameNot(FileTraversal.UNKNOWN).ast.isMethod.name.l shouldBe List("<global>", "foo")
  }

  "should allow traversing from namespace block to type declaration" in {
    cpg.namespaceBlock.filenameNot(FileTraversal.UNKNOWN).ast.isTypeDecl.nameNot("<global>").name.l shouldBe List(
      "my_struct"
    )
  }

  "should allow traversing from namespace block to namespace" in {
    cpg.namespaceBlock.filenameNot(FileTraversal.UNKNOWN).namespace.name.l shouldBe List(
      NamespaceTraversal.globalNamespaceName
    )
  }

}
