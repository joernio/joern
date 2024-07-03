package io.joern.rubysrc2cpg.deprecated.passes.ast

import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.types.structure.FileTraversal
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal
import io.joern.x2cpg.Defines

class NamespaceBlockTest extends RubyCode2CpgFixture(useDeprecatedFrontend = true) {
  val cpg = code("""puts 123
      |def foo()
      |end
      |class MyClass
      |end
      |""".stripMargin)

  "should contain a correct global namespace block for the `<unknown>` file" in {
    val List(x) = cpg.namespaceBlock.filename(FileTraversal.UNKNOWN).l
    x.name shouldBe NamespaceTraversal.globalNamespaceName
    x.fullName shouldBe NamespaceTraversal.globalNamespaceName
    x.order shouldBe 1
  }

  "should contain correct namespace block for known file" in {
    val List(x) = cpg.namespaceBlock.filenameNot(FileTraversal.UNKNOWN).l
    x.name shouldBe NamespaceTraversal.globalNamespaceName
    x.filename should not be empty
    x.fullName shouldBe s"${x.filename}:${NamespaceTraversal.globalNamespaceName}"
    x.order shouldBe 1
  }

  "should allow traversing from namespace block to method" in {
    cpg.namespaceBlock.filenameNot(FileTraversal.UNKNOWN).ast.isMethod.name.l shouldBe List(
      ":program",
      "foo",
      Defines.ConstructorMethodName
    )
  }

  "should allow traversing from namespace block to type declaration" in {
    cpg.namespaceBlock
      .filenameNot(FileTraversal.UNKNOWN)
      .ast
      .isTypeDecl
      .nameNot(NamespaceTraversal.globalNamespaceName)
      .name
      .l shouldBe List("MyClass")
  }

  "should allow traversing from namespace block to namespace" in {
    cpg.namespaceBlock.filenameNot(FileTraversal.UNKNOWN).namespace.name.l shouldBe List(
      NamespaceTraversal.globalNamespaceName
    )
  }
}
