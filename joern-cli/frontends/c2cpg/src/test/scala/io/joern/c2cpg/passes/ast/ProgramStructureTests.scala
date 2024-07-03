package io.joern.c2cpg.passes.ast

import io.joern.c2cpg.testfixtures.C2CpgSuite
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal

class ProgramStructureTests extends C2CpgSuite {

  "Program structure" should {

    "be correct for a simple test program" in {
      val cpg = code("""
          |struct Foo {
          |  int x, y;
          |};
          |
          |int main(int argc, char **argv) {
          |  return 0;
          |}
          |""".stripMargin)
      cpg.namespaceBlock.fullNameExact(NamespaceTraversal.globalNamespaceName).size shouldBe 1
      cpg.typeDecl.size should be > 0
    }

    "create one NamespaceBlock per file" in {
      val cpg                        = code("", "foo.c").moreCode("", "woo.c")
      val expectedFilenames          = Seq("foo.c", "woo.c")
      val expectedNamespaceFullNames = expectedFilenames.map(f => s"$f:${NamespaceTraversal.globalNamespaceName}")
      val allNamespaceBlockFullNames = cpg.namespaceBlock.fullNameNot(NamespaceTraversal.globalNamespaceName).fullName.l
      allNamespaceBlockFullNames.zip(expectedNamespaceFullNames).foreach { case (actual, expected) =>
        actual should endWith(expected)
      }
    }
  }

}
