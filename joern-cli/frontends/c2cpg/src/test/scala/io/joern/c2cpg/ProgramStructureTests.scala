package io.joern.c2cpg

import io.joern.c2cpg.testfixtures.CCodeToCpgSuite
import io.shiftleft.semanticcpg.language._
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal

class ProgramStructureTests extends CCodeToCpgSuite {

  "Program structure of test project" should {
    val cpg = code("""
        |struct Foo {
        |  int x, y;
        |};
        |
        |int main(int argc, char **argv) {
        |  return 0;
        |}
        |""".stripMargin)

    "contain <global> namespace block node" in {
      cpg.namespaceBlock.fullNameExact(NamespaceTraversal.globalNamespaceName).size shouldBe 1
    }

    "contain type-decl node" in {
      cpg.typeDecl.size should be > 0
    }

  }

}
