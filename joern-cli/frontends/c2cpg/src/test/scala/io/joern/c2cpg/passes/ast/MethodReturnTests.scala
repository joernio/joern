package io.joern.c2cpg.passes.ast

import io.joern.c2cpg.testfixtures.C2CpgSuite
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal

class MethodReturnTests extends C2CpgSuite {

  "MethodReturnTest1" should {
    val cpg = code("""
        | int *foo() { return x; }
        |""".stripMargin)

    "should have METHOD_RETURN node with correct fields" in {
      val List(x, _, _) = cpg.methodReturn.l
      x.code shouldBe "RET"
      x.typeFullName shouldBe "int*"
      x.lineNumber shouldBe Option(2)
      x.columnNumber shouldBe Option(2)
      // we expect the METHOD_RETURN node to be the right-most
      // child so that when traversing the AST from left to
      // right in CFG construction, we visit it last.
      x.order shouldBe 2
    }

    "should allow traversing to method" in {
      cpg.methodReturn.method.name.l shouldBe List(
        "foo",
        NamespaceTraversal.globalNamespaceName,
        NamespaceTraversal.globalNamespaceName
      )
    }

  }

  "MethodReturnTest2" should {
    val cpg = code("""
        |int main(int argc, char *argv[]) {
        |  if (argc == 1) {
        |    return 1;
        |  }
        |  return 2;
        |}
        |""".stripMargin)

    "be correct for multiple returns" in {
      // synthetic method returns; these do not represent the actual return statements from C/C++!
      inside(cpg.method("main").methodReturn.l) { case List(mainMethodReturn) =>
        mainMethodReturn.code shouldBe "RET"
        mainMethodReturn.typeFullName shouldBe "int"
      }
      val astReturns  = cpg.method("main").ast.isReturn.l
      val cfgReturns  = cpg.method("main").methodReturn.cfgPrev.l
      val travReturns = cpg.method("main").methodReturn.toReturn.l
      inside(astReturns) { case List(ret1, ret2) =>
        ret1.code shouldBe "return 1;"
        ret1.lineNumber shouldBe Option(4)
        ret1.columnNumber shouldBe Option(5)
        ret2.code shouldBe "return 2;"
        ret2.lineNumber shouldBe Option(6)
        ret2.columnNumber shouldBe Option(3)
      }
      astReturns shouldBe cfgReturns
      astReturns shouldBe travReturns
    }
  }
}
