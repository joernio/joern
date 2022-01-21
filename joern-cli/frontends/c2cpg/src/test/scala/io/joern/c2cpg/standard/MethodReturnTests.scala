package io.joern.c2cpg.standard

import io.joern.c2cpg.testfixtures.CCodeToCpgSuite
import io.shiftleft.semanticcpg.language._

class MethodReturnTest1 extends CCodeToCpgSuite {

  override val code: String =
    """
      | int *foo() { return x; }
      |""".stripMargin

  "should have METHOD_RETURN node with correct fields" in {
    val List(x, _) = cpg.methodReturn.l
    x.code shouldBe "int*"
    x.typeFullName shouldBe "int*"
    x.lineNumber shouldBe Some(2)
    x.columnNumber shouldBe Some(1)
    // we expect the METHOD_RETURN node to be the right-most
    // child so that when traversing the AST from left to
    // right in CFG construction, we visit it last.
    x.order shouldBe 2
  }

  "should allow traversing to method" in {
    cpg.methodReturn.method.name.l shouldBe List("foo", "<global>")
  }

}

class MethodReturnTest2 extends CCodeToCpgSuite {

  override val code: String =
    """
     |int main(int argc, char *argv[]) {
     |  if (argc == 1) {
     |    return 1;
     |  }
     |  return 2;
     |}
     |""".stripMargin

  "be correct for multiple returns" in {
    // synthetic method returns; these do not represent the actual return statements from C/C++!
    inside(cpg.methodReturn.l) { case List(ret1, ret2, _) =>
      // method return from main
      ret1.code shouldBe "RET"
      ret1.typeFullName shouldBe "int"
      // method return from synthetic fake global method
      ret2.code shouldBe "RET"
      ret2.typeFullName shouldBe "ANY"
    }
    inside(cpg.method("main").ast.isReturn.l) { case List(ret1, ret2) =>
      ret1.code shouldBe "return 1;"
      ret1.lineNumber shouldBe Some(4)
      ret1.columnNumber shouldBe Some(4)
      ret2.code shouldBe "return 2;"
      ret2.lineNumber shouldBe Some(6)
      ret2.columnNumber shouldBe Some(2)
    }
    inside(cpg.method("main").methodReturn.cfgPrev.l) { case List(ret1, ret2) =>
      ret1.code shouldBe "return 1;"
      ret1.lineNumber shouldBe Some(4)
      ret1.columnNumber shouldBe Some(4)
      ret2.code shouldBe "return 2;"
      ret2.lineNumber shouldBe Some(6)
      ret2.columnNumber shouldBe Some(2)
    }
    pendingUntilFixed {
      inside(cpg.method("main").methodReturn.toReturn.l) { case List(ret1, ret2) =>
        ret1.code shouldBe "return 1;"
        ret1.lineNumber shouldBe Some(4)
        ret1.columnNumber shouldBe Some(4)
        ret2.code shouldBe "return 2;"
        ret2.lineNumber shouldBe Some(6)
        ret2.columnNumber shouldBe Some(2)
      }
    }
  }
}
