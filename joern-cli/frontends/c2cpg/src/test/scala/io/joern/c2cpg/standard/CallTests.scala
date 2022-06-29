package io.joern.c2cpg.standard

import io.joern.c2cpg.testfixtures.CCodeToCpgSuite
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Literal}
import io.shiftleft.semanticcpg.language.{NoResolve, _}

class CallTests extends CCodeToCpgSuite {

  implicit val resolver: NoResolve.type = NoResolve

  private val cpg = code("""
    |int add(int x, int y) {
    |  return x + y;
    |}
    |int main(int argc, char **argv) {
    |  printf("%d\n", add((1+2), 3));
    |}""".stripMargin)

  "should contain a call node for `add` with correct fields" in {
    val List(x) = cpg.call("add").l
    x.code shouldBe "add((1+2), 3)"
    x.name shouldBe "add"
    x.order shouldBe 2
    x.methodFullName shouldBe "add"
    x.argumentIndex shouldBe 2
    // TODO x.signature
    // x.typeFullName : deprecated
    x.lineNumber shouldBe Some(6)
    x.columnNumber shouldBe Some(18)
  }

  "should allow traversing from call to arguments" in {
    cpg.call("add").argument.size shouldBe 2

    val List(arg1) = cpg.call("add").argument(1).l
    arg1.isInstanceOf[Call] shouldBe true
    arg1.asInstanceOf[Call].name shouldBe Operators.addition
    arg1.code shouldBe "1+2"
    arg1.order shouldBe 1
    arg1.argumentIndex shouldBe 1

    val List(arg2) = cpg.call("add").argument(2).l
    arg2.isInstanceOf[Literal] shouldBe true
    arg2.asInstanceOf[Literal].code shouldBe "3"
    arg2.code shouldBe "3"
    arg2.order shouldBe 2
    arg2.argumentIndex shouldBe 2
  }

  "should allow traversing from call to surrounding method" in {
    val List(x) = cpg.call("add").method.l
    x.name shouldBe "main"
  }

  "should allow traversing from call to callee method" in {
    val List(x) = cpg.call("add").callee.l
    x.name shouldBe "add"
  }

  "should allow traversing from argument to parameter" in {
    val List(x) = cpg.call("add").argument(1).parameter.l
    x.name shouldBe "x"
  }

}
