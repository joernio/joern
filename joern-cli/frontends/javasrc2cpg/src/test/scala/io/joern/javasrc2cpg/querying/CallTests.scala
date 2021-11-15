package io.joern.javasrc2cpg.querying

import io.joern.javasrc2cpg.testfixtures.JavaSrcCodeToCpgFixture
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, nodes}
import io.shiftleft.codepropertygraph.generated.nodes.Call
import io.shiftleft.semanticcpg.language.NoResolve
import io.shiftleft.semanticcpg.language._

class CallTests extends JavaSrcCodeToCpgFixture {

  implicit val resolver: ICallResolver = NoResolve

  override val code: String =
    """
      | class Foo {
      |   int add(int x, int y) {
      |     return x + y;
      |   }
      |
      |   int main(int argc, char argv) {
      |     return add(argc, 3);
      |   }
      |
      |   int bar(int argc) {
      |     foo(argc);
      |   }
      | }
      |""".stripMargin

  "should contain a call node for `add` with correct fields" in {
    val List(x) = cpg.call("add").l
    x.code shouldBe "add(argc, 3)"
    x.name shouldBe "add"
    x.order shouldBe 2
    x.methodFullName shouldBe "Foo.add:int(int,int)"
    x.signature shouldBe "int(int,int)"
    x.argumentIndex shouldBe 2
    x.lineNumber shouldBe Some(8)
  }

  "should allow traversing from call to arguments" in {
    cpg.call("add").argument.size shouldBe 3
    val List(arg0) = cpg.call("add").argument(0).l
    arg0.isInstanceOf[nodes.Identifier] shouldBe true
    arg0.asInstanceOf[nodes.Identifier].name shouldBe "this"
    arg0.code shouldBe "this"
    arg0.order shouldBe 0
    arg0.argumentIndex shouldBe 0

    val List(arg1) = cpg.call("add").argument(1).l
    arg1.isInstanceOf[nodes.Identifier] shouldBe true
    arg1.asInstanceOf[nodes.Identifier].name shouldBe "argc"
    arg1.code shouldBe "argc"
    arg1.order shouldBe 1
    arg1.argumentIndex shouldBe 1

    val List(arg2) = cpg.call("add").argument(2).l
    arg2.asInstanceOf[nodes.Literal].code shouldBe "3"
    arg2.isInstanceOf[nodes.Literal] shouldBe true
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

  "should handle unresolved calls with appropriate defaults" in {
    val List(call: Call) = cpg.call("foo").l
    call.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH.toString
    call.methodFullName shouldBe "<empty>"
    call.signature shouldBe ""
    call.code shouldBe "foo(argc)"
  }
}
