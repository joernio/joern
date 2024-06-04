package io.joern.jimple2cpg.querying

import io.joern.jimple2cpg.testfixtures.JimpleCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes
import io.shiftleft.semanticcpg.language.{NoResolve, _}

class CallTests extends JimpleCode2CpgFixture {

  implicit val resolver: ICallResolver = NoResolve

  lazy val cpg: Cpg = code("""
      | class Foo {
      |   static int add(int x, int y) {
      |     return x + y;
      |   }
      |
      |   static int main(int argc, char argv) {
      |     return add(argc, 3);
      |   }
      | }
      |""".stripMargin).cpg

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
    cpg.call("add").argument.size shouldBe 2

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
    val List(x) = cpg.call.nameExact("add").method.l
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
