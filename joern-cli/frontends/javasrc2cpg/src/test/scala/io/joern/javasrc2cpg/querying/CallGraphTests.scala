package io.joern.javasrc2cpg.querying

import io.joern.javasrc2cpg.testfixtures.JavaSrcCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.semanticcpg.language.*

class CallGraphTests extends JavaSrcCode2CpgFixture {

  lazy val cpg = code("""
       |class Foo {
       | int add(int x, int y) {
       |  return x + y;
       | }
       | int main(int argc, char argv) {
       |  System.out.println(add(1+2, 3));
       | }
       |}
    """.stripMargin)

  "should find that add is called by main" in {
    cpg.method.name("add").caller.name.toSetMutable shouldBe Set("main")
  }

  "should find that main calls add and others" in {
    cpg.method.name("main").callee.name.toSetMutable shouldBe Set(
      "add",
      "println",
      Operators.fieldAccess,
      Operators.addition
    )
  }

  "should find three outgoing calls for main" in {
    cpg.method.name("main").call.code.toSetMutable shouldBe Set(
      "1 + 2",
      "this.add(1 + 2, 3)",
      "System.out",
      "System.out.println(add(1 + 2, 3))"
    )
  }

  "should find one callsite for add" in {
    cpg.method.name("add").callIn.code.toSetMutable shouldBe Set("this.add(1 + 2, 3)")
  }

  "should find that argument '1+2' is passed to parameter 'x'" in {
    cpg.parameter.name("x").argument.code.toSetMutable shouldBe Set("1 + 2")
  }

  "should allow traversing from argument to formal parameter" in {
    cpg.argument.parameter.name.toSetMutable should not be empty
  }

  "should allow traversing from argument to call" in {
    cpg.method.name("println").callIn.argument.inCall.name.toSetMutable shouldBe Set("println")
  }

}
