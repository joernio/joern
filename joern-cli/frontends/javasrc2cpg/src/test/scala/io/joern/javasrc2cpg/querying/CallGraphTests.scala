package io.joern.javasrc2cpg.querying

import io.joern.javasrc2cpg.testfixtures.JavaSrcCodeToCpgFixture
import io.shiftleft.semanticcpg.language.NoResolve
import io.shiftleft.semanticcpg.language._

class CallGraphTests extends JavaSrcCodeToCpgFixture {

  implicit val resolver = NoResolve

  override val code = """
       class Foo {
        int add(int x, int y) {
         return x + y;
        }
        int main(int argc, char argv) {
         System.out.println(add(1+2, 3));
        }
       }
    """

  "should find that add is called by main" in {
    cpg.method.name("add").caller.name.toSet shouldBe Set("main")
  }

  "should find that main calls add and others" in {
    cpg.method.name("main").callee.name.toSet shouldBe Set("add", "println", "<operator>.addition")
  }

  "should find three outgoing calls for main" in {
    cpg.method.name("main").call.code.toSet shouldBe
      Set("1 + 2", "add(1 + 2, 3)", "println(add(1 + 2, 3))")
  }

  "should find one callsite for add" in {
    cpg.method.name("add").callIn.code.toSet shouldBe Set("add(1 + 2, 3)")
  }

  "should find that argument '1+2' is passed to parameter 'x'" in {
    cpg.parameter.name("x").argument.code.toSet shouldBe Set("1 + 2")
  }

  "should allow traversing from argument to formal parameter" in {
    cpg.argument.parameter.name.toSet should not be empty
  }

  "should allow traversing from argument to call" in {
    cpg.method.name("println").callIn.argument.inCall.name.toSet shouldBe Set("println")
  }

}
