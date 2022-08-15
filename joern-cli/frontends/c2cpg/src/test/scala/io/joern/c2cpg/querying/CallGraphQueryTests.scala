package io.joern.c2cpg.querying

import io.joern.c2cpg.testfixtures.CCodeToCpgSuite
import io.shiftleft.semanticcpg.language.{NoResolve, _}

class CallGraphQueryTests extends CCodeToCpgSuite {

  private implicit val resolver: NoResolve.type = NoResolve

  private val cpg = code("""
     |int add(int x, int y) {
     |  return x + y;
     |}
     |
     |int main(int argc, char **argv) {
     |  printf("%d\n", add((1+2), 3));
     |}
    """.stripMargin)

  "should find that add is called by main" in {
    cpg.method.name("add").caller.name.toSetMutable shouldBe Set("main")
  }

  "should find that main calls add and others" in {
    cpg.method.name("main").callee.name.toSetMutable shouldBe Set("add", "printf", "<operator>.addition")
  }

  "should find three outgoing calls for main" in {
    cpg.method.name("main").call.code.toSetMutable shouldBe
      Set("1+2", "add((1+2), 3)", "printf(\"%d\\n\", add((1+2), 3))")
  }

  "should find one callsite for add" in {
    cpg.method.name("add").callIn.code.toSetMutable shouldBe Set("add((1+2), 3)")
  }

  "should find that argument '1+2' is passed to parameter 'x'" in {
    cpg.parameter.name("x").argument.code.toSetMutable shouldBe Set("1+2")
  }

  "should allow traversing from argument to formal parameter" in {
    cpg.argument.parameter.name.toSetMutable should not be empty
  }

  "should allow traversing from argument to call" in {
    cpg.method.name("printf").callIn.argument.inCall.name.toSetMutable shouldBe Set("printf")
  }

}
