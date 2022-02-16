package io.joern.jimple2cpg.querying

import io.joern.jimple2cpg.testfixtures.JimpleCodeToCpgFixture
import io.shiftleft.semanticcpg.language.{NoResolve, _}

class StaticCallGraphTests extends JimpleCodeToCpgFixture {

  implicit val resolver: NoResolve.type = NoResolve

  override val code = """
       class Foo {
        static int add(int x, int y) {
         return x + y;
        }
        static int main(int argc, char argv) {
         System.out.println(add(1+2, 3));
         return 0;
        }
       }
    """

  "should find that add is called by main" in {
    cpg.method.name("add").caller.name.toSet shouldBe Set("main")
  }

  "should find that main calls add and others" in {
    cpg.method.name("main").callee.name.filterNot(_.startsWith("<operator>")).toSet shouldBe Set("add", "println")
  }

  "should find a set of outgoing calls for main" in {
    cpg.method.name("main").call.code.toSet shouldBe
      Set(
        "println($stack3)",
        "add(3, 3)",
        "argc = @parameter0",
        "$stack2 = java.lang.System.out",
        "argv = @parameter1",
        "$stack3 = add(3, 3)",
        "java.lang.System.out"
      )
  }

  "should find one callsite for add" in {
    cpg.method.name("add").callIn.code.toSet shouldBe Set("add(3, 3)")
  }

  "should find that argument '1+2' is passed to parameter 'x'" in {
    cpg.parameter.name("x").argument.code.toSet shouldBe Set("3")
  }

  "should allow traversing from argument to formal parameter" in {
    cpg.argument.parameter.name.toSet should not be empty
  }

  "should allow traversing from argument to call" in {
    cpg.method.name("add").callIn.argument.inCall.name.toSet shouldBe Set("add")
  }

}
