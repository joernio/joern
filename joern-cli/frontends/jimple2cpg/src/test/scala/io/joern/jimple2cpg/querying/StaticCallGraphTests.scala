package io.joern.jimple2cpg.querying

import io.joern.jimple2cpg.testfixtures.JimpleCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language.{NoResolve, _}

class StaticCallGraphTests extends JimpleCode2CpgFixture {

  implicit val resolver: NoResolve.type = NoResolve

  val cpg: Cpg = code("""
       class Foo {
        static int add(int x, int y) {
         return x + y;
        }
        static int main(int argc, char argv) {
         System.out.println(add(1+2, 3));
         return 0;
        }
       }
    """).cpg

  "should find that add is called by main" in {
    cpg.method.name("add").caller.name.toSetMutable shouldBe Set("main")
  }

  "should find that main calls add and others" in {
    cpg.method.name("main").callee.name.filterNot(_.startsWith("<operator>")).toSetMutable shouldBe Set(
      "add",
      "println"
    )
  }

  "should find a set of outgoing calls for main" in {
    cpg.method.name("main").call.code.toSetMutable shouldBe
      Set(
        "add(3, 3)",
        "$stack2.println($stack3)",
        "$stack2 = java.lang.System.out",
        "$stack3 = add(3, 3)",
        "java.lang.System.out"
      )
  }

  "should find one callsite for add" in {
    cpg.method.name("add").callIn.code.toSetMutable shouldBe Set("add(3, 3)")
  }

  "should find that argument '1+2' is passed to parameter 'x'" in {
    cpg.parameter.name("x").argument.code.toSetMutable shouldBe Set("3")
  }

  "should allow traversing from argument to formal parameter" in {
    cpg.argument.parameter.name.toSetMutable should not be empty
  }

  "should allow traversing from argument to call" in {
    cpg.method.name("add").callIn.argument.inCall.name.toSetMutable shouldBe Set("add")
  }

}
