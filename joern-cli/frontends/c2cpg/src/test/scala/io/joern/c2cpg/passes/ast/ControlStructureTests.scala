package io.joern.c2cpg.passes.ast

import io.joern.c2cpg.parser.FileDefaults
import io.joern.c2cpg.testfixtures.C2CpgSuite
import io.shiftleft.codepropertygraph.generated.ControlStructureTypes
import io.shiftleft.semanticcpg.language.*
import org.apache.commons.lang3.StringUtils

class ControlStructureTests extends C2CpgSuite(FileDefaults.CppExt) {

  "ControlStructureTest1" should {
    val cpg = code("""
        |void foo(int x, int y) {
        | try {
        |    goto foo;
        |    foo:
        | } catch(exc_t exc) {
        |   // ...
        | }
        |
        | for(int i = 0; i < 10; i++) {
        |     if (x > y) {
        |     continue;
        |    }
        |    while(y++ < x) {
        |     printf("foo\n");
        |   }
        | }
        |
        |switch(y) {
        |  case 1:
        |   printf("bar\n");
        |   break;
        |  default:
        |};
        |
        | int i = 0;
        | do {
        |   i++;
        | } while(i < 11);
        |}
        |
        |""".stripMargin)

    "should identify `try` block" in {
      cpg.method("foo").tryBlock.code.l shouldBe List("try")
    }

    "should identify `if` block" in {
      cpg.method("foo").ifBlock.condition.code.l shouldBe List("x > y")
    }

    "should identify `switch` block" in {
      cpg.method("foo").switchBlock.code.map(StringUtils.normalizeSpace).l shouldBe List(
        "switch(y) { case 1: printf(\"bar\\n\"); break; default: }"
      )
    }

    "should identify `for` block" in {
      cpg.method("foo").forBlock.condition.code.l shouldBe List("i < 10")
    }

    "should identify `while` block" in {
      cpg.method("foo").whileBlock.condition.code.l shouldBe List("y++ < x")
    }

    "should identify `do` block" in {
      cpg.method("foo").doBlock.condition.code.l shouldBe List("i < 11")
    }

    "should identify `goto`" in {
      cpg.method("foo").goto.code.l shouldBe List("goto foo;")
    }

    "should identify `break`" in {
      cpg.method("foo").break.code.l shouldBe List("break;")
    }

    "should identify `continue`" in {
      cpg.method("foo").continue.code.l shouldBe List("continue;")
    }

  }

  "ControlStructureTest2" should {
    val cpg = code("""
        |void foo() {
        |  for (int x=1,y=1; x; --x) { bar(); };
        |}
        |""".stripMargin)

    "should be correct for for-loop with multiple assignments" in {
      inside(cpg.controlStructure.l) { case List(forLoop) =>
        forLoop.controlStructureType shouldBe ControlStructureTypes.FOR
        inside(forLoop.astChildren.isLocal.l) { case List(localX, localY) =>
          localX.code shouldBe "int x"
          localY.code shouldBe "int y"
        }
        inside(forLoop.astChildren.order(3).l) { case List(assignmentBlock) =>
          inside(assignmentBlock.astChildren.l) { case List(assignmentX, assignmentY) =>
            assignmentX.code shouldBe "x=1"
            assignmentX.order shouldBe 1
            assignmentY.code shouldBe "y=1"
            assignmentY.order shouldBe 2
          }
        }
        inside(forLoop.condition.l) { case List(x) =>
          x.code shouldBe "x"
          x.order shouldBe 4
        }
        inside(forLoop.astChildren.order(5).l) { case List(updateX) =>
          updateX.code shouldBe "--x"
        }
        inside(forLoop.astChildren.order(6).l) { case List(loopBody) =>
          loopBody.astChildren.isCall.head.code shouldBe "bar()"
        }
      }
    }

  }
}
