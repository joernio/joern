package io.joern.javasrc2cpg.querying

import io.joern.javasrc2cpg.testfixtures.JavaSrcCodeToCpgFixture
import io.shiftleft.codepropertygraph.generated.nodes.{Block, Call, ControlStructure, Identifier, Local}
import io.shiftleft.semanticcpg.language._
import org.scalatest.Ignore

class ControlStructureTests extends JavaSrcCodeToCpgFixture {

  override val code =
    """
      |class Foo {
      |  int baz(Iterable<Integer> xs) {
      |    int sum = 0;
      |    for( Integer x : xs) {
      |      sum += x;
      |    }
      |    return sum;
      |  }
      |
      |  int bar(boolean x, boolean y, boolean z) {
      |    if (x || (y && z)) {
      |      return 1;
      |    }
      |    return 2;
      |  }
      |
      |  void foo(int x, int y) {
      |    try { } catch(exc_t exc) {
      |     // ...
      |    }
      |
      |    for(int i = 0; i < 10; i++) {
      |      if (x > y) {
      |        continue;
      |      }
      |      while(y++ < x) {
      |        printf("foo\n");
      |      }
      |    }
      |
      |    switch(y) {
      |      case 1:
      |        printf("bar\n");
      |        break;
      |      default:
      |    };
      |
      |    int i = 0;
      |    do {
      |      i++;
      |    } while(i < 11);
      |  }
      |
      |  public void elseTest(boolean b) {
      |    int x;
      |    if (b) {
      |      x = 42;
      |    } else {
      |      x = 39;
      |    }
      |  }
      |
      |  public boolean isConnected() {
      |    switch (this) {
      |      case Reconnected:
      |        return true;
      |
      |      case ConnectionLost:
      |      default:
      |        return false;
      |    }
      |  }
      |}
      |""".stripMargin

  "should identify `try` block" in {
    cpg.method.name("foo").tryBlock.code.l shouldBe List("try")
  }

  "should identify `if` block" in {
    cpg.method.name("foo").ifBlock.condition.code.l shouldBe List("x > y")
  }

  "should identify `switch` block" in {
    cpg.method.name("foo").switchBlock.code.l shouldBe List("switch(y)")
  }

  "should identify `for` block" in {
    cpg.method.name("foo").forBlock.condition.code.l shouldBe List("i < 10")
  }

  "should identify `while` block" in {
    cpg.method.name("foo").whileBlock.condition.code.l shouldBe List("y++ < x")
  }

  "should identify `do` block" in {
    cpg.method.name("foo").doBlock.condition.code.l shouldBe List("i < 11")
  }

  "should identify `break`" in {
    cpg.method.name("foo").break.code.l shouldBe List("break;")
  }

  "should identify `continue`" in {
    cpg.method.name("foo").continue.code.l shouldBe List("continue;")
  }

  "should handle complex boolean conditions" in {
    cpg.method.name("bar").ifBlock.condition.code.l shouldBe List("x || (y && z)")
  }

  "should parse a `foreach` loop as a for" in {
    val List(forLoop: ControlStructure) = cpg.method.name("baz").forBlock.l
    forLoop.controlStructureType shouldBe "FOR"
    val List(iterator: Identifier, variable: Local, body: Block) = forLoop.astChildren.l

    iterator.name shouldBe "xs"
    iterator.typeFullName shouldBe "java.lang.Iterable<java.lang.Integer>"
    variable.name shouldBe "x"
    variable.typeFullName shouldBe "java.lang.Integer"

    body.astChildren.head.code shouldBe "sum += x"
  }

  "should identify an else block" in {
    val ifBlock = cpg.method.name("elseTest").ifBlock.head
    ifBlock.code shouldBe "if (b)"
    val List(condition: Identifier, thenBlock: Block, elseBlock: ControlStructure) = ifBlock.astChildren.l
    condition.code shouldBe "b"
    condition.order shouldBe 1

    thenBlock.order shouldBe 2
    val thenBody = thenBlock.astChildren.head.asInstanceOf[Call]
    thenBody.code shouldBe "x = 42"
    thenBody.argument.head.code shouldBe "x"
    thenBody.argument.tail.head.code shouldBe "42"
    thenBody.order shouldBe 1

    elseBlock.code shouldBe "else"
    elseBlock.controlStructureType shouldBe "ELSE"
    elseBlock.order shouldBe 3
    val elseAssign = elseBlock.astChildren.head.astChildren.head.asInstanceOf[Call]
    elseAssign.order shouldBe 1
    elseAssign.code shouldBe "x = 39"
  }

  "should handle a switch conditioned on `this`" in {
    val switchBlock = cpg.method.name("isConnected").switchBlock.l match {
      case List(block) => block
      case res => fail(s"Expected single switch block but got $res")
    }

    switchBlock.astChildren.size shouldBe 2
    val List(cond: Identifier, body: Block) = switchBlock.astChildren.l

    cond.order shouldBe 1
    cond.code shouldBe "this"
    cond.typeFullName shouldBe "Foo"

    // The visible statements/labels + jump targets
    body.astChildren.size shouldBe 7
  }
}
