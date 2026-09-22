package io.joern.c2cpg.dataflow

import io.joern.c2cpg.testfixtures.DataFlowCodeToCpgSuite
import io.joern.dataflowengineoss.language.*
import io.shiftleft.semanticcpg.language.*

class DoublePointerDataflowTests extends DataFlowCodeToCpgSuite {

  "Issue5580 double pointer via address-of and dereference" should {
    val cpg = code("""
        |#include <stdio.h>
        |
        |int main(int argc, char **argv) {
        |  char *colors[] = {"red", "green", "blue"};
        |
        |  char *red = colors[0];
        |
        |  char **color_list = colors;
        |  char *green = color_list[1];
        |
        |  char **blue_pointer = &colors[2];
        |  char *blue = *blue_pointer;
        |
        |  printf("R:%s\n", red);
        |  printf("G:%s\n", green);
        |  printf("B:%s\n", blue);
        |  return 0;
        |}""".stripMargin)

    "find flow from colors to blue" in {
      val source = cpg.identifier("colors")
      val sink   = cpg.identifier("blue")
      sink.reachableByFlows(source).nonEmpty shouldBe true
    }

    "still find flow from colors to red" in {
      val source = cpg.identifier("colors").where(_.method.name("main"))
      cpg.identifier("red").where(_.method.name("main")).reachableByFlows(source).nonEmpty shouldBe true
    }

    "still find flow from colors to green" in {
      val source = cpg.identifier("colors").where(_.method.name("main"))
      cpg.identifier("green").where(_.method.name("main")).reachableByFlows(source).nonEmpty shouldBe true
    }
  }

  "direct address-of to identifier then dereference" should {
    val cpg = code("""
        |void use(char *b);
        |
        |void foo() {
        |  char *colors;
        |  char **p = &colors;
        |  char *b = *p;
        |  use(b);
        |}""".stripMargin)

    "find flow from colors to b" in {
      val source = cpg.identifier("colors")
      val sink   = cpg.identifier("b")
      sink.reachableByFlows(source).nonEmpty shouldBe true
    }
  }

  "reassigned pointer drops stale address-of" should {
    val cpg = code("""
        |void sink(int x);
        |
        |void foo() {
        |  int a;
        |  int b;
        |  int *p;
        |  p = &a;
        |  p = &b;
        |  int x = *p;
        |  sink(x);
        |}""".stripMargin)

    "not flow from a to x after reassignment" in {
      val source = cpg.identifier("a")
      val sink   = cpg.identifier("x")
      sink.reachableByFlows(source).size shouldBe 0
    }
  }

  "shadowed locals with same name in different blocks" should {
    val cpg = code("""
        |int cond();
        |
        |void foo(char *a, char *b) {
        |  char *x_then;
        |  char *x_else;
        |  if (cond()) {
        |    char **p = &a;
        |    x_then = *p;
        |  } else {
        |    char **p = &b;
        |    x_else = *p;
        |  }
        |}""".stripMargin)

    "flow from a to x_then and not to x_else" in {
      val source = cpg.identifier.name("a").where(_.method.name("foo"))
      cpg.identifier("x_then").reachableByFlows(source).nonEmpty shouldBe true
      cpg.identifier("x_else").reachableByFlows(source).size shouldBe 0
    }

    "flow from b to x_else and not to x_then" in {
      val source = cpg.identifier.name("b").where(_.method.name("foo"))
      cpg.identifier("x_else").reachableByFlows(source).nonEmpty shouldBe true
      cpg.identifier("x_then").reachableByFlows(source).size shouldBe 0
    }
  }

  "unrelated sink" should {
    val cpg = code("""
        |void sink(int b);
        |
        |void foo() {
        |  int a;
        |  int b;
        |  int *p = &a;
        |  sink(b);
        |}""".stripMargin)

    "not flow from a to sink argument b" in {
      val source = cpg.identifier("a")
      val sink   = cpg.call("sink").argument(1)
      sink.reachableByFlows(source).size shouldBe 0
    }
  }

}
