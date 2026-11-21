package io.joern.c2cpg.dataflow

import io.joern.c2cpg.parser.FileDefaults
import io.joern.c2cpg.testfixtures.DataFlowCodeToCpgSuite
import io.joern.dataflowengineoss.language.*
import io.shiftleft.semanticcpg.language.*

class PureVirtualCallDataflowTests extends DataFlowCodeToCpgSuite {

  private val fileName = s"Test${FileDefaults.CppExt}"

  "A call on an object of a class that overrides a pure virtual function" should {

    "not pass taint through the pure virtual function when the override returns a constant" in {
      val cpg = code(
        """
          |struct Op { virtual char *apply(char *s) = 0; };
          |struct Impl : Op { char *apply(char *s) override { return (char *)"constant"; } };
          |void sink(char *s) {}
          |void test(char *src) {
          |  Impl impl;
          |  sink(impl.apply(src));
          |}
          |""".stripMargin,
        fileName
      )
      def source = cpg.method.name("test").parameter.name("src")
      def sink   = cpg.method.name("sink").parameter.index(1)
      sink.reachableByFlows(source).map(flowToResultPairs).l shouldBe List()
    }

    "find the path through the override when it returns its argument" in {
      val cpg = code(
        """
          |struct Op { virtual char *apply(char *s) = 0; };
          |struct Impl : Op { char *apply(char *s) override { return s; } };
          |void sink(char *s) {}
          |void test(char *src) {
          |  Impl impl;
          |  sink(impl.apply(src));
          |}
          |""".stripMargin,
        fileName
      )
      def source = cpg.method.name("test").parameter.name("src")
      def sink   = cpg.method.name("sink").parameter.index(1)
      sink.reachableByFlows(source).map(flowToResultPairs).l shouldBe List(
        List(
          ("test(char *src)", 5),
          ("impl.apply(src)", 7),
          ("apply(this, char *s)", 3),
          ("return s;", 3),
          ("RET", 3),
          ("impl.apply(src)", 7),
          ("sink(char *s)", 4)
        )
      )
    }
  }

  "A call through a pointer to the base class" should {
    "keep the path through the pure virtual function when one override returns a constant" in {
      val cpg = code(
        """
          |struct Op { virtual char *apply(char *s) = 0; };
          |struct Impl : Op { char *apply(char *s) override { return (char *)"constant"; } };
          |void sink(char *s) {}
          |void test(Op *op, char *src) {
          |  sink(op->apply(src));
          |}
          |""".stripMargin,
        fileName
      )
      def source = cpg.method.name("test").parameter.name("src")
      def sink   = cpg.method.name("sink").parameter.index(1)
      sink.reachableByFlows(source).map(flowToResultPairs).l shouldBe List(
        List(("test(Op *op, char *src)", 5), ("op->apply(src)", 6), ("sink(char *s)", 4))
      )
    }
  }
}
