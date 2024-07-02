package io.joern.javasrc2cpg.querying.dataflow

import io.joern.dataflowengineoss.language.*
import io.joern.javasrc2cpg.testfixtures.JavaSrcCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class LambdaTests extends JavaSrcCode2CpgFixture(withOssDataflow = true) {

  "dataflow through a simple lambda call should be found" in {
    val cpg = code("""
        |import java.util.function.Consumer;
        |
        |public class Foo {
        |
        |    public static void sink(String s) {}
        |
        |    public static Consumer<String> getConsumer() {
        |        return input -> sink(input);
        |    }
        |
        |    public static void test(String input) {
        |        Consumer<String> consumer = getConsumer();
        |        consumer.accept(input);
        |    }
        |}
        |""".stripMargin)

    def source = cpg.method.name("test").parameter.name("input")
    def sink   = cpg.method.name("sink").parameter.name("s")

    pendingUntilFixed {
      sink.reachableBy(source).isEmpty shouldBe false
    }
  }

  "dataflow around lambda in map" should {
    val cpg = code("""
        |import java.util.ArrayList;
        |import java.util.List;
        |
        |public class LambdaExample {
        |
        |    public foo1() {
        |       String myValue = "abc";
        |		List<String> userPayload = new ArrayList<>();
        |		List<String> userNamesList = userPayload.stream.map(item -> {
        |           sink2(myValue);
        |           return item + myValue;
        |       });
        |		sink1(userNamesList);
        |       return;
        |    }
        |}
        |""".stripMargin)

    "be found for case 1" in {
      def source = cpg.identifier("item")

      def sink = cpg.call("sink1")

      sink.reachableByFlows(source).isEmpty shouldBe false
    }

    "be found for case 2" in {
      def source = cpg.identifier("myValue").head

      def sink = cpg.call("sink2")

      sink.reachableByFlows(source).isEmpty shouldBe false
    }
  }

  "dataflow around lambda in foreach" should {
    val cpg = code("""
        |import java.util.ArrayList;
        |import java.util.List;
        |
        |public class LambdaExample {
        |
        |    public foo() {
        |       String myValue = "abc";
        |       List<String> userPayload = new ArrayList<>();
        |       List<String> userNamesList = new ArrayList<>();
        |       userPayload.forEach(item -> {
        |           userNamesList.add(item + myValue);
        |           sink2(myValue);
        |       });
        |       sink1(userNamesList);
        |       return;
        |     }
        |}
        |""".stripMargin)

    "be found for case 1" ignore {
      def source = cpg.identifier("item")

      def sink = cpg.call("sink1")

      sink.reachableByFlows(source).isEmpty shouldBe false
    }

    "be found for case 2" in {
      def source = cpg.identifier("myValue").head

      def sink = cpg.call("sink2")

      sink.reachableByFlows(source).isEmpty shouldBe false
    }
  }

}
