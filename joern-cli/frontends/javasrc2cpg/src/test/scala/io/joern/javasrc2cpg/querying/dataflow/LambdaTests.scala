package io.joern.javasrc2cpg.querying.dataflow

import io.joern.dataflowengineoss.language._
import io.joern.javasrc2cpg.testfixtures.JavaSrcCode2CpgFixture
import io.shiftleft.semanticcpg.language._

class LambdaTests extends JavaSrcCode2CpgFixture {

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

}
