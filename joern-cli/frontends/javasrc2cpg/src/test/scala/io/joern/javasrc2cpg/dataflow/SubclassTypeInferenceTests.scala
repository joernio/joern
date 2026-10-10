package io.joern.javasrc2cpg.dataflow

import io.joern.dataflowengineoss.language.*
import io.joern.javasrc2cpg.testfixtures.JavaSrcCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class SubclassTypeInferenceTests extends JavaSrcCode2CpgFixture(withOssDataflow = true) {

  "method calls with subclass arguments" should {
    val cpg = code(
      """
        |package foo;
        |
        |import a.b.Animal;
        |
        |public class Foo {
        |  public static void process(Animal item) {
        |    sink(item);
        |  }
        |
        |  static void sink(Animal item) {
        |    System.out.println(item);
        |  }
        |}
        |""".stripMargin,
      fileName = "Foo.java"
    ).moreCode(
      """
        |package example;
        |
        |import a.b.Animal;
        |
        |public class Child extends Animal {
        |  public String source;
        |}
        |""".stripMargin,
      fileName = "Child.java"
    ).moreCode(
      """
        |package example;
        |
        |import example.Child;
        |import foo.Foo;
        |
        |public class Caller {
        |  public static void run() {
        |    Child child = new Child();
        |    child.source = "MALICIOUS";
        |    Foo.process(child);
        |  }
        |}
        |""".stripMargin,
      fileName = "Caller.java"
    )

    "resolve process call when argument is a subclass of the declared parameter" in {
      val call = cpg.method.name("run").call.name("process").head
      call.methodFullName shouldBe "foo.Foo.process:void(a.b.Animal)"
      call.signature shouldBe "void(a.b.Animal)"
    }

    "flow taint from source field into println via resolved process call" in {
      def source = cpg.literal.code("\"MALICIOUS\"")
      def sink   = cpg.call
        .name(".*println.*")
        .argument(1)
        .ast
        .collectAll[io.shiftleft.codepropertygraph.generated.nodes.Expression]
      sink.reachableBy(source).size shouldBe 1
    }
  }
}
