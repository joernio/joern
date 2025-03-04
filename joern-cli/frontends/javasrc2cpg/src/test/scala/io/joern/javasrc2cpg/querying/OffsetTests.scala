package io.joern.javasrc2cpg.querying

import io.joern.javasrc2cpg.Config
import io.joern.javasrc2cpg.testfixtures.JavaSrcCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class OffsetTests extends JavaSrcCode2CpgFixture {
  private val contentEnabled = Config().withDisableFileContent(false)

  "a class with a single method without file content enabled" should {
    val source = """
        |class Foo {
        |  void foo(int x) {
        |    System.out.println(x);
        |  }
        |}
        |""".stripMargin

    val cpg = code(source, fileName = "Foo.java").withConfig(Config())

    "have an empty content field" in {
      cpg.file.name(".*Foo.java").content.l shouldBe List("<empty>")
    }

    "not have offsets for the foo method" in {
      def method = cpg.method.name("foo").head
      method.offset shouldBe None
      method.offsetEnd shouldBe None

    }

    "not have offsets set for the default constructor" in {
      def method = cpg.method.nameExact("<init>").head
      method.offset shouldBe None
      method.offsetEnd shouldBe None
    }
  }

  "a class with a single method" should {
    val source = """
        |class Foo {
        |  void foo(int x) {
        |    System.out.println(x);
        |  }
        |}
        |""".stripMargin

    val cpg = code(source, fileName = "Foo.java").withConfig(contentEnabled)

    "have the file content field set correctly" in {
      cpg.file.name(".*Foo.java").content.l shouldBe List(source)
    }

    "have the method's source representation be retrievable via the offset fields" in {
      def method    = cpg.method.name("foo").head
      val offset    = method.offset.get
      val offsetEnd = method.offsetEnd.get

      cpg.file.name(".*Foo.java").content.head.substring(offset, offsetEnd) shouldBe
        """void foo(int x) {
          |    System.out.println(x);
          |  }""".stripMargin
    }

    "not have offsets set for the default constructor" in {
      def method = cpg.method.nameExact("<init>").head
      method.offset shouldBe None
      method.offsetEnd shouldBe None
    }
  }

  "a class with multiple methods" should {
    val method1 = """void method1(int param1) {
    |    System.out.println(param1);
    |  }""".stripMargin

    val method2 = """void method2(int param2) {
    |    System.out.println(param2);
    |  }""".stripMargin

    val cpg = code(
      s"""
       |class Foo {
       |  $method1
       |
       |  $method2
       |}
       |""".stripMargin,
      fileName = "Foo.java"
    ).withConfig(contentEnabled)

    "have the first method's source representation retrievable via the sourceCode property" in {
      def method = cpg.method.name("method1").head
      method.sourceCode shouldBe method1
    }

    "have the second method's source representation retrievable via the sourceCode property" in {
      inside(cpg.method.name("method2").sourceCode.l) { case List(sourceCode) => sourceCode shouldBe method2 }
    }

    "have the first method's source representation retrievable via the offset fields" in {
      def method    = cpg.method.name("method1").head
      val offset    = method.offset.get
      val offsetEnd = method.offsetEnd.get

      cpg.file.name(".*Foo.java").content.head.substring(offset, offsetEnd) shouldBe method1
    }

    "have the second method's source representation retrievable via the offset fields" in {
      def method    = cpg.method.name("method2").head
      val offset    = method.offset.get
      val offsetEnd = method.offsetEnd.get

      cpg.file.name(".*Foo.java").content.head.substring(offset, offsetEnd) shouldBe method2
    }

    "not have offsets set for the default constructor" in {
      def method = cpg.method.nameExact("<init>").head
      method.offset shouldBe None
      method.offsetEnd shouldBe None
    }
  }

  "a class with an explicit constructor and multiple methods" should {
    val constructor = """Foo() {
    |    // Do nothing
    |  }""".stripMargin

    val method1 = """void method1(int param1) {
    |    System.out.println(param1);
    |  }""".stripMargin

    val method2 = """void method2(int param2) {
    |    System.out.println(param2);
    |  }""".stripMargin

    val cpg = code(
      s"""
       |class Foo {
       |  $method1
       |
       |  $method2
       |
       |  $constructor
       |}
       |""".stripMargin,
      fileName = "Foo.java"
    ).withConfig(contentEnabled)

    "have the first method's source representation retrievable via the offset fields" in {
      def method    = cpg.method.name("method1").head
      val offset    = method.offset.get
      val offsetEnd = method.offsetEnd.get

      cpg.file.name(".*Foo.java").content.head.substring(offset, offsetEnd) shouldBe method1
    }

    "have the second method's source representation retrievable via the offset fields" in {
      def method    = cpg.method.name("method2").head
      val offset    = method.offset.get
      val offsetEnd = method.offsetEnd.get

      cpg.file.name(".*Foo.java").content.head.substring(offset, offsetEnd) shouldBe method2
    }

    "have the constructor's source representation be retrievable via the offset fields" in {
      def method    = cpg.method.nameExact("<init>").head
      val offset    = method.offset.get
      val offsetEnd = method.offsetEnd.get

      cpg.file.name(".*Foo.java").content.head.substring(offset, offsetEnd) shouldBe constructor
    }
  }

  "a class with a lambda method" should {
    "have the lambda method source be retrievable via the synthetic method offsets" in {
      val cpg = code(
        """
          |import java.util.function.Consumer;
          |
          |class Foo {
          |  Consumer<String> buildConsumer() {
          |    return value -> System.out.println(value);
          |  }
          |}
          """.stripMargin,
        fileName = "Foo.java"
      ).withConfig(contentEnabled)

      def method    = cpg.method.name(".*lambda.*").head
      val offset    = method.offset.get
      val offsetEnd = method.offsetEnd.get

      cpg.file
        .name(".*Foo.java")
        .content
        .head
        .substring(offset, offsetEnd) shouldBe "value -> System.out.println(value)"
    }
  }
}
