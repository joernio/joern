package io.joern.jimple2cpg.querying

import io.joern.jimple2cpg.Config
import io.joern.jimple2cpg.testfixtures.JimpleCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language._

class CodeDumperTests extends JimpleCode2CpgFixture {
  private val config = Config().withDisableFileContent(false)

  "a Java source code CPG" should {
    implicit val finder: NodeExtensionFinder = DefaultNodeExtensionFinder
    val cpg: Cpg = code(
      """
                     |public class Foo {
                     |
                     | public void test() {
                     |   var a = 1;
                     |   var b = 2;
                     |   var c = a + b;
                     | }
                     |
                     |}
                     |""".stripMargin,
      "Foo.java"
    )
      .withConfig(config)
      .cpg

    "allow one to get decompiled java code in one file" in {
      inside(cpg.file.name(".*Foo.class").l) {
        case decompiledJava :: Nil =>
          decompiledJava.content.linesIterator.map(_.strip).l shouldBe List(
            "/*",
            "* Decompiled with CFR 0.152.",
            "*/",
            "public class Foo {",
            "public void test() {",
            "int a = 1;",
            "int b = 2;",
            "int c = a + b;",
            "}",
            "}"
          )
        case content => fail(s"Expected exactly 1 file")
      }
    }
  }

  "Java Source CPG across multiple files" should {
    implicit val finder: NodeExtensionFinder = DefaultNodeExtensionFinder
    val cpg: Cpg = code(
      """
        |package bar;
        |public class Foo {
        | public void test() {
        |   var a = 1;
        |   var b = 2;
        |   var c = a + b;
        | }
        |}
        |""".stripMargin,
      "Foo.java"
    ).moreCode(
      """
          |package bar;
          |public class Baz {
          | public void bazTest() {
          |   var fooObj = new Foo();
          |   var b = 2;
          | }
          |}
          |""".stripMargin,
      "Baz.java"
    ).withConfig(config)
      .cpg

    "allow one to get java decompiled code for all classes" in {
      inside(cpg.file.name(".*Foo.class").l) {
        case decompiledJavaFoo :: Nil =>
          decompiledJavaFoo.content.linesIterator.map(_.strip).filter(_.nonEmpty).l shouldBe List(
            "/*",
            "* Decompiled with CFR 0.152.",
            "*/",
            "package bar;",
            "public class Foo {",
            "public void test() {",
            "int a = 1;",
            "int b = 2;",
            "int c = a + b;",
            "}",
            "}"
          )

        case _ => fail("Expected exactly 1 file")
      }

      inside(cpg.file.name(".*Baz.class").l) {
        case decompiledJavaBaz :: Nil =>
          decompiledJavaBaz.content.linesIterator.map(_.strip).filter(_.nonEmpty).l shouldBe List(
            "/*",
            "* Decompiled with CFR 0.152.",
            "*/",
            "package bar;",
            "import bar.Foo;",
            "public class Baz {",
            "public void bazTest() {",
            "Foo fooObj = new Foo();",
            "int b = 2;",
            "}",
            "}"
          )
        case _ => fail("Expected exactly 1 file")
      }
    }
  }
}
