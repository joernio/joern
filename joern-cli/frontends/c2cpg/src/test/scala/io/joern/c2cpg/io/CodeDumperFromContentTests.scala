package io.joern.c2cpg.io

import io.joern.c2cpg.testfixtures.C2CpgSuite
import io.joern.c2cpg.Config
import io.shiftleft.semanticcpg.language.*

class CodeDumperFromContentTests extends C2CpgSuite {

  private implicit val finder: NodeExtensionFinder = DefaultNodeExtensionFinder

  "dumping code from content" should {
    val cpg = code("""
        |// A comment
        |int my_func(int param1)
        |{
        |   int x = foo(param1);
        |}
        |""".stripMargin).withConfig(Config().withDisableFileContent(false))

    "allow one to dump a method node's source code from `File.contents`" in {
      inside(cpg.method.nameExact("my_func").dumpRaw.l) {
        case content :: Nil =>
          content.linesIterator.map(_.strip).l shouldBe List(
            "int my_func(int param1) /* <=== my_func */",
            "{",
            "int x = foo(param1);",
            "}"
          )
        case content => fail(s"Expected exactly 1 content dump, but got: $content")
      }
    }
  }

  "code from method content" should {
    val myFuncContent =
      """int my_func(int param1)
        |{
        |  int x = foo(param1);
        |}""".stripMargin

    val cpg = code(s"""
        |// A comment
        |$myFuncContent;
        |""".stripMargin).withConfig(Config().withDisableFileContent(false))

    "allow one to dump a method node's source code from `Method.content`" in {
      val List(content) = cpg.method.nameExact("my_func").content.l
      content shouldBe myFuncContent
    }
  }

  "code from typedecl content" should {
    val myClassContent =
      """class Foo {
        |  public:
        |    int a;
        |    string b;
        |}""".stripMargin

    val cpg = code(
      s"""
         |// A comment
         |$myClassContent;
         |""".stripMargin,
      "Foo.cpp"
    ).withConfig(Config().withDisableFileContent(false))

    "allow one to dump a typedecl node's source code from `TypeDecl.content`" in {
      val List(content) = cpg.typeDecl.nameExact("Foo").content.l
      content shouldBe myClassContent
    }
  }

}
