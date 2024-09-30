package io.joern.jssrc2cpg.io

import io.joern.jssrc2cpg.testfixtures.JsSrc2CpgSuite
import io.joern.jssrc2cpg.Config
import io.shiftleft.semanticcpg.language.*

class CodeDumperFromContentTests extends JsSrc2CpgSuite {

  private implicit val finder: NodeExtensionFinder = DefaultNodeExtensionFinder

  "dumping code from content" should {
    val cpg = code(
      """
     |// A comment
     |function my_func(param1)
     |{
     |   var x = foo(param1);
     |}""".stripMargin,
      "index.js"
    ).withConfig(Config().withDisableFileContent(false))

    "allow one to dump a method node's source code from `File.contents`" in {
      inside(cpg.method.nameExact("my_func").dumpRaw.l) {
        case content :: Nil =>
          content.linesIterator.map(_.strip).l shouldBe List(
            "function my_func(param1) /* <=== index.js::program:my_func */",
            "{",
            "var x = foo(param1);",
            "}"
          )
        case content => fail(s"Expected exactly 1 content dump, but got: $content")
      }
    }
  }

  "code from method content" should {
    val myFuncContent =
      """function my_func(param1)
        |{
        |  var x = foo(param1);
        |}""".stripMargin

    val fullCode = s"""
        |// A comment
        |$myFuncContent
        |""".stripMargin

    val cpg = code(fullCode, "index.js").withConfig(Config().withDisableFileContent(false))

    "allow one to dump a method node's source code from `Method.content`" in {
      val List(content) = cpg.method.nameExact("my_func").content.l
      content shouldBe myFuncContent
    }

    "allow one to dump the :program method node's source code from `Method.content`" in {
      val List(content) = cpg.method.nameExact(":program").content.l
      content shouldBe fullCode
    }
  }

  "code from typedecl content" should {
    val myClassContent =
      """class Foo
        |{
        |  x = 'foo';
        |}""".stripMargin

    val cpg = code(
      s"""
         |// A comment
         |$myClassContent
         |""".stripMargin,
      "index.js"
    ).withConfig(Config().withDisableFileContent(false))

    "allow one to dump a typedecl node's source code from `TypeDecl.content`" in {
      val List(content) = cpg.typeDecl.nameExact("Foo").content.l
      content shouldBe myClassContent
    }

    "allow one to dump the <init> method node's source code from `Method.content`" in {
      val List(content) = cpg.method.nameExact("<init>").content.l
      content shouldBe myClassContent
    }
  }

  "content with UTF8 characters" should {
    val myClassContent =
      """class Foo {
        |  // ✅ This is a comment with UTF8.
        |  x = 'foo';
        |}""".stripMargin

    val cpg = code(s"""
         |// A comment
         |$myClassContent
         |""".stripMargin).withConfig(Config().withDisableFileContent(false))

    "allow one to dump source code" in {
      val List(content) = cpg.typeDecl.nameExact("Foo").content.l
      content shouldBe myClassContent
    }
  }

}
