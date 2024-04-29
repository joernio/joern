package io.joern.swiftsrc2cpg.io

import io.joern.swiftsrc2cpg.testfixtures.SwiftSrc2CpgSuite
import io.joern.swiftsrc2cpg.Config
import io.shiftleft.semanticcpg.language.*

class CodeDumperFromContentTests extends SwiftSrc2CpgSuite {

  "dumping code from content" should {
    implicit val finder: NodeExtensionFinder = DefaultNodeExtensionFinder

    val cpg = code("""
     |// A comment
     |func my_func(param1: Int) -> Int {
     |  let x: Int = foo(p: param1)
     |}""".stripMargin).withConfig(Config().withDisableFileContent(false))

    "allow one to dump a method node's source code from `File.contents`" in {
      inside(cpg.method.nameExact("my_func").dumpRaw.l) {
        case content :: Nil =>
          content.linesIterator.map(_.strip).l shouldBe List(
            "func my_func(param1: Int) -> Int { /* <=== Test0.swift:<global>:my_func:Int(Int) */",
            "let x: Int = foo(p: param1)",
            "}"
          )
        case content => fail(s"Expected exactly 1 content dump, but got: $content")
      }
    }
  }

  "code from method content" should {
    val myFuncContent =
      """func my_func(param1: Int) -> Int {
        |  let x: Int = foo(p: param1)
        |}""".stripMargin

    val cpg = code(s"""
         |// A comment
         |$myFuncContent
         |""".stripMargin).withConfig(Config().withDisableFileContent(false))

    "allow one to dump a method node's source code from `Method.content`" in {
      val List(content) = cpg.method.nameExact("my_func").content.l
      content shouldBe myFuncContent
    }
  }

  "code from typedecl content" should {
    val myClassContent =
      """class Foo {
        |  var x = 'foo';
        |}""".stripMargin

    val cpg = code(s"""
         |// A comment
         |$myClassContent
         |""".stripMargin).withConfig(Config().withDisableFileContent(false))

    "allow one to dump a typedecl node's source code from `TypeDecl.content`" in {
      val List(content) = cpg.typeDecl.nameExact("Foo").content.l
      content shouldBe myClassContent
    }
  }

}
