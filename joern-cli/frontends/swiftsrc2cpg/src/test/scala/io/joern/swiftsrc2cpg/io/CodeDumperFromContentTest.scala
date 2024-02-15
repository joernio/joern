package io.joern.swiftsrc2cpg.io

import io.joern.swiftsrc2cpg.testfixtures.SwiftSrc2CpgSuite
import io.joern.swiftsrc2cpg.Config
import io.shiftleft.semanticcpg.language.*

class CodeDumperFromContentTest extends SwiftSrc2CpgSuite {

  private val codeString = """
   |// A comment
   |func my_func(param1: Int) -> Int {
   |  let x: Int = foo(p: param1)
   |}""".stripMargin

  "dumping code from content" should {
    implicit val finder: NodeExtensionFinder = DefaultNodeExtensionFinder

    val cpg = code(codeString, "test.swift").withConfig(Config().withDisableFileContent(false))

    "allow one to dump a method node's source code from `File.contents`" in {
      inside(cpg.method.nameExact("my_func").dumpRaw.l) {
        case content :: Nil =>
          content.linesIterator.map(_.strip).l shouldBe List(
            "func my_func(param1: Int) -> Int { /* <=== test.swift:<global>:my_func */",
            "let x: Int = foo(p: param1)",
            "}"
          )
        case content => fail(s"Expected exactly 1 content dump, but got: $content")
      }
    }
  }

}
