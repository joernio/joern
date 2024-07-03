package io.joern.swiftsrc2cpg.io

import better.files.File
import io.joern.swiftsrc2cpg.testfixtures.SwiftSrc2CpgSuite
import io.shiftleft.semanticcpg.codedumper.CodeDumper
import io.shiftleft.semanticcpg.language.*

import java.util.regex.Pattern

class CodeDumperFromFileTests extends SwiftSrc2CpgSuite {

  implicit val finder: NodeExtensionFinder = DefaultNodeExtensionFinder

  private val codeString = """
   |// A comment
   |func my_func(param1: Int) -> Int {
   |  let x: Int = foo(p: param1)
   |}""".stripMargin

  private val cpg = code(codeString, "test.swift")

  private val path = File(cpg.metaData.root.head) / "test.swift"

  override def beforeAll(): Unit = {
    super.beforeAll()
    // we have to restore the input file because CPG creation in SwiftSrc2CpgSuite
    // deletes it right after the CPG is ready.
    path.createFileIfNotExists(createParents = true)
    path.writeText(codeString)
  }

  override def afterAll(): Unit = {
    super.afterAll()
    path.delete(swallowIOExceptions = true)
  }

  "dumping code from file" should {

    "return empty string for empty traversal" in {
      cpg.method.name("notinthere").dump shouldBe empty
    }

    "be able to dump complete function" in {
      val code = cpg.method.name("my_func").dumpRaw.mkString("\n")
      code should (
        startWith("func my_func")
          and include("foo(p: param1)")
          and endWith("}")
      )
    }

    "dump method with arrow for expression (a call)" ignore {
      val code = cpg.call.name("foo").dumpRaw.mkString("\n")
      code should (
        startWith("func")
          and include regex s".*let x: Int = foo.*${Pattern.quote(CodeDumper.arrow(Option("my_func")).toString)}.*"
          and endWith("}")
      )
    }

    "allow dumping via .dump" in {
      val code = cpg.method.name("my_func").dumpRaw.mkString("\n")
      code should startWith("func my_func")
    }

    "allow dumping callIn" ignore {
      implicit val resolver: ICallResolver = NoResolve
      val code                             = cpg.method.name("foo").callIn.dumpRaw.mkString("\n")
      code should startWith("func my_func")
    }

  }

}
