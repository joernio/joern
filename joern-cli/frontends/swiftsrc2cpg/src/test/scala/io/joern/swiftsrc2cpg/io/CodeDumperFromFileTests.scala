package io.joern.swiftsrc2cpg.io

import io.shiftleft.semanticcpg.utils.FileUtil.*
import io.joern.swiftsrc2cpg.testfixtures.SwiftCompilerSrc2CpgSuite
import io.shiftleft.semanticcpg.codedumper.CodeDumper
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.utils.FileUtil

import java.nio.file.{Files, Paths}
import java.util.regex.Pattern

class CodeDumperFromFileTests extends SwiftCompilerSrc2CpgSuite {

  implicit val finder: NodeExtensionFinder = DefaultNodeExtensionFinder

  private val codeString = """
   |// A comment
   |func foo(p: Int) -> Int { return p }
   |func my_func(param1: Int) -> Int {
   |  let x: Int = foo(p: param1)
   |  return x
   |}""".stripMargin

  private val cpg = codeWithSwiftSetup(codeString)

  // The Swift compiler writes the source under `<root>/Sources/main.swift`. The CPG creation
  // pipeline deletes the source tree once the CPG is built, so we restore the file before
  // the dump-based assertions read it back.
  private val path = Paths.get(cpg.metaData.root.head) / "Sources" / "main.swift"

  override def beforeAll(): Unit = {
    super.beforeAll()
    path.createWithParentsIfNotExists(createParents = true)
    Files.writeString(path, codeString)
  }

  override def afterAll(): Unit = {
    super.afterAll()
    FileUtil.delete(path, swallowIoExceptions = true)
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

    "dump method with arrow for expression (a call)" in {
      val code           = cpg.call.name("foo").dumpRaw.mkString("\n")
      val myFuncFullName = cpg.method.name("my_func").fullName.head
      code should (
        startWith("func")
          and include regex s".*let x: Int = foo.*${Pattern.quote(CodeDumper.arrow(Option(myFuncFullName)).toString)}.*"
          and endWith("}")
      )
    }

    "allow dumping via .dump" in {
      val code = cpg.method.name("my_func").dumpRaw.mkString("\n")
      code should startWith("func my_func")
    }

    "allow dumping callIn" in {
      // With swift-build enabled, the call to `foo` resolves to its definition's fullName, so
      // `cpg.method("foo").callIn` reaches back to the call site inside `my_func`.
      implicit val resolver: ICallResolver = NoResolve
      val code                             = cpg.method.name("foo").callIn.dumpRaw.mkString("\n")
      code should startWith("func my_func")
    }

  }

}
