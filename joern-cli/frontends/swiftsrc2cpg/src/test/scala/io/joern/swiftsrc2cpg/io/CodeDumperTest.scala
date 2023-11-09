package io.joern.swiftsrc2cpg.io

import better.files.File
import io.joern.swiftsrc2cpg.testfixtures.SwiftSrc2CpgSuite
import io.shiftleft.semanticcpg.codedumper.CodeDumper
import io.shiftleft.semanticcpg.language._

import java.util.regex.Pattern

class CodeDumperTest extends SwiftSrc2CpgSuite {

  implicit val finder: NodeExtensionFinder = DefaultNodeExtensionFinder

  private val codeString = """
   |// A comment
   |func greet(person: String) -> String {
   |  let greeting = "Hello, " + person + "!"
   |  foo()
   |  return greeting
   |}
   |""".stripMargin

  private val cpg = code(codeString, "main.swift")

  private val path = File(cpg.metaData.root.head) / "main.swift"

  override def beforeAll(): Unit = {
    super.beforeAll()
    path.createFileIfNotExists(createParents = true)
    path.writeText(codeString)
  }

  override def afterAll(): Unit = {
    super.afterAll()
    path.delete(swallowIOExceptions = true)
  }

  "dumping code" should {

    "return empty string for empty traversal" in {
      cpg.method.name("notinthere").dump shouldBe empty
    }

    "be able to dump complete function" ignore {
      val code = cpg.method.name("greet").dumpRaw.mkString("\n")
      code should (
        startWith("func greet")
          and include("let greeting")
          and endWith("}")
      )
    }

    "dump method with arrow for expression (a call)" ignore {
      val code = cpg.call.name("foo").dumpRaw.mkString("\n")
      code should (
        startWith("func greet")
          and include regex s".*foo.*${Pattern.quote(CodeDumper.arrow(Option("main.swift:<global>:greet")).toString)}.*"
          and endWith("}")
      )
    }

    "allow dumping via .dump" ignore {
      val code = cpg.method.name("greet").dumpRaw.mkString("\n")
      code should startWith("func greet")
    }

  }

}
