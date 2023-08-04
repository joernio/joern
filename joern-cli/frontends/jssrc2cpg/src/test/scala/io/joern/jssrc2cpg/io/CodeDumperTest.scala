package io.joern.jssrc2cpg.io

import better.files.File
import io.joern.jssrc2cpg.testfixtures.JsSrc2CpgSuite
import io.shiftleft.semanticcpg.codedumper.CodeDumper
import io.shiftleft.semanticcpg.language._

import java.util.regex.Pattern

class CodeDumperTest extends JsSrc2CpgSuite {

  implicit val finder: NodeExtensionFinder = DefaultNodeExtensionFinder

  private val codeString = """
   |// A comment
   |function my_func(param1)
   |{
   |   var x = foo(param1);
   |}""".stripMargin

  private val cpg = code(codeString, "index.js")

  private val path = File(cpg.metaData.root.head) / "index.js"

  override def beforeAll(): Unit = {
    super.beforeAll()
    // we have to restore the input file because CPG creation in JsSrc2CpgSuite
    // deletes it right after the CPG is ready.
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

    "be able to dump complete function" in {
      val code = cpg.method.name("my_func").dumpRaw.mkString("\n")
      code should (
        startWith("function my_func")
          and include("foo(param1)")
          and endWith("}")
      )
    }

    "dump method with arrow for expression (a call)" in {
      val code = cpg.call.name("foo").dumpRaw.mkString("\n")
      code should (
        startWith("function")
          and include regex s".*var x = foo.*${Pattern.quote(CodeDumper.arrow(Option("index.js::program:my_func")).toString)}.*"
          and endWith("}")
      )
    }

    "allow dumping via .dump" in {
      val code = cpg.method.name("my_func").dumpRaw.mkString("\n")
      code should startWith("function my_func")
    }

  }

}
