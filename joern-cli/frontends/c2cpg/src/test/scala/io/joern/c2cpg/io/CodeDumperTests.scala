package io.joern.c2cpg.io

import better.files.File
import io.joern.c2cpg.testfixtures.CCodeToCpgSuite
import io.shiftleft.semanticcpg.codedumper.CodeDumper
import io.shiftleft.semanticcpg.language._

import java.util.regex.Pattern

class CodeDumperTests extends CCodeToCpgSuite {

  implicit val finder: NodeExtensionFinder = DefaultNodeExtensionFinder

  private val codeString = """
   |// A comment
   |int my_func(int param1)
   |{
   |   int x = foo(param1);
   |}""".stripMargin

  private val cpg = code(codeString, "test.c")

  private val path = File(cpg.metaData.root.head) / "test.c"

  override def beforeAll(): Unit = {
    super.beforeAll()
    // we have to restore the input file because CPG creation in CCodeToCpgSuite
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
        startWith("int my_func")
          and include("foo(param1)")
          and endWith("}")
      )
    }

    "dump method with arrow for expression (a call)" in {
      val code = cpg.call.name("foo").dumpRaw.mkString("\n")
      code should (
        startWith("int")
          and include regex (".*" + "int x = foo" + ".*" + Pattern.quote(
            CodeDumper.arrow(Option("my_func")).toString
          ) + ".*")
          and endWith("}")
      )
    }

    "methodCode should return nothing on invalid filename" in {
      CodeDumper.code("fooNonexisting", 1, 2) shouldBe empty
    }

    "allow dumping via .dump" in {
      val code = cpg.method.name("my_func").dumpRaw.mkString("\n")
      code should startWith("int my_func")
    }

    "allow dumping callIn" in {
      implicit val resolver: ICallResolver = NoResolve
      val code                             = cpg.method.name("foo").callIn.dumpRaw.mkString("\n")
      code should startWith("int")
    }
  }

}
