package io.joern.c2cpg.io

import io.joern.c2cpg.testfixtures.C2CpgSuite
import io.shiftleft.semanticcpg.utils.FileUtil.*
import io.shiftleft.semanticcpg.codedumper.CodeDumper
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.utils.FileUtil

import java.nio.file.{Files, Paths}
import java.util.regex.Pattern

class CodeDumperFromFileTests extends C2CpgSuite {

  implicit val finder: NodeExtensionFinder = DefaultNodeExtensionFinder

  private val codeString = """
   |// A comment
   |int my_func(int param1)
   |{
   |   int x = foo(param1);
   |}""".stripMargin

  private val cpg = code(codeString, "test.c")

  private val path = Paths.get(cpg.metaData.root.head) / "test.c"

  override def beforeAll(): Unit = {
    super.beforeAll()
    // we have to restore the input file because CPG creation in C2CpgSuite
    // deletes it right after the CPG is ready.
    path.createWithParentsIfNotExists(createParents = true)
    Files.writeString(path, codeString)
  }

  override def afterAll(): Unit = {
    super.afterAll()
    FileUtil.delete(path, swallowIoExceptions = true)
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
          and include regex (s".*int x = foo.*${Pattern.quote(CodeDumper.arrow(Option("my_func")).toString)}.*")
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
