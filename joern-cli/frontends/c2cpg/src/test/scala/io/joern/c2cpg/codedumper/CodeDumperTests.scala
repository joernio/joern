package io.joern.c2cpg.codedumper

import io.joern.c2cpg.testfixtures.CCodeToCpgSuite
import io.shiftleft.semanticcpg.codedumper.CodeDumper
import io.shiftleft.semanticcpg.language._

import java.util.regex.Pattern

class CodeDumperTests extends CCodeToCpgSuite {

  implicit val finder: NodeExtensionFinder = DefaultNodeExtensionFinder

  override val code: String = """
                | // A comment
                |int my_func(int param1)
                |{
                |   int x = foo(param1);
                |}""".stripMargin

  "should return empty string for empty traversal" in {
    cpg.method.name("notinthere").dump.mkString("\n") shouldBe ""
  }

  "should be able to dump complete function" in {
    val code = cpg.method.name("my_func").dumpRaw.mkString("\n")
    code should startWith("int my_func")
    code should include("foo(param1)")
    code should endWith("}")
  }

  "should dump method with arrow for expression (a call)" in {
    val code = cpg.call.name("foo").dumpRaw.mkString("\n")
    code should startWith("int")
    code should include regex (".*" + "int x = foo" + ".*" + Pattern.quote(CodeDumper.arrow.toString) + ".*")
    code should endWith("}")
  }

  "methodCode should return nothing on invalid filename" in {
    CodeDumper.code("fooNonexisting", 1, 2) shouldBe ""
  }

  "should allow dumping via .dump" in {
    val code = cpg.method.name("my_func").dumpRaw.mkString("\n")
    code should startWith("int my_func")
  }

  "should allow dumping callIn" in {
    implicit val resolver: ICallResolver = NoResolve
    val code                             = cpg.method.name("foo").callIn.dumpRaw.mkString("\n")
    code should startWith("int")
  }

}
