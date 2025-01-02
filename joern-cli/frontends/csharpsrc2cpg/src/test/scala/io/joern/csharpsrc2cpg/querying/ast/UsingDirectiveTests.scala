package io.joern.csharpsrc2cpg.querying.ast

import io.joern.csharpsrc2cpg.testfixtures.CSharpCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class UsingDirectiveTests extends CSharpCode2CpgFixture {

  "`global using` directive in another file" should {
    val cpg = code("""
        |class Foo
        |{
        | static void Run()
        | {
        |   Console.WriteLine("Hello");
        | }
        |}""".stripMargin)
      .moreCode(
        """
          |global using System;
          |""".stripMargin,
        "globals.cs"
      )

    "make the imported namespace available in the current file" in {
      inside(cpg.call("WriteLine").l) {
        case writeLine :: Nil =>
          writeLine.methodFullName shouldBe "System.Console.WriteLine:System.Void(System.String)"
        case xs =>
          fail(s"Expected single WriteLine call, but found $xs")
      }
    }
  }

  "`using` directive in another file" should {
    val cpg = code("""
        |class Foo
        |{
        | static void Run()
        | {
        |   Console.WriteLine("Hello");
        | }
        |}""".stripMargin)
      .moreCode(
        """
          |using System;
          |""".stripMargin,
        "dummy.cs"
      )

    "not affect the imported namespaces in the current file" in {
      inside(cpg.call("WriteLine").l) {
        case writeLine :: Nil =>
          writeLine.methodFullName shouldBe "<unresolvedNamespace>.WriteLine:<unresolvedSignature>"
        case xs =>
          fail(s"Expected single WriteLine call, but found $xs")
      }
    }
  }
}
