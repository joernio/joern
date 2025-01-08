package io.joern.csharpsrc2cpg.querying.ast

import io.joern.csharpsrc2cpg.testfixtures.CSharpCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class PropertyGetterTests extends CSharpCode2CpgFixture {

  "`System.Console.Out` being assigned to a variable" should {
    val cpg = code("""
        |using System;
        |var x = System.Console.Out;
        |""".stripMargin)

    "have variable correctly typed" in {
      cpg.identifier.nameExact("x").typeFullName.l shouldBe List("System.IO.TextWriter")
    }

    "have System.Console.Out correctly set" in {
      inside(cpg.fieldAccess.code("System.Console.Out").l) {
        case consoleOut :: Nil =>
          consoleOut.typeFullName shouldBe "System.IO.TextWriter"
          consoleOut.fieldIdentifier.canonicalName.l shouldBe List("Out")
        case xs =>
          fail(s"Expected single fieldAccess for Console.Out, but got $xs")
      }
    }
  }

  "`System.Console.Out.WriteLine` call" should {
    val cpg = code("""
        |using System;
        |using System.IO;
        |System.Console.Out.WriteLine("X");
        |""".stripMargin)

    "have correct properties for WriteLine" in {
      inside(cpg.call.nameExact("WriteLine").l) {
        case writeLine :: Nil =>
          writeLine.code shouldBe "System.Console.Out.WriteLine(\"X\")"
          writeLine.methodFullName shouldBe "System.IO.TextWriter.WriteLine:System.Void(System.String)"
          writeLine.typeFullName shouldBe "System.Void"
        case xs =>
          fail(s"Expected single WriteLine call, but got $xs")
      }
    }

    "have correct properties for System.Console" in {
      inside(cpg.fieldAccess.code("System.Console").l) {
        case sysConsole :: Nil =>
          sysConsole.fieldIdentifier.canonicalName.l shouldBe List("Console")
          sysConsole.typeFullName shouldBe "System.Console"
        case xs =>
          fail(s"Expected single fieldAccess for System.Console, but got $xs")
      }
    }

    "have correct properties for System.Console.Out" in {
      inside(cpg.fieldAccess.code("System.Console.Out").l) {
        case out :: Nil =>
          out.code shouldBe "System.Console.Out"
          out.typeFullName shouldBe "System.IO.TextWriter"
        case xs =>
          fail(s"Expected single fieldAccess for System.Console.Out, but got $xs")
      }
    }
  }

}
