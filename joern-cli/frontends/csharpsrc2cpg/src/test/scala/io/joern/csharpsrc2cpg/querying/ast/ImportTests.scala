package io.joern.csharpsrc2cpg.querying.ast

import io.shiftleft.semanticcpg.language.*
import io.joern.csharpsrc2cpg.testfixtures.CSharpCode2CpgFixture

class ImportTests extends CSharpCode2CpgFixture {

  "top-level using statements" should {

    val cpg = code("""
        |using System;
        |using System.Text;
        |
        |namespace HelloWorld
        |{
        |  class Program
        |  {
        |    static void Main(string[] args)
        |    {
        |      Console.WriteLine("Hey!");
        |    }
        |  }
        |
        |}
        |""".stripMargin)

    "create the respective import node for a simple base-level namespace" in {
      inside(cpg.imports.l) {
        case sysImport :: _ :: Nil =>
          sysImport.importedAs shouldBe Option("System")
          sysImport.importedEntity shouldBe Option("System")
        case _ => fail("Unexpected import node structure")
      }
    }

    "create the respective import node for a fully-qualified namespace" in {
      inside(cpg.imports.l) {
        case _ :: textImport :: Nil =>
          textImport.importedAs shouldBe Option("Text")
          textImport.importedEntity shouldBe Option("System.Text")
        case _ => fail("Unexpected import node structure")
      }
    }

    "allow for the type of `Console` to be known" in {
      inside(cpg.identifier.nameExact("Console").l) {
        case textImport :: Nil =>
          textImport.typeFullName shouldBe "System.Console"
        case _ => fail("Unexpected identifier node structure")
      }
    }

  }

}
