package io.joern.csharpsrc2cpg.querying.ast

import io.joern.csharpsrc2cpg.testfixtures.CSharpCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.ModifierTypes
import io.shiftleft.semanticcpg.language.*

class TopLevelStatementTests extends CSharpCode2CpgFixture {

  "WriteLine as a top-level statement should be found inside a fictitious method" in {
    val cpg = code("""
        |using System;
        |Console.WriteLine("Foo");
        |""".stripMargin)

    inside(cpg.call("WriteLine").method.l) { case method :: Nil =>
      method.fullName shouldBe "Test0_cs_Program.<Main>$:System.Void(System.String[])"
      method.signature shouldBe "System.Void(System.String[])"
      method.typeDecl.l shouldBe cpg.typeDecl("Test0_cs_Program").l
    }
  }

  "fictitious class name when found inside a directory" in {
    val cpg = code(
      """
        |System.Console.WriteLine(args);
        |""".stripMargin,
      "MyProject/Main.cs"
    )
    inside(cpg.call("WriteLine").method.l) { case method :: Nil =>
      method.fullName shouldBe "MyProject_Main_cs_Program.<Main>$:System.Void(System.String[])"
      method.signature shouldBe "System.Void(System.String[])"
      method.typeDecl.l shouldBe cpg.typeDecl("MyProject_Main_cs_Program").l
    }
  }

  "free-variable `args` is in fact a method parameter" in {
    val cpg = code("""
        |System.Console.WriteLine(args);
        |""".stripMargin)
    inside(cpg.parameter("args").l) { case args :: Nil =>
      args.typeFullName shouldBe "System.String[]"
      args.method.fullName shouldBe "Test0_cs_Program.<Main>$:System.Void(System.String[])"
    }
  }

  "class declaration defined after top-level statements is present" in {
    val cpg = code("""
        |System.Console.WriteLine(args);
        |class XYZ
        |{
        |}""".stripMargin)
    inside(cpg.typeDecl("XYZ").l) { case xyz :: Nil =>
      xyz.fullName shouldBe "XYZ"
    }
  }

  "top-level method becomes an inner static local method to the fictitious main" in {
    val cpg = code("""
        |void Run() {}
        |""".stripMargin)
    inside(cpg.method.nameExact("Run").l) { case run :: Nil =>
      run.methodReturn.typeFullName shouldBe "System.Void"
      run.fullName shouldBe "Test0_cs_Program.<Main>$.Run:System.Void()"
      run.modifier.modifierType.toSet shouldBe Set(ModifierTypes.STATIC, ModifierTypes.INTERNAL)
      run.parentBlock.method.l shouldBe cpg.method
        .fullNameExact("Test0_cs_Program.<Main>$:System.Void(System.String[])")
        .l
    }
  }
}
