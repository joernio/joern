package io.joern.csharpsrc2cpg.querying.ast

import io.joern.csharpsrc2cpg.testfixtures.CSharpCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}
import io.shiftleft.semanticcpg.language.*

class FieldAccessTests extends CSharpCode2CpgFixture {

  "Console.WriteLine call while importing System" should {
    val cpg = code("""
        |using System;
        |Console.WriteLine("foo");
        |""".stripMargin)

    "have WriteLine call correctly set" in {
      inside(cpg.call.nameExact("WriteLine").l) {
        case writeLine :: Nil =>
          writeLine.code shouldBe "Console.WriteLine(\"foo\")"
          writeLine.methodFullName shouldBe "System.Console.WriteLine:System.Void(System.String)"
        case xs => fail(s"Expected single WriteLine call, but got $xs")
      }
    }

    "have foo literal correctly set" in {
      inside(cpg.call.nameExact("WriteLine").argument(1).isLiteral.l) {
        case foo :: Nil =>
          foo.typeFullName shouldBe "System.String"
          foo.code shouldBe "\"foo\""
        case xs => fail(s"Expected single literal argument to WriteLine, but got $xs")
      }
    }

    "have Console correctly set" in {
      inside(cpg.call.nameExact("WriteLine").argument(0).isIdentifier.l) {
        case console :: Nil =>
          console.code shouldBe "Console"
          console.typeFullName shouldBe "System.Console"
        case xs => fail(s"Expected single Console identifier, but got $xs")
      }
    }
  }

  "System.Console.WriteLine call while importing System" should {
    val cpg = code("""
        |using System;
        |System.Console.WriteLine("foo");
        |""".stripMargin)

    "have WriteLine call correctly set" in {
      inside(cpg.call.nameExact("WriteLine").l) {
        case writeLine :: Nil =>
          writeLine.methodFullName shouldBe "System.Console.WriteLine:System.Void(System.String)"
          writeLine.code shouldBe "System.Console.WriteLine(\"foo\")"
        case xs => fail(s"Expected single WriteLine call, but got $xs")
      }
    }

    "have foo literal correctly set" in {
      inside(cpg.call.nameExact("WriteLine").argument(1).isLiteral.l) {
        case foo :: Nil =>
          foo.typeFullName shouldBe "System.String"
          foo.code shouldBe "\"foo\""
        case xs => fail(s"Expected single literal argument to WriteLine, but got $xs")
      }
    }

    "have System.Console correctly set" in {
      inside(cpg.call.nameExact("WriteLine").argument(0).fieldAccess.l) {
        case sysConsole :: Nil =>
          sysConsole.typeFullName shouldBe "System.Console"
          sysConsole.code shouldBe "System.Console"
          sysConsole.fieldIdentifier.code.l shouldBe List("Console")
          sysConsole.fieldIdentifier.canonicalName.l shouldBe List("Console")
        case xs => fail(s"Expected single fieldAccess to the left of WriteLine, but got $xs")
      }
    }
  }

  "System.Console.WriteLine call without importing System" should {
    val cpg = code("""
        |System.Console.WriteLine("foo");
        |""".stripMargin)

    "have WriteLine call correctly set" in {
      inside(cpg.call.nameExact("WriteLine").l) {
        case writeLine :: Nil =>
          writeLine.methodFullName shouldBe "System.Console.WriteLine:System.Void(System.String)"
          writeLine.code shouldBe "System.Console.WriteLine(\"foo\")"
        case xs => fail(s"Expected single WriteLine call, but got $xs")
      }
    }

    "have foo literal correctly set" in {
      inside(cpg.call.nameExact("WriteLine").argument(1).isLiteral.l) {
        case foo :: Nil =>
          foo.typeFullName shouldBe "System.String"
          foo.code shouldBe "\"foo\""
        case xs => fail(s"Expected single literal argument to WriteLine, but got $xs")
      }
    }

    "have System.Console correctly set" in {
      inside(cpg.call.nameExact("WriteLine").argument(0).fieldAccess.l) {
        case sysConsole :: Nil =>
          sysConsole.typeFullName shouldBe "System.Console"
          sysConsole.code shouldBe "System.Console"
          sysConsole.fieldIdentifier.code.l shouldBe List("Console")
          sysConsole.fieldIdentifier.canonicalName.l shouldBe List("Console")
        case xs => fail(s"Expected single fieldAccess to the left of WriteLine, but got $xs")
      }
    }
  }

  "field access via explicit `this.X`" should {
    val cpg = code("""
        |using System;
        |class C
        |{
        |  int x;
        |  C()
        |  {
        |   Console.WriteLine(this.x);
        |  }
        |}""".stripMargin)
    "have correct type for `this.x`" in {
      inside(cpg.call("WriteLine").argument(1).fieldAccess.l) {
        case fieldAccess :: Nil =>
          fieldAccess.code shouldBe "this.x"
          fieldAccess.typeFullName shouldBe "System.Int32"
          fieldAccess.methodFullName shouldBe Operators.fieldAccess
          fieldAccess.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
          fieldAccess.referencedMember.l shouldBe cpg.typeDecl.nameExact("C").member.nameExact("x").l
        case xs => fail(s"Expected single fieldAccess, but got $xs")
      }
    }
  }
}
