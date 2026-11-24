package io.joern.csharpsrc2cpg.querying.ast

import io.joern.csharpsrc2cpg.testfixtures.CSharpCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.{Identifier, TypeRef}
import io.shiftleft.semanticcpg.language.*

class FieldAccessTests extends CSharpCode2CpgFixture {

  "Console.WriteLine call while importing System" should {
    val cpg = code("""
        |using System;
        |Console.WriteLine("foo");
        |""".stripMargin)

    "have WriteLine call correctly set" in {
      inside(cpg.call.nameExact("WriteLine").l) { case writeLine :: Nil =>
        writeLine.code shouldBe "Console.WriteLine(\"foo\")"
        writeLine.methodFullName shouldBe "System.Console.WriteLine:System.Void(System.String)"
      }
    }

    "have foo literal correctly set" in {
      inside(cpg.call.nameExact("WriteLine").argument(1).isLiteral.l) { case foo :: Nil =>
        foo.typeFullName shouldBe "System.String"
        foo.code shouldBe "\"foo\""
      }
    }

    "have Console correctly set" in {
      inside(cpg.call.nameExact("WriteLine").argument(0).isIdentifier.l) { case console :: Nil =>
        console.code shouldBe "Console"
        console.typeFullName shouldBe "System.Console"
      }
    }
  }

  "System.Console.WriteLine call while importing System" should {
    val cpg = code("""
        |using System;
        |System.Console.WriteLine("foo");
        |""".stripMargin)

    "have WriteLine call correctly set" in {
      inside(cpg.call.nameExact("WriteLine").l) { case writeLine :: Nil =>
        writeLine.methodFullName shouldBe "System.Console.WriteLine:System.Void(System.String)"
        writeLine.code shouldBe "System.Console.WriteLine(\"foo\")"
      }
    }

    "have foo literal correctly set" in {
      inside(cpg.call.nameExact("WriteLine").argument(1).isLiteral.l) { case foo :: Nil =>
        foo.typeFullName shouldBe "System.String"
        foo.code shouldBe "\"foo\""
      }
    }

    "have System.Console correctly set" in {
      inside(cpg.call.nameExact("WriteLine").argument(0).isCall.isFieldAccess.l) { case sysConsole :: Nil =>
        sysConsole.typeFullName shouldBe "System.Console"
        sysConsole.code shouldBe "System.Console"
        sysConsole.fieldIdentifier.code.l shouldBe List("Console")
        sysConsole.fieldIdentifier.canonicalName.l shouldBe List("Console")
      }
    }
  }

  "System.Console.WriteLine call without importing System" should {
    val cpg = code("""
        |System.Console.WriteLine("foo");
        |""".stripMargin)

    "have WriteLine call correctly set" in {
      inside(cpg.call.nameExact("WriteLine").l) { case writeLine :: Nil =>
        writeLine.methodFullName shouldBe "System.Console.WriteLine:System.Void(System.String)"
        writeLine.code shouldBe "System.Console.WriteLine(\"foo\")"
      }
    }

    "have foo literal correctly set" in {
      inside(cpg.call.nameExact("WriteLine").argument(1).isLiteral.l) { case foo :: Nil =>
        foo.typeFullName shouldBe "System.String"
        foo.code shouldBe "\"foo\""
      }
    }

    "have System.Console correctly set" in {
      inside(cpg.call.nameExact("WriteLine").argument(0).isCall.isFieldAccess.l) { case sysConsole :: Nil =>
        sysConsole.typeFullName shouldBe "System.Console"
        sysConsole.code shouldBe "System.Console"
        sysConsole.fieldIdentifier.code.l shouldBe List("Console")
        sysConsole.fieldIdentifier.canonicalName.l shouldBe List("Console")
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
      inside(cpg.call("WriteLine").argument(1).isCall.isFieldAccess.l) { case fieldAccess :: Nil =>
        fieldAccess.code shouldBe "this.x"
        fieldAccess.typeFullName shouldBe "System.Int32"
        fieldAccess.methodFullName shouldBe Operators.fieldAccess
        fieldAccess.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
        fieldAccess.referencedMember.l shouldBe cpg.typeDecl.nameExact("C").member.nameExact("x").l
      }
    }
  }

  "static field access via `T.f`" should {
    val cpg = code("""
        |class T
        |{
        |  public static int f;
        |  public int g;
        |}
        |class C
        |{
        |  void M(T t)
        |  {
        |    var a = T.f;
        |    var b = t.g;
        |  }
        |}""".stripMargin)

    "have a TYPE_REF as receiver of `T.f`" in {
      inside(cpg.call.isFieldAccess.codeExact("T.f").l) { case fieldAccess :: Nil =>
        fieldAccess.fieldIdentifier.canonicalName.l shouldBe List("f")
        inside(fieldAccess.argument(1)) { case base: TypeRef =>
          base.code shouldBe "T"
          base.typeFullName shouldBe "T"
        }
      }
    }

    "still have an IDENTIFIER as receiver of the instance access `t.g`" in {
      inside(cpg.call.isFieldAccess.codeExact("t.g").l) { case fieldAccess :: Nil =>
        inside(fieldAccess.argument(1)) { case base: Identifier =>
          base.name shouldBe "t"
          base.typeFullName shouldBe "T"
        }
      }
    }
  }

  "implicit access to a static field" should {
    val cpg = code("""
        |class C
        |{
        |  static int f;
        |  int g;
        |  void M()
        |  {
        |    var a = f + 1;
        |    var b = g + 1;
        |  }
        |}""".stripMargin)

    "have a TYPE_REF as receiver for the static field and `this` for the instance field" in {
      inside(cpg.method.nameExact("M").call.isFieldAccess.l) { case staticAccess :: instanceAccess :: Nil =>
        inside(staticAccess.argument(1)) { case base: TypeRef =>
          base.typeFullName shouldBe "C"
        }
        inside(instanceAccess.argument(1)) { case base: Identifier =>
          base.name shouldBe "this"
        }
      }
    }
  }
}
