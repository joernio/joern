package io.joern.csharpsrc2cpg.querying.ast

import io.joern.csharpsrc2cpg.testfixtures.CSharpCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, ModifierTypes}
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Identifier, Literal}
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
      inside(cpg.call.code("System.Console.Out").l) {
        case consoleOut :: Nil =>
          consoleOut.name shouldBe "get_Out"
          consoleOut.methodFullName shouldBe "System.Console.get_Out:System.IO.TextWriter()"
          consoleOut.argument shouldBe empty
          consoleOut.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
          consoleOut.typeFullName shouldBe "System.IO.TextWriter"
        case xs => fail(s"Expected single call for System.Console.Out, but got $xs")
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
        case xs => fail(s"Expected single WriteLine call, but got $xs")
      }
    }

    "have correct arguments for WriteLine" in {
      inside(cpg.call.nameExact("WriteLine").argument.sortBy(_.argumentIndex).l) {
        case (receiver: Call) :: (literal: Literal) :: Nil =>
          receiver.argumentIndex shouldBe 0
          receiver.code shouldBe "System.Console.Out"
          receiver.name shouldBe "get_Out"
          receiver.typeFullName shouldBe "System.IO.TextWriter"

          literal.argumentIndex shouldBe 1
          literal.code shouldBe "\"X\""
          literal.typeFullName shouldBe "System.String"
        case xs => fail(s"Expected two arguments for WriteLine, but got $xs")
      }
    }

    "have correct properties for System.Console.Out" in {
      inside(cpg.call.code("System.Console.Out").l) {
        case out :: Nil =>
          out.name shouldBe "get_Out"
          out.typeFullName shouldBe "System.IO.TextWriter"
          out.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
          out.methodFullName shouldBe "System.Console.get_Out:System.IO.TextWriter()"
        case xs =>
          fail(s"Expected single call for System.Console.Out, but got $xs")
      }
    }

    "have correct arguments for System.Console.Out" in {
      cpg.call.code("System.Console.out").argument shouldBe empty
    }
  }

  "`ConsoleKeyInfo.KeyChar` being assigned to a variable" should {
    val cpg = code("""
        |using System;
        |var x = new ConsoleKeyInfo();
        |var y = x.KeyChar;
        |""".stripMargin)

    "have variable correctly typed" in {
      cpg.assignment.target.isIdentifier.nameExact("x").typeFullName.l shouldBe List("System.ConsoleKeyInfo")
      cpg.assignment.target.isIdentifier.nameExact("y").typeFullName.l shouldBe List("System.Char")
    }

    "have correct properties for KeyChar call" in {
      inside(cpg.call.code("x.KeyChar").l) {
        case keyChar :: Nil =>
          keyChar.name shouldBe "get_KeyChar"
          keyChar.methodFullName shouldBe "System.ConsoleKeyInfo.get_KeyChar:System.Char(System.ConsoleKeyInfo)"
          keyChar.typeFullName shouldBe "System.Char"
          keyChar.signature shouldBe "System.Char(System.ConsoleKeyInfo)"
        case xs => fail(s"Expected single call to KeyChar, but got $xs")
      }
    }

    "have correct arguments for KeyChar call" in {
      inside(cpg.call.code("x.KeyChar").argument.sortBy(_.argumentIndex).l) {
        case (x: Identifier) :: Nil =>
          x.typeFullName shouldBe "System.ConsoleKeyInfo"
          x.code shouldBe "x"
          x.name shouldBe "x"
          x.argumentIndex shouldBe 0
        case xs => fail(s"Expected single identifier argument to KeyChar, but got $xs")
      }
    }
  }

  "uninitialized get-only property declaration" should {
    val cpg = code("""
        |class C
        |{
        |  public int MyProperty { get; }
        |}
        |""".stripMargin)

    "be lowered into a get_* method" in {
      inside(cpg.method.nameExact("get_MyProperty").l) {
        case method :: Nil =>
          method.fullName shouldBe "C.get_MyProperty:System.Int32(C)"
          method.signature shouldBe "System.Int32(C)"
        case xs => fail(s"Expected single get_MyProperty method, but got $xs")
      }
    }

    "have correct modifiers" in {
      cpg.method.nameExact("get_MyProperty").modifier.modifierType.sorted.l shouldBe List(ModifierTypes.PUBLIC)
    }

    "have correct parameters" in {
      inside(cpg.method.nameExact("get_MyProperty").parameter.l) {
        case thisParam :: Nil =>
          thisParam.typeFullName shouldBe "C"
          thisParam.name shouldBe "this"
        case xs => fail(s"Expected this parameter for get_MyProperty, but got $xs")
      }
    }

    "have empty body" in {
      cpg.method.nameExact("get_MyProperty").body.astChildren shouldBe empty
    }
  }

  "assignment whose RHS is a get-only property declared in the source-code" should {
    val cpg = code("""
        |class C { public int MyProperty {get;} }
        |class M
        |{
        | void Run()
        | {
        |   var c = new C();
        |   var x = c.MyProperty;
        | }
        |}
        |""".stripMargin)

    "have a get_* method call on the RHS" in {
      inside(cpg.assignment.where(_.target.isIdentifier.nameExact("x")).source.l) {
        case (rhs: Call) :: Nil =>
          rhs.code shouldBe "c.MyProperty"
          rhs.name shouldBe "get_MyProperty"
          rhs.methodFullName shouldBe "C.get_MyProperty:System.Int32(C)"
          rhs.typeFullName shouldBe "System.Int32"
          rhs.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH
        case xs => fail(s"Expected single RHS call for the assignment of x, but got $xs")
      }
    }

    "have correct arguments to the get_* call" in {
      inside(cpg.call.codeExact("c.MyProperty").argument.sortBy(_.argumentIndex).l) {
        case (baseArg: Identifier) :: Nil =>
          baseArg.argumentIndex shouldBe 0
          baseArg.code shouldBe "c"
          baseArg.typeFullName shouldBe "C"
        case xs => fail(s"Expected single identifier argument to c.MyProperty, but got $xs")
      }
    }

    "have correct typeFullName for the assignment" in {
      cpg.assignment.where(_.target.isIdentifier.nameExact("x")).typeFullName.l shouldBe List("System.Int32")
    }
  }

  "assignment whose RHS is a static get-only property declared in the source-code" should {
    val cpg = code("""
        |class C { public static int MyProperty {get;} }
        |class M
        |{
        | void Run()
        | {
        |   var c = new C();
        |   var x = c.MyProperty;
        | }
        |}
        |""".stripMargin)

    "have a get_* method call on the RHS" in {
      inside(cpg.assignment.where(_.target.isIdentifier.nameExact("x")).source.l) {
        case (rhs: Call) :: Nil =>
          rhs.code shouldBe "c.MyProperty"
          rhs.name shouldBe "get_MyProperty"
          rhs.methodFullName shouldBe "C.get_MyProperty:System.Int32()"
          rhs.typeFullName shouldBe "System.Int32"
          rhs.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
        case xs => fail(s"Expected single RHS call for the assignment of x, but got $xs")
      }
    }

    "have correct arguments to the get_* call" in {
      cpg.call.codeExact("c.MyProperty").argument shouldBe empty
    }

    "have correct typeFullName for the assignment" in {
      cpg.assignment.where(_.target.isIdentifier.nameExact("x")).typeFullName.l shouldBe List("System.Int32")
    }
  }

  "uninitialized static get-only property declaration" should {
    val cpg = code("""
        |public class C
        |{
        | public static string MyProperty { get; }
        |}
        |""".stripMargin)

    "be lowered into a get_* method" in {
      inside(cpg.method.nameExact("get_MyProperty").l) {
        case method :: Nil =>
          method.fullName shouldBe "C.get_MyProperty:System.String()"
          method.signature shouldBe "System.String()"
        case xs => fail(s"Expected single get_MyProperty method, but got $xs")
      }
    }

    "have correct modifiers" in {
      cpg.method.nameExact("get_MyProperty").modifier.modifierType.sorted.l shouldBe List(
        ModifierTypes.PUBLIC,
        ModifierTypes.STATIC
      )
    }

    "have no parameters" in {
      cpg.method.nameExact("get_MyProperty").parameter shouldBe empty
    }

    "have empty body" in {
      cpg.method.nameExact("get_MyProperty").body.astChildren shouldBe empty
    }
  }
}
