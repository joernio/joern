package io.joern.csharpsrc2cpg.querying.ast

import io.joern.csharpsrc2cpg.testfixtures.CSharpCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.ModifierTypes
import io.shiftleft.codepropertygraph.generated.nodes.Call
import io.shiftleft.semanticcpg.language.*

class PropertySetterTests extends CSharpCode2CpgFixture {

  "uninitialized set-only property declaration" should {
    val cpg = code("""
        |using System;
        |class C
        |{
        | public int MyProperty { set { Console.WriteLine(value); } }
        |}
        |""".stripMargin)

    "be lowered into a set_* method" in {
      inside(cpg.method.nameExact("set_MyProperty").l) {
        case method :: Nil =>
          method.fullName shouldBe "C.set_MyProperty:void(C,System.Int32)"
          method.signature shouldBe "void(C,System.Int32)"
        case xs => fail(s"Expected single set_MyProperty method, but got $xs")
      }
    }

    "have correct modifiers" in {
      cpg.method.nameExact("set_MyProperty").modifier.modifierType.sorted.l shouldBe List(ModifierTypes.PUBLIC)
    }

    "have correct parameters" in {
      inside(cpg.method.nameExact("set_MyProperty").parameter.sortBy(_.index).l) {
        case thisArg :: valueArg :: Nil =>
          thisArg.index shouldBe 0
          thisArg.name shouldBe "this"
          thisArg.typeFullName shouldBe "C"

          valueArg.index shouldBe 1
          valueArg.name shouldBe "value"
          valueArg.typeFullName shouldBe "System.Int32"
        case xs => fail(s"Expected two arguments to set_MyProperty, but got $xs")
      }
    }

    "have correct body" in {
      inside(cpg.method.nameExact("set_MyProperty").body.flatMap(_.astChildren).l) {
        case (writeLine: Call) :: Nil =>
          writeLine.code shouldBe "Console.WriteLine(value)"
          writeLine.methodFullName shouldBe "System.Console.WriteLine:System.Void(System.Boolean)"
        case xs => fail(s"Expected single node inside set_MyProperty's body, but got $xs")
      }
    }
  }

  "uninitialized static set-only property declaration" should {
    val cpg = code("""
        |class C
        |{
        | public static int MyProperty { set { } }
        |}
        |""".stripMargin)

    "be lowered into a set_* method" in {
      inside(cpg.method.nameExact("set_MyProperty").l) {
        case method :: Nil =>
          method.fullName shouldBe "C.set_MyProperty:void(System.Int32)"
          method.signature shouldBe "void(System.Int32)"
        case xs => fail(s"Expected single set_MyProperty method, but got $xs")
      }
    }

    "have correct modifiers" in {
      cpg.method.nameExact("set_MyProperty").modifier.modifierType.sorted.l shouldBe List(
        ModifierTypes.PUBLIC,
        ModifierTypes.STATIC
      )
    }

    "have correct parameters" in {
      inside(cpg.method.nameExact("set_MyProperty").parameter.sortBy(_.index).l) {
        case valueArg :: Nil =>
          valueArg.index shouldBe 1
          valueArg.name shouldBe "value"
          valueArg.typeFullName shouldBe "System.Int32"
        case xs => fail(s"Expected two arguments to set_MyProperty, but got $xs")
      }
    }

    "have correct body" in {
      cpg.method.nameExact("set_MyProperty").body.flatMap(_.astChildren) shouldBe empty
    }
  }

}
