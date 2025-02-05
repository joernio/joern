package io.joern.csharpsrc2cpg.querying.ast

import io.joern.csharpsrc2cpg.testfixtures.CSharpCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, ModifierTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Identifier, Literal}
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

  "setting a previously declared {set{}} property via `x.Property = y` where `x` is a local variable" should {
    val cpg = code("""
        |class MyData
        |{
        |  public int MyProperty { set {} }
        |}
        |class Main
        |{
        |  public static void DoStuff()
        |  {
        |    var m = new MyData();
        |    m.MyProperty = 3; // rendered as MyData.set_MyProperty(m, 3)
        |  }
        |}
        |""".stripMargin)

    "be translated to that property's set_* method" in {
      inside(cpg.call.nameExact("set_MyProperty").l) {
        case setter :: Nil =>
          setter.code shouldBe "m.MyProperty = 3"
          setter.methodFullName shouldBe "MyData.set_MyProperty:System.Void(System.Int32)"
          setter.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH
        case xs => fail(s"Expected single call to set_MyProperty, but got $xs")
      }
    }

    "have correct arguments to the set_* method" in {
      inside(cpg.call.nameExact("set_MyProperty").argument.sortBy(_.argumentIndex).l) {
        case (m: Identifier) :: (three: Literal) :: Nil =>
          m.typeFullName shouldBe "MyData"
          m.code shouldBe "m"
          m.argumentIndex shouldBe 0
          three.code shouldBe "3"
          three.typeFullName shouldBe "System.Int32"
          three.argumentIndex shouldBe 1
        case xs => fail(s"Unexpected arguments to set_MyProperty, got $xs")
      }
    }
  }

  "setting a previously declared {get;set;} property via `x.Property = y` where `x` is method parameter" should {
    val cpg = code("""
        |class MyData
        |{
        |  public int MyProperty { get; set; }
        |}
        |class Main
        |{
        |  public static void DoStuff(MyData m)
        |  {
        |    m.MyProperty = 3; // rendered as MyData.set_MyProperty(m, 3)
        |  }
        |}
        |""".stripMargin)

    "be translated to that property's set_* method" in {
      inside(cpg.call.nameExact("set_MyProperty").l) {
        case setter :: Nil =>
          setter.code shouldBe "m.MyProperty = 3"
          setter.methodFullName shouldBe "MyData.set_MyProperty:System.Void(System.Int32)"
          setter.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH
        case xs => fail(s"Expected single call to set_MyProperty, but got $xs")
      }
    }

    "have correct arguments to the set_* method" in {
      inside(cpg.call.nameExact("set_MyProperty").argument.sortBy(_.argumentIndex).l) {
        case (m: Identifier) :: (three: Literal) :: Nil =>
          m.typeFullName shouldBe "MyData"
          m.code shouldBe "MyData m"
          m.argumentIndex shouldBe 0
          three.code shouldBe "3"
          three.typeFullName shouldBe "System.Int32"
          three.argumentIndex shouldBe 1
        case xs => fail(s"Unexpected arguments to set_MyProperty, got $xs")
      }
    }
  }

  "setting a previously declared {get;set;} property via `this.Property = y`" should {
    val cpg = code("""
        |class MyData
        |{
        |  public int MyProperty { get; set; }
        |  public void DoStuff()
        |  {
        |    this.MyProperty = 3; // rendered as MyData.set_MyProperty(this, 3)
        |  }
        |}
        |""".stripMargin)

    "be translated to that property's set_* method" in {
      inside(cpg.call.nameExact("set_MyProperty").l) {
        case setter :: Nil =>
          setter.code shouldBe "this.MyProperty = 3"
          setter.methodFullName shouldBe "MyData.set_MyProperty:System.Void(System.Int32)"
          setter.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH
        case xs => fail(s"Expected single call to set_MyProperty, but got $xs")
      }
    }

    "have correct arguments to the set_* method" in {
      inside(cpg.call.nameExact("set_MyProperty").argument.sortBy(_.argumentIndex).l) {
        case (thisId: Identifier) :: (three: Literal) :: Nil =>
          thisId.typeFullName shouldBe "MyData"
          thisId.code shouldBe "this"
          thisId.argumentIndex shouldBe 0
          three.code shouldBe "3"
          three.typeFullName shouldBe "System.Int32"
          three.argumentIndex shouldBe 1
        case xs => fail(s"Unexpected arguments to set_MyProperty, got $xs")
      }
    }
  }

  "setting a previously declared static {set{}} property via `C.Property = y` where `C` is the class name" should {
    val cpg = code("""
        |class MyData
        |{
        |  public static int MyProperty { set{} }
        |}
        |class Main
        |{
        |  public static void DoStuff()
        |  {
        |    MyData.MyProperty = 3; // rendered as MyData.set_MyProperty(3)
        |  }
        |}
        |""".stripMargin)

    "be translated to that property's set_* method" in {
      inside(cpg.call.nameExact("set_MyProperty").l) {
        case setter :: Nil =>
          setter.code shouldBe "MyData.MyProperty = 3"
          setter.methodFullName shouldBe "MyData.set_MyProperty:System.Void(System.Int32)"
          setter.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
        case xs => fail(s"Expected single call to set_MyProperty, but got $xs")
      }
    }

    "have correct arguments to the set_* method" in {
      inside(cpg.call.nameExact("set_MyProperty").argument.sortBy(_.argumentIndex).l) {
        case (three: Literal) :: Nil =>
          three.code shouldBe "3"
          three.typeFullName shouldBe "System.Int32"
          three.argumentIndex shouldBe 1
        case xs => fail(s"Unexpected arguments to set_MyProperty, got $xs")
      }
    }
  }

  "setting a previously declared {get;set;} property via `x.Property += y` where `x` is a local variable" should {
    val cpg = code("""
        |class MyData
        |{
        |  public int MyProperty { get; set; }
        |}
        |class Main
        |{
        |  public static void DoStuff()
        |  {
        |    var m = new MyData();
        |    m.MyProperty += 3; // rendered as MyData.set_MyProperty(m, MyData.get_MyProperty() + 3)
        |  }
        |}
        |""".stripMargin)

    "be translated to that property's set_* method" in {
      inside(cpg.call.nameExact("set_MyProperty").l) {
        case setter :: Nil =>
          setter.code shouldBe "m.MyProperty += 3"
          setter.name shouldBe "set_MyProperty"
          setter.methodFullName shouldBe "MyData.set_MyProperty:System.Void(System.Int32)"
          setter.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH
        case xs => fail(s"Expected single call to set_MyProperty, but got $xs")
      }
    }

    "have correct arguments to the set_* method" in {
      inside(cpg.call.nameExact("set_MyProperty").argument.sortBy(_.argumentIndex).l) {
        case (m: Identifier) :: (plusCall: Call) :: Nil =>
          m.typeFullName shouldBe "MyData"
          m.code shouldBe "m"
          m.argumentIndex shouldBe 0

          plusCall.argumentIndex shouldBe 1
          plusCall.code shouldBe "m.MyProperty += 3"
          plusCall.methodFullName shouldBe Operators.plus
          plusCall.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
        case xs => fail(s"Unexpected arguments to set_MyProperty, got $xs")
      }
    }

    "have correct arguments to the synthetic `+` call" in {
      inside(cpg.call.nameExact("set_MyProperty").argument(1).isCall.argument.sortBy(_.argumentIndex).l) {
        case (getter: Call) :: (three: Literal) :: Nil =>
          getter.argumentIndex shouldBe 1
          getter.code shouldBe "m.MyProperty += 3"
          getter.methodFullName shouldBe "MyData.get_MyProperty:System.Int32()"
          getter.name shouldBe "get_MyProperty"
          getter.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH

          three.argumentIndex shouldBe 2
          three.code shouldBe "3"
          three.typeFullName shouldBe "System.Int32"
        case xs => fail(s"Expected two arguments for +, but got $xs")
      }
    }

    "have correct arguments to the synthetic getter call" in {
      inside(cpg.call.nameExact("get_MyProperty").argument.sortBy(_.argumentIndex).l) {
        case (receiver: Identifier) :: Nil =>
          receiver.argumentIndex shouldBe 0
          receiver.typeFullName shouldBe "MyData"
          receiver.code shouldBe "m"
          receiver.name shouldBe "m"
        case xs => fail(s"Expected single argument to get_MyProperty, but got $xs")
      }
    }
  }

  "setting a previously declared static {get;set;} property via `C.Property += y` where `C` is the class name" should {
    val cpg = code("""
        |class MyData
        |{
        |  public static int MyProperty { get; set; }
        |}
        |class Main
        |{
        |  public static void DoStuff()
        |  {
        |    MyData.MyProperty += 3; // rendered as MyData.set_MyProperty(MyData.get_MyProperty() + 3)
        |  }
        |}
        |""".stripMargin)

    "be translated to that property's set_* method" in {
      inside(cpg.call.nameExact("set_MyProperty").l) {
        case setter :: Nil =>
          setter.code shouldBe "MyData.MyProperty += 3"
          setter.name shouldBe "set_MyProperty"
          setter.methodFullName shouldBe "MyData.set_MyProperty:System.Void(System.Int32)"
          setter.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
        case xs => fail(s"Expected single call to set_MyProperty, but got $xs")
      }
    }

    "have correct arguments to the set_* method" in {
      inside(cpg.call.nameExact("set_MyProperty").argument.sortBy(_.argumentIndex).l) {
        case (plusCall: Call) :: Nil =>
          plusCall.argumentIndex shouldBe 1
          plusCall.code shouldBe "MyData.MyProperty += 3"
          plusCall.methodFullName shouldBe Operators.plus
          plusCall.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
        case xs => fail(s"Unexpected arguments to set_MyProperty, got $xs")
      }
    }

    "have correct arguments to the synthetic `+` call" in {
      inside(cpg.call.nameExact("set_MyProperty").argument(1).isCall.argument.sortBy(_.argumentIndex).l) {
        case (getter: Call) :: (three: Literal) :: Nil =>
          getter.argumentIndex shouldBe 1
          getter.code shouldBe "MyData.MyProperty += 3"
          getter.methodFullName shouldBe "MyData.get_MyProperty:System.Int32()"
          getter.name shouldBe "get_MyProperty"
          getter.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH

          three.argumentIndex shouldBe 2
          three.code shouldBe "3"
          three.typeFullName shouldBe "System.Int32"
        case xs => fail(s"Expected two arguments for +, but got $xs")
      }
    }

    "have correct arguments to the synthetic getter call" in {
      cpg.call.nameExact("get_MyProperty").argument shouldBe empty
    }
  }
}
