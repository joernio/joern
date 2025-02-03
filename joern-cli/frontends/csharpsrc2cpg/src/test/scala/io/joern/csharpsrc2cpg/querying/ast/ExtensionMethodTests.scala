package io.joern.csharpsrc2cpg.querying.ast

import io.joern.csharpsrc2cpg.testfixtures.CSharpCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, ModifierTypes}
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Identifier}
import io.shiftleft.semanticcpg.language.*

class ExtensionMethodTests extends CSharpCode2CpgFixture {

  "nullary extension-method declaration" should {
    val cpg = code("""
        |class MyClass {}
        |static class Extensions
        |{
        | public static void DoStuff(this MyClass myClass) {}
        |}
        |""".stripMargin)

    "have correct properties" in {
      inside(cpg.method.nameExact("DoStuff").l) {
        case doStuff :: Nil =>
          doStuff.fullName shouldBe "Extensions.DoStuff:System.Void(MyClass)"
          doStuff.signature shouldBe "System.Void(MyClass)"
          doStuff.methodReturn.typeFullName shouldBe "System.Void"
          doStuff.modifier.modifierType.toSet shouldBe Set(ModifierTypes.STATIC, ModifierTypes.PUBLIC)
        case xs => fail(s"Expected single DoStuff method, but got $xs")
      }
    }

    "have correct parameters" in {
      inside(cpg.method.nameExact("DoStuff").parameter.sortBy(_.index).l) {
        case myClass :: Nil =>
          myClass.typeFullName shouldBe "MyClass"
          myClass.code shouldBe "this MyClass myClass"
          myClass.name shouldBe "myClass"
        case xs => fail(s"Expected single parameter, but got $xs")
      }
    }
  }

  "nullary extension-method call" should {
    val cpg = code("""
        |var x = new MyClass();
        |x.DoStuff();
        |
        |class MyClass {}
        |static class Extensions
        |{
        | public static void DoStuff(this MyClass myClass) {}
        |}
        |""".stripMargin)

    "have correct properties" in {
      inside(cpg.call.nameExact("DoStuff").l) {
        case doStuff :: Nil =>
          doStuff.code shouldBe "x.DoStuff()"
          doStuff.methodFullName shouldBe "Extensions.DoStuff:System.Void(MyClass)"
          doStuff.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
        case xs => fail(s"Expected single DoStuff call, but got $xs")
      }
    }

    "have correct arguments" in {
      inside(cpg.call.nameExact("DoStuff").argument.sortBy(_.argumentIndex).l) {
        case (x: Identifier) :: Nil =>
          x.argumentIndex shouldBe 0
          x.name shouldBe "x"
          x.typeFullName shouldBe "MyClass"
          x.code shouldBe "x"
        case xs => fail(s"Expected single identifier argument to DoStuff, but got $xs")
      }
    }
  }

  "two same-named extension methods in different namespaces" should {
    val cpg = code("""
        |using Version1;
        |var x = new MyClass();
        |x.DoStuff(0);
        |
        |class MyClass {}
        |""".stripMargin)
      .moreCode("""
          |namespace Version1;
          |
          |static class Extension1
          |{
          | public static void DoStuff(this MyClass myClass, int z) {}
          |}
          |""".stripMargin)
      .moreCode("""
          |namespace Version2;
          |
          |static class Extension2
          |{
          | public static void DoStuff(this MyClass myClass, int z) {}
          |}
          |""".stripMargin)

    "find the correct extension method" in {
      inside(cpg.call.nameExact("DoStuff").l) {
        case doStuff :: Nil =>
          doStuff.code shouldBe "x.DoStuff(0)"
          doStuff.methodFullName shouldBe "Version1.Extension1.DoStuff:System.Void(MyClass,System.Int32)"
        case xs => fail(s"Expected single DoStuff call, but got $xs")
      }
    }
  }

  "two same-named extension methods involving explicit sub-types" should {

    "map to the compile-time type (1)" in {
      val cpg = code("""
          |var x = new MyConcrete();
          |x.DoStuff();
          |
          |abstract class MyAbstract;
          |class MyConcrete : MyAbstract;
          |
          |static class Extensions
          |{
          |    public static int DoStuff(this MyAbstract myAbstract) => 1;
          |    public static int DoStuff(this MyConcrete myConcrete) => 2;
          |}
          |""".stripMargin)
      cpg.call.nameExact("DoStuff").methodFullName.l shouldBe List("Extensions.DoStuff:System.Int32(MyConcrete)")
    }

    "map to the compile-time type (2)" in {
      val cpg = code("""
          |MyAbstract x = new MyConcrete();
          |x.DoStuff();
          |
          |abstract class MyAbstract;
          |class MyConcrete : MyAbstract;
          |
          |static class Extensions
          |{
          |    public static int DoStuff(this MyAbstract myAbstract) => 1;
          |    public static int DoStuff(this MyConcrete myConcrete) => 2;
          |}
          |""".stripMargin)
      cpg.call.nameExact("DoStuff").methodFullName.l shouldBe List("Extensions.DoStuff:System.Int32(MyAbstract)")
    }
  }

  "calling an extension method for `List<string>`" should {

    "resolve correctly if the receiver is of type `List<string>`" in {
      val cpg = code("""
          |using System.Collections.Generic;
          |
          |var x = new List<string>();
          |x.DoStuff();
          |
          |static class Extensions
          |{
          |    public static int DoStuff(this List<string> myList) => 1;
          |}
          |""".stripMargin)

      cpg.call.nameExact("DoStuff").methodFullName.l shouldBe List("Extensions.DoStuff:System.Int32(List)")
    }

    "resolve correctly if there's only 1 type-parametric extension for `List<T>`" in {
      val cpg = code("""
          |using System.Collections.Generic;
          |
          |var x = new List<string>();
          |x.DoStuff();
          |
          |static class Extensions
          |{
          |    public static int DoStuff<T>(this List<T> myList) => 1;
          |}
          |""".stripMargin)

      cpg.call.nameExact("DoStuff").methodFullName.l shouldBe List("Extensions.DoStuff:System.Int32(List)")
    }

    // TODO: The two `DoStuff` methods have the same methodFullName.
    "resolve correctly if there are 2 possible extensions, one for `List<string>` and another for `List<T>`" ignore {
      val cpg = code("""
          |using System.Collections.Generic;
          |
          |var x = new List<string>();
          |x.DoStuff();
          |
          |static class Extensions
          |{
          |    public static int DoStuff<T>(this List<T> myList) { return 1; }
          |    public static int DoStuff(this List<string> myList) { return 2; }
          |}
          |""".stripMargin)

      cpg.call.nameExact("DoStuff").callee.l shouldBe cpg.literal("2").method.l
    }

    "resolve correctly if the extra argument is type-compatible with the extension method's extra parameter" in {
      val cpg = code("""
          |using System.Collections.Generic;
          |
          |var x = new List<string>();
          |x.DoStuff(null);
          |
          |static class Extensions
          |{
          |    public static int DoStuff(this List<string> myList, object x) { return 2; }
          |}
          |""".stripMargin)

      cpg.call.nameExact("DoStuff").callee.l shouldBe cpg.literal("2").method.l
    }
  }

  "consecutive unary extension method calls" should {
    val cpg = code("""
        |var x = new MyClass();
        |var y = x.Foo().Bar();
        |
        |class MyClass {}
        |static class Extensions
        |{
        | public static MyClass Foo(this MyClass c) => c;
        | public static int Bar(this MyClass c) => 1;
        |}
        |""".stripMargin)

    "have correct properties and arguments" in {
      inside(cpg.call.nameExact("Bar").l) {
        case bar :: Nil =>
          bar.code shouldBe "x.Foo().Bar()"
          bar.methodFullName shouldBe "Extensions.Bar:System.Int32(MyClass)"
          bar.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
          inside(bar.argument.sortBy(_.argumentIndex).l) {
            case (foo: Call) :: Nil =>
              foo.code shouldBe "x.Foo()"
              foo.methodFullName shouldBe "Extensions.Foo:MyClass(MyClass)"
              foo.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
              inside(foo.argument.sortBy(_.argumentIndex).l) {
                case (x: Identifier) :: Nil =>
                  x.code shouldBe "x"
                  x.name shouldBe "x"
                  x.typeFullName shouldBe "MyClass"
                case xs => fail(s"Expected identifier argument to Foo, but got $xs")
              }
            case xs => fail(s"Expected single call argument to Bar, but got $xs")
          }
        case xs => fail(s"Expected single call to Bar, but got $xs")
      }
    }

    "have correct properties for the result of the chained call" in {
      cpg.assignment.target.isIdentifier.nameExact("y").typeFullName.l shouldBe List("System.Int32")
    }
  }
}
