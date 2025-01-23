package io.joern.csharpsrc2cpg.querying.ast

import io.joern.csharpsrc2cpg.testfixtures.CSharpCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.ModifierTypes
import io.shiftleft.codepropertygraph.generated.nodes.Identifier
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
          doStuff.fullName shouldBe "Extensions.DoStuff:void(MyClass)"
          doStuff.signature shouldBe "void(MyClass)"
          doStuff.methodReturn.typeFullName shouldBe "void"
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
          doStuff.methodFullName shouldBe "Extensions.DoStuff:void(MyClass)"
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
          doStuff.methodFullName shouldBe "Version1.Extension1.DoStuff:void(MyClass,System.Int32)"
        case xs => fail(s"Expected single DoStuff call, but got $xs")
      }
    }
  }
}
