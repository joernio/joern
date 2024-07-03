package io.joern.javasrc2cpg.querying

import io.joern.javasrc2cpg.testfixtures.JavaSrcCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.ModifierTypes
import io.shiftleft.codepropertygraph.generated.nodes.Return
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.types.structure.FileTraversal

import java.io.File

class NewTypeDeclTests extends JavaSrcCode2CpgFixture {
  "constructors with access modifiers" should {
    "have correct public modifier" in {
      val cpg = code("""
          |class Foo {
          |  public Foo() {}
          |}
          |""".stripMargin)

      inside(cpg.typeDecl.nameExact("Foo").method.nameExact("<init>").l) { case List(constructor) =>
        constructor.modifier.modifierType.toSet shouldBe Set(ModifierTypes.PUBLIC, ModifierTypes.CONSTRUCTOR)
      }
    }

    "have correct private modifier" in {
      val cpg = code("""
          |class Foo {
          |  private Foo() {}
          |}
          |""".stripMargin)

      inside(cpg.typeDecl.nameExact("Foo").method.nameExact("<init>").l) { case List(constructor) =>
        constructor.modifier.modifierType.toSet shouldBe Set(ModifierTypes.PRIVATE, ModifierTypes.CONSTRUCTOR)
      }
    }

    "have correct protected modifier" in {
      val cpg = code("""
          |class Foo {
          |  protected Foo() {}
          |}
          |""".stripMargin)

      inside(cpg.typeDecl.nameExact("Foo").method.nameExact("<init>").l) { case List(constructor) =>
        constructor.modifier.modifierType.toSet shouldBe Set(ModifierTypes.PROTECTED, ModifierTypes.CONSTRUCTOR)
      }
    }

    "have correct empty access modifier" in {
      val cpg = code("""
          |class Foo {
          |  Foo() {}
          |}
          |""".stripMargin)

      inside(cpg.typeDecl.nameExact("Foo").method.nameExact("<init>").l) { case List(constructor) =>
        constructor.modifier.modifierType.toSet shouldBe Set(ModifierTypes.CONSTRUCTOR)
      }
    }
  }

  "typedecls extending unresolved types available in imports should have inheritsFrom set" in {
    val cpg = code("""package io.vrooom.vulnerableapp;
        |
        |import android.content.BroadcastReceiver;
        |import android.content.Context;
        |import android.content.Intent;
        |import android.util.Log;
        |
        |public class CustomReceiver extends BroadcastReceiver {
        |    public WriteFileBroadcastReceiver() {}
        |    @Override
        |    public void onReceive(Context context, Intent intent) {
        |    }
        |}
        |""".stripMargin)
    inside(cpg.typeDecl.name("CustomReceiver").inheritsFromTypeFullName.l) { case List(name) =>
      name shouldBe "android.content.BroadcastReceiver"
    }
  }

  "the AST for an interface declaration" should {
    "not have a default constructor defined" in {
      val cpg = code("""
          |interface Foo {
          |  public void foo();
          |}
          |""".stripMargin)

      cpg.typeDecl.name("Foo").method.fullName.l shouldBe List("Foo.foo:void()")
    }

    "should have correct inheritsFromTypeFullName" in {
      val cpg = code(
        """
          |package a;
          |public class A {}
          |""".stripMargin,
        "a/A.java" // must specify "a/" to make the test pass
      ).moreCode("""
          |package a;
          |public class B extends A {}
          |""".stripMargin)
      val typeDecl = cpg.typeDecl("B").head
      typeDecl.inheritsFromTypeFullName should contain("a.A")
    }
  }

}
class TypeDeclTests extends JavaSrcCode2CpgFixture {

  val cpg = code(
    """
      | package a.b.c.d;
      | class Bar extends Woo {
      |   int x;
      |   int method () { return 1; }
      | };
      | class Woo {}
      |
      | public class OuterClass {
      |   interface InnerInterface {
      |     int id(int x);
      |   }
      |
      |   class InnerClass implements InnerInterface {
      |     @Override
      |     public int id(int x) {
      |       return x;
      |     }
      |
      |     class InnerClass2 {}
      |   }
      |
      |   public int method(int bbb) {
      |     InnerInterface innerInterface = new InnerClass();
      |     return innerInterface.id(bbb);
      |   }
      |   public static void main(String[] args) { }
      |
      | }
      |
      | enum TestEnum {
      |   A, B, C;
      |
      |   public void enumMethod() {}
      | }
      | """.stripMargin,
    fileName = "Foo.java"
  )

  "should create a default constructor if no constructor is defined" in {
    val typeFullName = "a.b.c.d.OuterClass$InnerClass$InnerClass2"

    val List(x) = cpg.typeDecl.name(".*InnerClass2").l
    x.method.size shouldBe 1
    val constructor = x.method.head

    constructor.name shouldBe io.joern.x2cpg.Defines.ConstructorMethodName
    constructor.fullName shouldBe s"$typeFullName.${io.joern.x2cpg.Defines.ConstructorMethodName}:void()"
    constructor.signature shouldBe "void()"
    constructor.modifier.map(_.modifierType).toList should contain theSameElementsAs List(
      ModifierTypes.CONSTRUCTOR,
      ModifierTypes.PUBLIC
    )

    constructor.parameter.size shouldBe 2
    constructor.parameter.name.toList shouldBe List("this", "outerClass")
    val thisParam = constructor.parameter.head
    thisParam.name shouldBe "this"
    thisParam.typeFullName shouldBe typeFullName
    thisParam.order shouldBe 0
    thisParam.index shouldBe 0
    thisParam.dynamicTypeHintFullName shouldBe List(typeFullName)

    val constructorReturn = constructor.methodReturn
    constructorReturn.typeFullName shouldBe "void"
  }

  "should contain a type decl for `foo` with correct fields" in {
    val List(x) = cpg.typeDecl.name("Bar").l
    x.name shouldBe "Bar"
    x.code shouldBe "class Bar"
    x.fullName shouldBe "a.b.c.d.Bar"
    x.isExternal shouldBe false
    x.inheritsFromTypeFullName should contain theSameElementsAs List("a.b.c.d.Woo")
    x.aliasTypeFullName shouldBe None
    x.order shouldBe 1
    x.filename shouldBe "Foo.java"
  }

  "should contain type decl for external type `int`" in {
    val List(x) = cpg.typeDecl("int").l
    x.name shouldBe "int"
    x.fullName shouldBe "int"
    x.isExternal shouldBe true
    x.inheritsFromTypeFullName.toList shouldBe List()
    x.aliasTypeFullName shouldBe None
    x.order shouldBe -1
    x.filename shouldBe FileTraversal.UNKNOWN
  }

  "should create a correct type decl for inner interface" in {
    cpg.typeDecl.nameExact("OuterClass$InnerInterface").l match {
      case List(interface) =>
        interface.fullName shouldBe "a.b.c.d.OuterClass$InnerInterface"
        interface.inheritsFromTypeFullName.toList shouldBe List("java.lang.Object")
        interface.isExternal shouldBe false
        interface.method.name("id").toList match {
          case List(method) =>
            method.fullName shouldBe "a.b.c.d.OuterClass$InnerInterface.id:int(int)"
            method.signature shouldBe "int(int)"
            method.parameter.size shouldBe 2
          // TODO: Add and check modifiers

          case res => fail(s"Expected method id on interface but got $res")
        }

      case res =>
        fail(s"Expected typeDecl for interface but got $res")
    }
  }

  "should create type decl for inner class implementing interface" in {
    cpg.typeDecl.nameExact("OuterClass$InnerClass").l match {
      case List(innerClass) =>
        innerClass.fullName shouldBe "a.b.c.d.OuterClass$InnerClass"
        innerClass.inheritsFromTypeFullName should contain theSameElementsAs List(
          "java.lang.Object",
          "a.b.c.d.OuterClass$InnerInterface"
        )
        innerClass.isExternal shouldBe false

        innerClass.method.nameExact("id").l match {
          case List(method) =>
            method.fullName shouldBe "a.b.c.d.OuterClass$InnerClass.id:int(int)"
            method.signature shouldBe "int(int)"
            method.block.astChildren.head shouldBe a[Return]
            method.block.ast.isIdentifier.head.name shouldBe "x"

          case res => fail(s"Expected id method in InnerClass but got $res")
        }

      case res => fail(s"Expected type decl for inner class but got $res")
    }
  }

}
