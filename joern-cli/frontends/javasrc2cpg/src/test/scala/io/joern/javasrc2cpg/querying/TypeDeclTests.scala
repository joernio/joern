package io.joern.javasrc2cpg.querying

import io.joern.javasrc2cpg.testfixtures.JavaSrcCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.{ModifierTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.{Block, Call, Identifier, Literal, Return, TypeDecl}
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.types.structure.FileTraversal

class NewTypeDeclTests extends JavaSrcCode2CpgFixture {
  "a Java 25 compact class declaration" should {
    val cpg = code("""
        |String s = "hello";
        |
        |void main(ArrayList<String> list) {
        |  list.add(s);
        |}
        |""".stripMargin)

    "contain a typeDecl with the correct fields set for the compact class" in {
      inside(cpg.typeDecl.internal.l) { case List(compactDecl) =>
        compactDecl.name shouldBe "$COMPACT_CLASS"
        compactDecl.inheritsFromTypeFullName.l shouldBe List("java.lang.Object")
      }
    }

    "contain a member node for s" in {
      inside(cpg.member.name("s").l) { case List(sMember) =>
        sMember.name shouldBe "s"
        sMember.typeFullName shouldBe "java.lang.String"

        sMember.astParent.isInstanceOf[TypeDecl] shouldBe true
        sMember.astParent.asInstanceOf[TypeDecl].fullName shouldBe "$COMPACT_CLASS"
      }
    }

    "contain the main method" in {
      inside(cpg.method("main").l) { case List(mainMethod) =>
        mainMethod.fullName shouldBe "$COMPACT_CLASS.main:void(java.util.ArrayList)"
        inside(mainMethod.parameter.index(1).l) { case List(listParam) =>
          listParam.typeFullName shouldBe "java.util.ArrayList"
        }
      }
    }
  }

  "a Java 25 compact class declaration with a package declaration" should {
    val cpg = code("""
        |package foo.bar;
        |
        |String s = "hello";
        |
        |void main(ArrayList<String> list) {
        |  list.add(s);
        |}
        |""".stripMargin)

    "contain a typeDecl with the correct fields set for the compact class" in {
      inside(cpg.typeDecl.internal.l) { case List(compactDecl) =>
        compactDecl.name shouldBe "$COMPACT_CLASS"
        compactDecl.fullName shouldBe "foo.bar.$COMPACT_CLASS"
        compactDecl.inheritsFromTypeFullName.l shouldBe List("java.lang.Object")
      }
    }

    "contain a member node for s" in {
      inside(cpg.member.name("s").l) { case List(sMember) =>
        sMember.typeFullName shouldBe "java.lang.String"
        sMember.astParent.isInstanceOf[TypeDecl] shouldBe true
        sMember.astParent.asInstanceOf[TypeDecl].fullName shouldBe "foo.bar.$COMPACT_CLASS"
      }
    }

    "contain the main method with the correct full name" in {
      inside(cpg.method("main").l) { case List(mainMethod) =>
        mainMethod.fullName shouldBe "foo.bar.$COMPACT_CLASS.main:void(java.util.ArrayList)"
        inside(mainMethod.parameter.index(1).l) { case List(listParam) =>
          listParam.typeFullName shouldBe "java.util.ArrayList"
        }
      }
    }
  }

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

  "type decl code" should {
    "include abstract modifier and type parameters" in {
      val cpg = code("""
          |class BaseReq {}
          |public abstract class GenericService<T extends BaseReq, R> {}
          |""".stripMargin)

      cpg.typeDecl.nameExact("GenericService").code.l shouldBe
        List("public abstract class GenericService<T extends BaseReq, R>")
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

  "a package-private class" should {
    val cpg = code("class Foo {}")

    "have no modifier" in {
      cpg.typeDecl.name("Foo").modifier.modifierType.toSet shouldBe Set()
    }

    "have the correct code field" in {
      cpg.typeDecl.name("Foo").code.l shouldBe List("class Foo")
    }
  }

  "a public class" should {
    val cpg = code("public class Foo {}")

    "have the correct modifier" in {
      cpg.typeDecl.name("Foo").modifier.modifierType.toSet shouldBe Set("PUBLIC")
    }

    "have the correct code field" in {
      cpg.typeDecl.name("Foo").code.l shouldBe List("public class Foo")
    }
  }

  "a protected class" should {
    val cpg = code("class Outer { protected class Foo {} }")

    "have the correct modifier" in {
      cpg.typeDecl.name(".*Foo").modifier.modifierType.toSet shouldBe Set("PROTECTED")
    }

    "have the correct code field" in {
      cpg.typeDecl.name(".*Foo").code.l shouldBe List("protected class Foo")
    }
  }

  "a private class" should {
    val cpg = code("class Outer { private class Foo {} }")

    "have the correct modifier" in {
      cpg.typeDecl.name(".*Foo").modifier.modifierType.toSet shouldBe Set("PRIVATE")
    }

    "have the correct code field" in {
      cpg.typeDecl.name(".*Foo").code.l shouldBe List("private class Foo")
    }
  }

  "an abstract class" should {
    val cpg = code("abstract class Foo {}")

    "have the correct modifier" in {
      cpg.typeDecl.name("Foo").modifier.modifierType.toSet shouldBe Set("ABSTRACT")
    }

    "have the correct code field" in {
      cpg.typeDecl.name("Foo").code.l shouldBe List("abstract class Foo")
    }
  }

  "a static class" should {
    val cpg = code("class Outer { static class Foo {} }")

    "have the correct modifier" in {
      cpg.typeDecl.name(".*Foo").modifier.modifierType.toSet shouldBe Set("STATIC")
    }

    "have the correct code field" in {
      cpg.typeDecl.name(".*Foo").code.l shouldBe List("static class Foo")
    }
  }

  "a final class" should {
    val cpg = code("final class Foo {}")

    "have the correct modifier" in {
      cpg.typeDecl.name("Foo").modifier.modifierType.toSet shouldBe Set("FINAL")
    }

    "have the correct code field" in {
      cpg.typeDecl.name("Foo").code.l shouldBe List("final class Foo")
    }
  }

  "a sealed class" should {
    val cpg = code("sealed class Foo {}")

    "have the correct modifier" in {
      cpg.typeDecl.name("Foo").modifier.modifierType.toSet shouldBe Set("SEALED")
    }

    "have the correct code field" in {
      cpg.typeDecl.name("Foo").code.l shouldBe List("sealed class Foo")
    }
  }

  "a non-sealed class" should {
    val cpg = code("non-sealed class Foo {}")

    "have the correct modifier" in {
      cpg.typeDecl.name("Foo").modifier.modifierType.toSet shouldBe Set("NON_SEALED")
    }

    "have the correct code field" in {
      cpg.typeDecl.name("Foo").code.l shouldBe List("non-sealed class Foo")
    }
  }

  "a strictfp class" should {
    val cpg = code("strictfp class Foo {}")

    "have the correct modifier" in {
      cpg.typeDecl.name("Foo").modifier.modifierType.toSet shouldBe Set("STRICTFP")
    }

    "have the correct code field" in {
      cpg.typeDecl.name("Foo").code.l shouldBe List("strictfp class Foo")
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
    inside(cpg.typeDecl.nameExact("OuterClass$InnerInterface").l) { case List(interface) =>
      interface.fullName shouldBe "a.b.c.d.OuterClass$InnerInterface"
      interface.inheritsFromTypeFullName.toList shouldBe List("java.lang.Object")
      interface.isExternal shouldBe false
      inside(interface.method.name("id").toList) { case List(method) =>
        method.fullName shouldBe "a.b.c.d.OuterClass$InnerInterface.id:int(int)"
        method.signature shouldBe "int(int)"
        method.parameter.size shouldBe 2
      // TODO: Add and check modifiers
      }
    }
  }

  "should create type decl for inner class implementing interface" in {
    inside(cpg.typeDecl.nameExact("OuterClass$InnerClass").l) { case List(innerClass) =>
      innerClass.fullName shouldBe "a.b.c.d.OuterClass$InnerClass"
      innerClass.inheritsFromTypeFullName should contain theSameElementsAs List(
        "java.lang.Object",
        "a.b.c.d.OuterClass$InnerInterface"
      )
      innerClass.isExternal shouldBe false

      inside(innerClass.method.nameExact("id").l) { case List(method) =>
        method.fullName shouldBe "a.b.c.d.OuterClass$InnerClass.id:int(int)"
        method.signature shouldBe "int(int)"
        method.block.astChildren.head shouldBe a[Return]
        method.block.ast.isIdentifier.head.name shouldBe "x"
      }
    }
  }

  "a static initialiser block" should {
    val cpg = code("""
        |package foo;
        |
        |public class Foo {
        |    static { staticInit(); }
        |
        |    static void staticInit() {}
        |}
        |""".stripMargin)

    "be added to the <clinit> method of the type decl" in {
      inside(cpg.typeDecl.nameExact("Foo").method.nameExact(io.joern.x2cpg.Defines.StaticInitMethodName).l) {
        case List(clinit) =>
          clinit.fullName shouldBe s"foo.Foo.${io.joern.x2cpg.Defines.StaticInitMethodName}:void()"
          inside(clinit.body.astChildren.l) { case List(staticInitBlock: Block) =>
            inside(staticInitBlock.astChildren.l) { case List(staticInitCall: Call) =>
              staticInitCall.code shouldBe "staticInit()"
              staticInitCall.methodFullName shouldBe "foo.Foo.staticInit:void()"
            }
          }
      }
    }

    "not be added to the <init> method" in {
      cpg.typeDecl
        .nameExact("Foo")
        .method
        .nameExact(io.joern.x2cpg.Defines.ConstructorMethodName)
        .call
        .nameExact("staticInit")
        .size shouldBe 0
    }
  }

  "a non-static initialiser block" should {
    val cpg = code("""
        |package foo;
        |
        |public class Foo {
        |    { instanceInit(); }
        |
        |    void instanceInit() {}
        |}
        |""".stripMargin)

    "be added to the <init> method of the type decl" in {
      inside(cpg.typeDecl.nameExact("Foo").method.nameExact(io.joern.x2cpg.Defines.ConstructorMethodName).l) {
        case List(init) =>
          init.fullName shouldBe s"foo.Foo.${io.joern.x2cpg.Defines.ConstructorMethodName}:void()"
          inside(init.body.astChildren.isBlock.l) { case List(instanceInitBlock) =>
            inside(instanceInitBlock.astChildren.l) { case List(instanceInitCall: Call) =>
              instanceInitCall.code shouldBe "this.instanceInit()"
              instanceInitCall.methodFullName shouldBe "foo.Foo.instanceInit:void()"
            }
          }
      }
    }

    "not create a <clinit> method" in {
      cpg.typeDecl.nameExact("Foo").method.nameExact(io.joern.x2cpg.Defines.StaticInitMethodName).size shouldBe 0
    }
  }

  "an initialiser block in an anonymous class" should {
    val cpg = code("""
        |package foo;
        |
        |import java.util.HashMap;
        |import java.util.Map;
        |
        |public class Foo {
        |    Map<String, Integer> m = new HashMap<String, Integer>() {{ put("a", 1); }};
        |}
        |""".stripMargin)

    "be added to the <init> method of the anonymous type decl" in {
      inside(
        cpg.typeDecl
          .fullNameExact("foo.Foo.m.HashMap$0")
          .method
          .nameExact(io.joern.x2cpg.Defines.ConstructorMethodName)
          .l
      ) { case List(init) =>
        inside(init.body.astChildren.isBlock.l) { case List(anonInitBlock) =>
          inside(anonInitBlock.astChildren.l) { case List(putCall: Call) =>
            putCall.code shouldBe "this.put(\"a\", 1)"
            putCall.name shouldBe "put"
          }
        }
      }
    }

    "not be added to the <init> method of the enclosing type decl" in {
      cpg.typeDecl
        .nameExact("Foo")
        .method
        .nameExact(io.joern.x2cpg.Defines.ConstructorMethodName)
        .call
        .nameExact("put")
        .size shouldBe 0
    }
  }

  "static field initialisers interleaved with a static initialiser block" should {
    val cpg = code("""
        |package foo;
        |
        |public class Foo {
        |    static int a = 1;
        |
        |    static { staticInit(); }
        |
        |    static int b = 2;
        |
        |    static void staticInit() {}
        |}
        |""".stripMargin)

    "be added to the <clinit> method in source order" in {
      inside(cpg.typeDecl.nameExact("Foo").method.nameExact(io.joern.x2cpg.Defines.StaticInitMethodName).l) {
        case List(clinit) =>
          inside(clinit.body.astChildren.l) { case List(aAssign: Call, initBlock: Block, bAssign: Call) =>
            aAssign.name shouldBe Operators.assignment
            inside(aAssign.argument.l) { case List(aFieldAccess: Call, oneLiteral: Literal) =>
              aFieldAccess.name shouldBe Operators.fieldAccess
              aFieldAccess.code shouldBe "Foo.a"
              oneLiteral.code shouldBe "1"
            }

            inside(initBlock.astChildren.l) { case List(staticInitCall: Call) =>
              staticInitCall.code shouldBe "staticInit()"
            }

            bAssign.name shouldBe Operators.assignment
            inside(bAssign.argument.l) { case List(bFieldAccess: Call, twoLiteral: Literal) =>
              bFieldAccess.name shouldBe Operators.fieldAccess
              bFieldAccess.code shouldBe "Foo.b"
              twoLiteral.code shouldBe "2"
            }
          }
      }
    }
  }

  "instance field initialisers interleaved with an instance initialiser block" should {
    val cpg = code("""
        |package foo;
        |
        |public class Foo {
        |    int a = 1;
        |
        |    { instanceInit(); }
        |
        |    int b = 2;
        |
        |    void instanceInit() {}
        |}
        |""".stripMargin)

    "be added to the <init> method in source order" in {
      inside(cpg.typeDecl.nameExact("Foo").method.nameExact(io.joern.x2cpg.Defines.ConstructorMethodName).l) {
        case List(init) =>
          val initialisers = init.body.astChildren.l.filter {
            case call: Call => call.name == Operators.assignment
            case _: Block   => true
            case _          => false
          }

          inside(initialisers) { case List(aAssign: Call, initBlock: Block, bAssign: Call) =>
            inside(aAssign.argument.l) { case List(aFieldAccess: Call, oneLiteral: Literal) =>
              aFieldAccess.name shouldBe Operators.fieldAccess
              aFieldAccess.code shouldBe "this.a"
              oneLiteral.code shouldBe "1"
            }

            inside(initBlock.astChildren.l) { case List(instanceInitCall: Call) =>
              instanceInitCall.code shouldBe "this.instanceInit()"
            }

            inside(bAssign.argument.l) { case List(bFieldAccess: Call, twoLiteral: Literal) =>
              bFieldAccess.name shouldBe Operators.fieldAccess
              bFieldAccess.code shouldBe "this.b"
              twoLiteral.code shouldBe "2"
            }
          }
      }
    }
  }

}
