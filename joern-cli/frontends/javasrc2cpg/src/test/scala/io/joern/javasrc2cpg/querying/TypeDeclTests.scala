package io.joern.javasrc2cpg.querying

import io.joern.javasrc2cpg.testfixtures.JavaSrcCodeToCpgFixture
import io.shiftleft.codepropertygraph.generated.nodes.Return
import io.shiftleft.semanticcpg.language._
import io.shiftleft.semanticcpg.language.types.structure.FileTraversal

import java.io.File

class TypeDeclTests extends JavaSrcCodeToCpgFixture {

  override val code: String =
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
      | """.stripMargin

  "should contain a type decl for `foo` with correct fields" in {
    val List(x) = cpg.typeDecl.name("Bar").l
    x.name shouldBe "Bar"
    x.code shouldBe "Bar"
    x.fullName shouldBe "a.b.c.d.Bar"
    x.isExternal shouldBe false
    x.inheritsFromTypeFullName should contain theSameElementsAs List("a.b.c.d.Woo")
    x.aliasTypeFullName shouldBe None
    x.order shouldBe 1
    x.filename should (
      startWith(File.separator) or // Unix
        startWith regex "[A-Z]:" // Windows
    )
    x.filename.endsWith(".java") shouldBe true
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
        interface.inheritsFromTypeFullName.toList shouldBe List()
        interface.isExternal shouldBe false
        interface.method.toList match {
          case List(method) =>
            method.name shouldBe "id"
            method.fullName shouldBe "a.b.c.d.OuterClass$InnerInterface.id:int(int)"
            method.signature shouldBe "int(int)"
            method.parameter.size shouldBe 2
          // TODO: Add and check modifiers

          case res => fail(s"Expected method id on interface but got $res")
        }

      case res => fail(s"Expected typeDecl for interface but got $res")
    }
  }

  "should create type decl for inner class implementing interface" in {
    // TODO Fix this
    cpg.typeDecl.name("OuterClass$InnerClass").l match {
      case List(innerClass) =>
        innerClass.fullName shouldBe "a.b.c.d.OuterClas$.InnerClass"
        innerClass.inheritsFromTypeFullName should contain theSameElementsAs List("Foo.OuterClass.InnerInterface")
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
