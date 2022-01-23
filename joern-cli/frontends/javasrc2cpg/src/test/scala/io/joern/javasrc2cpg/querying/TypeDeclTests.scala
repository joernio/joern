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

}
