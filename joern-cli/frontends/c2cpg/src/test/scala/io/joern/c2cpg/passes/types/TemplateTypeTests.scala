package io.joern.c2cpg.passes.types

import io.joern.c2cpg.parser.FileDefaults
import io.joern.c2cpg.testfixtures.CCodeToCpgSuite
import io.shiftleft.semanticcpg.language._
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal

class TemplateTypeTests extends CCodeToCpgSuite(fileSuffix = FileDefaults.CPP_EXT) {

  "Templates" should {

    "be correct for class templates" in {
      val cpg = code("""
        |template<class T> class X {};
        |template<typename A, typename B> class Y;
        |using A = X<int>;
        |using B = Y<int, char>;
        |""".stripMargin)
      inside(cpg.typeDecl.nameNot(NamespaceTraversal.globalNamespaceName).filter(x => !x.isExternal).l) {
        case List(x, y, a, b) =>
          x.name shouldBe "X"
          x.fullName shouldBe "X"
          x.aliasTypeFullName shouldBe Some("X<T>")
          y.name shouldBe "Y"
          y.fullName shouldBe "Y"
          y.aliasTypeFullName shouldBe Some("Y<A,B>")
          a.name shouldBe "A"
          a.fullName shouldBe "A"
          a.aliasTypeFullName shouldBe Some("X<int>")
          b.name shouldBe "B"
          b.fullName shouldBe "B"
          b.aliasTypeFullName shouldBe Some("Y<int, char>")
      }
    }

    "be correct for class templates with inheritance" in {
      val cpg = code("""
        |template<typename T> class X;
        |template<typename A, typename B> class Y : public X<A> {};
        |""".stripMargin)
      inside(cpg.typeDecl.nameNot(NamespaceTraversal.globalNamespaceName).filter(x => !x.isExternal).l) {
        case List(x, y) =>
          x.name shouldBe "X"
          x.fullName shouldBe "X"
          x.aliasTypeFullName shouldBe Some("X<T>")
          y.name shouldBe "Y"
          y.fullName shouldBe "Y"
          y.aliasTypeFullName shouldBe Some("Y<A,B>")
          y.inheritsFromTypeFullName shouldBe Seq("X<A>")
      }
    }

    "be correct for struct templates" in {
      val cpg = code("""
        |template<typename A, typename B> struct Foo;
        |""".stripMargin)
      inside(cpg.typeDecl.nameNot(NamespaceTraversal.globalNamespaceName).filter(x => !x.isExternal).l) {
        case List(foo) =>
          foo.name shouldBe "Foo"
          foo.fullName shouldBe "Foo"
          foo.aliasTypeFullName shouldBe Some("Foo<A,B>")
      }
    }

    "be correct for function templates" in {
      val cpg = code("""
       |template<class T, class U>
       |void x(T a, U b) {};
       |
       |template<class T, class U>
       |void y(T a, U b);
       |""".stripMargin)
      inside(cpg.method.nameNot("<global>").internal.l) { case List(x, y) =>
        x.name shouldBe "x"
        x.fullName shouldBe "x"
        x.signature shouldBe "void x<T,U> (T,U)"
        y.name shouldBe "y"
        y.fullName shouldBe "y"
        y.signature shouldBe "void y<T,U> (T,U)"
      }
    }

  }

}
