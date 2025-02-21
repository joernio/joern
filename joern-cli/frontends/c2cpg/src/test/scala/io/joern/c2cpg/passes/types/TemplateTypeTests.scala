package io.joern.c2cpg.passes.types

import io.joern.c2cpg.parser.FileDefaults
import io.joern.c2cpg.testfixtures.C2CpgSuite
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal

class TemplateTypeTests extends C2CpgSuite(fileSuffix = FileDefaults.CppExt) {

  "Templates" should {

    "be correct for class templates" in {
      val cpg = code("""
        |template<class T> class X {};
        |template<typename A, typename B> class Y;
        |using A = X<int>;
        |using B = Y<int, char>;
        |""".stripMargin)
      inside(cpg.typeDecl.nameNot(NamespaceTraversal.globalNamespaceName).filter(x => !x.isExternal).l) {
        case List(typeDeclX, typeDeclY, typeDeclA, typeDeclB) =>
          typeDeclX.name shouldBe "X"
          typeDeclX.fullName shouldBe "X"
          typeDeclX.aliasTypeFullName shouldBe Option("X<T>")
          typeDeclY.name shouldBe "Y"
          typeDeclY.fullName shouldBe "Y"
          typeDeclY.aliasTypeFullName shouldBe Option("Y<A,B>")
          typeDeclA.name shouldBe "A"
          typeDeclA.fullName shouldBe "A"
          typeDeclA.aliasTypeFullName shouldBe Option("X<int>")
          typeDeclB.name shouldBe "B"
          typeDeclB.fullName shouldBe "B"
          typeDeclB.aliasTypeFullName shouldBe Option("Y<int,char>")
      }
    }

    "be correct for class templates with inheritance" in {
      val cpg = code("""
        |template<typename T> class X;
        |template<typename A, typename B> class Y : public X<A> {};
        |""".stripMargin)
      inside(
        cpg.typeDecl.nameNot(NamespaceTraversal.globalNamespaceName).filter(x => !x.isExternal).sortBy(_.fullName).l
      ) { case List(x, y) =>
        x.name shouldBe "X"
        x.fullName shouldBe "X"
        x.aliasTypeFullName shouldBe Option("X<T>")
        y.name shouldBe "Y"
        y.fullName shouldBe "Y"
        y.aliasTypeFullName shouldBe Option("Y<A,B>")
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
          foo.aliasTypeFullName shouldBe Option("Foo<A,B>")
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
        x.fullName shouldBe "x:void(T,U)"
        x.signature shouldBe "void(T,U)"
        y.name shouldBe "y"
        y.fullName shouldBe "y:void(T,U)"
        y.signature shouldBe "void(T,U)"
      }
    }

  }

}
