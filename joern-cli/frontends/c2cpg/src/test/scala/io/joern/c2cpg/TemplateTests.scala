package io.joern.c2cpg

import io.joern.c2cpg.fixtures.CompleteCpgFixture
import io.shiftleft.semanticcpg.language._
import org.scalatest.Inside
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class TemplateTests extends AnyWordSpec with Matchers with Inside with CompleteCpgFixture {

  "Templates" should {

    "be correct for class templates" in CompleteCpgFixture("""
        |template<class T> class X {};
        |template<typename A, typename B> class Y;
        |using A = X<int>;
        |using B = Y<int, char>;
        |""".stripMargin) { cpg =>
      inside(cpg.typeDecl.nameNot("<global>").filter(x => !x.isExternal).l) { case List(x, y, a, b) =>
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

    "be correct for class templates with inheritance" in CompleteCpgFixture("""
        |template<typename T> class X;
        |template<typename A, typename B> class Y : public X<A> {};
        |""".stripMargin) { cpg =>
      inside(cpg.typeDecl.nameNot("<global>").filter(x => !x.isExternal).l) { case List(x, y) =>
        x.name shouldBe "X"
        x.fullName shouldBe "X"
        x.aliasTypeFullName shouldBe Some("X<T>")
        y.name shouldBe "Y"
        y.fullName shouldBe "Y"
        y.aliasTypeFullName shouldBe Some("Y<A,B>")
        y.inheritsFromTypeFullName shouldBe Seq("X<A>")
      }
    }

    "be correct for struct templates" in CompleteCpgFixture("""
        |template<typename A, typename B> struct Foo;
        |""".stripMargin) { cpg =>
      inside(cpg.typeDecl.nameNot("<global>").filter(x => !x.isExternal).l) { case List(foo) =>
        foo.name shouldBe "Foo"
        foo.fullName shouldBe "Foo"
        foo.aliasTypeFullName shouldBe Some("Foo<A,B>")
      }
    }

    "be correct for function templates" in CompleteCpgFixture("""
       |template<class T, class U>
       |void x(T a, U b) {};
       |
       |template<class T, class U>
       |void y(T a, U b);
       |""".stripMargin) { cpg =>
      inside(cpg.method.internal.l) { case List(_, x, y) =>
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
