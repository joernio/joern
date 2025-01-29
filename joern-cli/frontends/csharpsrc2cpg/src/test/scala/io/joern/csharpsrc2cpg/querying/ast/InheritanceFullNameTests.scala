package io.joern.csharpsrc2cpg.querying.ast

import io.joern.csharpsrc2cpg.testfixtures.CSharpCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class InheritanceFullNameTests extends CSharpCode2CpgFixture {
  "inherited type full names from classes" should {
    val cpg = code("""
        |namespace HelloWorld {
        |public class Foo {
        |    public string Bar {get; set;}
        |}
        |
        |public class Bar: Foo {}
        |
        |public class Baz {
        | public void main() {
        |   Bar b = new Bar();
        | }
        |}
        |}
        |""".stripMargin)

    "resolve full names for classes with inheritance" in {
      inside(cpg.typeDecl.nameExact("Bar").l) {
        case bar :: Nil =>
          bar.fullName shouldBe "HelloWorld.Bar"
          bar.inheritsFromTypeFullName shouldBe Seq("HelloWorld.Foo")
        case _ => fail("No class named `Bar` found")
      }
    }

    "resolve identifier full names instantiated from the class" in {
      inside(cpg.identifier.nameExact("b").l) {
        case b :: Nil =>
          b.typeFullName shouldBe "HelloWorld.Bar"
        case _ => fail("No identifier named `b` found")
      }
    }
  }

  "inherited type full names from interfaces" should {
    val cpg = code("""
        |namespace HelloWorld {
        |public interface Foo {
        |    void bar();
        |    void baz();
        |}
        |
        |public interface Qux : Foo {
        |    void bazz();
        |}
        |
        |public class Fred: Qux {}
        |}
        |""".stripMargin)

    "resolve fullName and inheritanceTypeFullName for Qux" in {
      inside(cpg.typeDecl.nameExact("Qux").l) { case qux :: Nil =>
        qux.fullName shouldBe "HelloWorld.Qux"
        qux.inheritsFromTypeFullName shouldBe Seq("HelloWorld.Foo")

        inside(qux.astChildren.isMethod.l) {
          case bazz :: Nil =>
            bazz.fullName shouldBe "HelloWorld.Qux.bazz:System.Void()"
            qux.fullName shouldBe "HelloWorld.Qux"
            qux.inheritsFromTypeFullName shouldBe Seq("HelloWorld.Foo")

            inside(qux.astChildren.isMethod.l) {
              case bazz :: Nil =>
                bazz.fullName shouldBe "HelloWorld.Qux.bazz:System.Void()"
              case _ => fail("There is no method named `baz` under `Qux` interface,")
            }
          case _ => fail("There is no interface named `Qux`")
        }
      }
    }

    "resolve fullName and inheritanceTypeFullName for class inheriting from interfaces" in {
      inside(cpg.typeDecl.nameExact("Fred").l) {
        case fred :: Nil =>
          fred.fullName shouldBe "HelloWorld.Fred"
          fred.inheritsFromTypeFullName shouldBe Seq("HelloWorld.Qux")
        case _ => fail("No class named `Fred`")
      }
    }

    "class inheriting from multiple interfaces" should {
      val cpg = code("""
          |namespace HelloWorld {
          |interface IShape
          |{
          |    double GetArea();
          |}
          |
          |interface IColor
          |{
          |    string GetColor();
          |}
          |
          |class Rectangle: IShape, IColor {}
          |}
          |""".stripMargin)

      "resolve fullName and inheritanceFromTypeFullName values" in {
        inside(cpg.typeDecl.nameExact("Rectangle").l) {
          case rectangle :: Nil =>
            rectangle.fullName shouldBe "HelloWorld.Rectangle"
            rectangle.inheritsFromTypeFullName shouldBe Seq("HelloWorld.IShape", "HelloWorld.IColor")
          case _ => fail("No class named `Fred`")
        }
      }
    }

  }
}
