package io.joern.jimple2cpg.querying

import io.joern.jimple2cpg.testfixtures.JimpleCode2CpgFixture
import io.shiftleft.semanticcpg.language.{NoResolve, _}

class DynamicCallGraphTests extends JimpleCode2CpgFixture {

  implicit val resolver: NoResolve.type = NoResolve

  "call from a nested super-class" should {
    val cpg = code("""
        |class Foo {
        |
        |      public static void main(String[] args){
        |          A b1 = new B();
        |          A c1 = new C();
        |
        |          A b2 = b1;
        |          A c2 = c1;
        |
        |          // what will get printed?
        |          b2.print(c2);
        |      }
        |
        |      public static class A extends Object {
        |          public void print(A object) {
        |              System.out.println("An instance of " + object.getClass().getSimpleName()
        |                      + " was passed to A's print(A object)");
        |          }
        |      }
        |
        |      public static class B extends A {
        |          public void print(A object) {
        |              System.out.println("An instance of " + object.getClass().getSimpleName()
        |                      + " was passed to B's print(A object)");
        |          }
        |      }
        |
        |      public static class C extends B {
        |          public void print(A object) {
        |              System.out.println("An instance of " + object.getClass().getSimpleName()
        |                      + " was passed to C's print(A object)");
        |          }
        |      }
        |
        |      public static class D extends A {
        |          public void print(A object) {
        |              System.out.println("An instance of " + object.getClass().getSimpleName()
        |                      + " was passed to D's print(A object)");
        |          }
        |      }
        |
        |}
        |""".stripMargin).cpg

    "find that add is called by main" in {
      cpg.method.name("print").caller.name.toSetMutable shouldBe Set("main")
    }

    "account for print calls from all subclasses due to using CHA" in {
      cpg.call.name("print").callee.definingTypeDecl.fullName.toSetMutable shouldBe Set(
        "Foo$D",
        "Foo$B",
        "Foo$C",
        "Foo$A"
      )
    }
  }

  "call from a method not overridden in a child class" should {
    implicit val resolver: NoResolve.type = NoResolve
    val cpg = code("""
        |class Foo {
        |
        |  private int x = 0;
        |
        |  public int foo(int a) {
        |    this.x = a;
        |    return this.x;
        |  }
        |}
        |
        |class Bar extends Foo {
        |
        |  public int bar(int b) {
        |    return foo(b);
        |  }
        |
        |}
        |""".stripMargin).cpg

    "find that foo is still called with the derived full name" in {
      cpg.call.name("foo").methodFullName.toSetMutable shouldBe Set("Bar.foo:int(int)")
    }

    "find that foo is not defined and thus point to the superclass implementation" in {
      cpg.method.name("foo").caller.name.toSetMutable shouldBe Set("bar")
    }

    "account for call to inherited superclass" in {
      cpg.call.name("foo").callee.definingTypeDecl.fullName.toSetMutable shouldBe Set("Foo")
    }

  }

}
