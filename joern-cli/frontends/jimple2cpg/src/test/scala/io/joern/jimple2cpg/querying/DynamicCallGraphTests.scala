package io.joern.jimple2cpg.querying

import io.joern.jimple2cpg.testfixtures.JimpleCodeToCpgFixture
import io.shiftleft.semanticcpg.language.{NoResolve, _}

class DynamicCallGraphTests extends JimpleCodeToCpgFixture {

  implicit val resolver: NoResolve.type = NoResolve

  override val code =
    """
class Foo {

	public static void main(String[] args){
		A b1 = new B();
		A c1 = new C();

		A b2 = b1;
		A c2 = c1;

		// what will get printed?
		b2.print(c2);
	}

	public static class A extends Object {
		public void print(A object) {
			System.out.println("An instance of " + object.getClass().getSimpleName()
					+ " was passed to A's print(A object)");
		}
	}

	public static class B extends A {
		public void print(A object) {
			System.out.println("An instance of " + object.getClass().getSimpleName()
					+ " was passed to B's print(A object)");
		}
	}

	public static class C extends B {
		public void print(A object) {
			System.out.println("An instance of " + object.getClass().getSimpleName()
					+ " was passed to C's print(A object)");
		}
	}

	public static class D extends A {
		public void print(A object) {
			System.out.println("An instance of " + object.getClass().getSimpleName()
					+ " was passed to D's print(A object)");
		}
	}

}
    """

  "should find that add is called by main" in {
    cpg.method.name("print").caller.name.toSetMutable shouldBe Set("main")
  }

  "should account for print calls from all subclasses due to using CHA" in {
    cpg.call.name("print").callee.definingTypeDecl.fullName.toSetMutable shouldBe Set(
      "Foo$D",
      "Foo$B",
      "Foo$C",
      "Foo$A"
    )
  }

}
