package io.joern.javasrc2cpg.querying

import io.joern.javasrc2cpg.testfixtures.JavaSrcCodeToCpgFixture
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Identifier, Literal, Method}
import io.shiftleft.semanticcpg.language._

class ConstructorInvocationTests extends JavaSrcCodeToCpgFixture {

  override val code: String =
    """
      |class Foo {
      |  int x;
      |  public Foo(int x) {
      |    this.x = x;
      |  }
      |
      |  public int getValue() {
      |    return x;
      |  }
      |}
      |
      |class Bar extends Foo {
      |  public Bar(int x) {
      |    super(x);
      |  }
      |
      |  public Bar(int x, int y) {
      |    this(x + y);
      |  }
      |}
      |""".stripMargin

  "it should create correct method nodes for constructors" in {
    cpg.method.name("Foo").l match {
      case List(cons: Method) =>
        cons.fullName shouldBe "Foo.Foo:Foo(int)"
        cons.signature shouldBe "Foo(int)"
        cons.code shouldBe "public Foo(int x)"

      case res =>
        fail(s"Expected single Foo constructor, but got $res")
    }

    cpg.method.name("Bar").l match {
      case List(cons1: Method, cons2: Method) =>
        cons1.fullName shouldBe "Bar.Bar:Bar(int)"
        cons1.signature shouldBe "Bar(int)"
        cons1.code shouldBe "public Bar(int x)"

        cons2.fullName shouldBe "Bar.Bar:Bar(int,int)"
        cons2.signature shouldBe "Bar(int,int)"
        cons2.code shouldBe "public Bar(int x, int y)"

      case res =>
        fail(s"Expected 2 Bar constructors, but got $res")
    }
  }

  "it should create a constructor call node for the `this` statement" in {
    cpg.call(".*Bar.*").l match {
      case List(call: Call) =>
        call.methodFullName shouldBe "Bar.Bar"
        call.name shouldBe "Bar.Bar"
        call.code shouldBe "this(x + y);"
        call.argument.head shouldBe a[Call]
        call.argument.head.asInstanceOf[Call].name shouldBe Operators.addition

      case res =>
        fail(s"Expected single call to Bar.Bar but got $res")
    }
  }

  "it should create a constructor call node for the `super` statement" in {
    cpg.call(".*Foo.*").l match {
      case List(call: Call) =>
        call.methodFullName shouldBe "Foo.Foo"
        call.name shouldBe "Foo.Foo"
        call.code shouldBe "super(x);"
        call.argument.head shouldBe a[Identifier]
        call.argument.head.code shouldBe "x"

      case res =>
        fail(s"Expected single call to Foo.Foo but got $res")
    }
  }
}
