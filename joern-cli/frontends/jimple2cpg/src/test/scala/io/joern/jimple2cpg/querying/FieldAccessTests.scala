package io.joern.jimple2cpg.querying

import io.joern.jimple2cpg.testfixtures.JimpleCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{Call, FieldIdentifier, Identifier}
import io.shiftleft.semanticcpg.language._

class FieldAccessTests extends JimpleCode2CpgFixture {

  implicit val resolver: ICallResolver = NoResolve

  val cpg: Cpg = code("""
      |class Foo {
      |  public static int MAX_VALUE = 12;
      |  public int value;
      |
      |  public Foo(int value) {
      |    if (value <= MAX_VALUE) {
      |      this.value = value;
      |    } else {
      |      this.value = MAX_VALUE;
      |    }
      |  }
      |
      |  public void setValue(int value) {
      |    if (value <= MAX_VALUE) {
      |      this.value = value;
      |    }
      |  }
      |}
      |
      |class Test {
      |public void foo() {
      |  int x = Foo.MAX_VALUE;
      |}
      |
      |public void bar() {
      |  Foo f = new Foo(5);
      |  int y = f.value;
      |}
      |
      |public void baz() {
      |  Foo g = new Foo(5);
      |  g.value = 66;
      |}
      |}
      |""".stripMargin).cpg

  "should handle static member accesses" in {
    val List(assign: Call) = cpg.method(".*foo.*").call(".*assignment").l
    assign.code shouldBe "x = Foo.MAX_VALUE"
    val List(access: Call) = cpg.method(".*foo.*").call(".*fieldAccess").l
    access.code shouldBe "Foo.MAX_VALUE"
    val List(identifier: Identifier, fieldIdentifier: FieldIdentifier) = access.argument.l: @unchecked
    identifier.code shouldBe "Foo"
    identifier.name shouldBe "Foo"
    identifier.typeFullName shouldBe "Foo"
    fieldIdentifier.canonicalName shouldBe "MAX_VALUE"
    fieldIdentifier.code shouldBe "MAX_VALUE"
  }

  "should handle object field accesses on RHS of assignments" in {
    val List(_: Call, _: Call, assign: Call) = cpg.method(".*bar.*").call(".*assignment").l
    assign.code shouldBe "y = f.value"
    val List(access: Call)                                             = cpg.method(".*bar.*").call(".*fieldAccess").l
    val List(identifier: Identifier, fieldIdentifier: FieldIdentifier) = access.argument.l: @unchecked
    identifier.name shouldBe "f"
    identifier.typeFullName shouldBe "Foo"
    fieldIdentifier.canonicalName shouldBe "value"
  }

  "should handle object field accesses on LHS of assignments" in {
    val List(_: Call, _: Call, assign: Call) = cpg.method(".*baz.*").call(".*assignment").l
    assign.code shouldBe "g.value = 66"
    val List(access: Call)                                             = cpg.method(".*baz.*").call(".*fieldAccess").l
    val List(identifier: Identifier, fieldIdentifier: FieldIdentifier) = access.argument.l: @unchecked
    identifier.name shouldBe "g"
    identifier.typeFullName shouldBe "Foo"
    fieldIdentifier.canonicalName shouldBe "value"
  }
}
