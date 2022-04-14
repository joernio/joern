package io.joern.javasrc2cpg.querying

import io.joern.javasrc2cpg.testfixtures.JavaSrcCodeToCpgFixture
import io.shiftleft.codepropertygraph.generated.nodes.{Call, FieldIdentifier, Identifier}
import io.shiftleft.semanticcpg.language._

class FieldAccessTests extends JavaSrcCodeToCpgFixture {

  implicit val resolver: ICallResolver = NoResolve

  override val code: String =
    """
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
      |""".stripMargin

  "should handle static member accesses" in {
    val List(access: Call)                                             = cpg.method(".*foo.*").call(".*fieldAccess").l
    val List(identifier: Identifier, fieldIdentifier: FieldIdentifier) = access.argument.l
    identifier.name shouldBe "Foo"
    identifier.typeFullName shouldBe "Foo"
    fieldIdentifier.canonicalName shouldBe "MAX_VALUE"
  }

  "should handle object field accesses on RHS of assignments" in {
    val List(access: Call)                                             = cpg.method(".*bar.*").call(".*fieldAccess").l
    val List(identifier: Identifier, fieldIdentifier: FieldIdentifier) = access.argument.l
    identifier.name shouldBe "f"
    identifier.typeFullName shouldBe "Foo"
    fieldIdentifier.canonicalName shouldBe "value"
  }

  "should handle object field accesses on LHS of assignments" in {
    val List(access: Call)                                             = cpg.method(".*baz.*").call(".*fieldAccess").l
    val List(identifier: Identifier, fieldIdentifier: FieldIdentifier) = access.argument.l
    identifier.name shouldBe "g"
    identifier.typeFullName shouldBe "Foo"
    fieldIdentifier.canonicalName shouldBe "value"
  }
}
