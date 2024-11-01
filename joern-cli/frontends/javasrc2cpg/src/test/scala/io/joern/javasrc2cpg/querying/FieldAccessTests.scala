package io.joern.javasrc2cpg.querying

import io.joern.javasrc2cpg.testfixtures.JavaSrcCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.nodes.{Call, FieldIdentifier, Identifier, TypeRef}
import io.shiftleft.semanticcpg.language.*

class FieldAccessTests extends JavaSrcCode2CpgFixture {

  val cpg = code("""
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
      |""".stripMargin)

  "should handle static member accesses" in {
    val List(access: Call)                                             = cpg.method(".*foo.*").call(".*fieldAccess").l
    val List(identifier: Identifier, fieldIdentifier: FieldIdentifier) = access.argument.l: @unchecked
    identifier.name shouldBe "Foo"
    identifier.typeFullName shouldBe "Foo"
    fieldIdentifier.canonicalName shouldBe "MAX_VALUE"
  }

  "should handle object field accesses on RHS of assignments" in {
    val List(access: Call)                                             = cpg.method(".*bar.*").call(".*fieldAccess").l
    val List(identifier: Identifier, fieldIdentifier: FieldIdentifier) = access.argument.l: @unchecked
    identifier.name shouldBe "f"
    identifier.typeFullName shouldBe "Foo"
    fieldIdentifier.canonicalName shouldBe "value"
  }

  "should handle object field accesses on LHS of assignments" in {
    val List(access: Call)                                             = cpg.method(".*baz.*").call(".*fieldAccess").l
    val List(identifier: Identifier, fieldIdentifier: FieldIdentifier) = access.argument.l: @unchecked
    identifier.name shouldBe "g"
    identifier.typeFullName shouldBe "Foo"
    fieldIdentifier.canonicalName shouldBe "value"
  }

  "should link to the referencing static member" in {
    val List(access: Call) = cpg.method(".*foo.*").call(".*fieldAccess").l
    access.referencedMember.name.head shouldBe "MAX_VALUE"
  }

  "should link to the referencing dynamic member on the RHS of assignments" in {
    val List(access: Call) = cpg.method(".*bar.*").call(".*fieldAccess").l
    access.referencedMember.name.head shouldBe "value"
  }

  "should link to the referencing dynamic member on the LHS of assignments" in {
    val List(access: Call) = cpg.method(".*baz.*").call(".*fieldAccess").l
    access.referencedMember.name.head shouldBe "value"
  }

  "correctly handle access to statically imported field" in {
    val cpg = code("""
        |import static Bar.STATIC_INT;
        |public class Foo {
        |  public void foo() {
        |    int x = STATIC_INT;
        |  }
        |}
        |""".stripMargin)
      .moreCode(
        """
          |public class Bar {
          |  public static int STATIC_INT = 111;
          |}
          |""".stripMargin,
        fileName = "Bar.java"
      )

    val List(assignment) = cpg.call.code("int x = STATIC_INT").l
    val fieldAccess      = assignment.argument(2).asInstanceOf[Call]
    val typeRef          = fieldAccess.argument(1).asInstanceOf[TypeRef]
    typeRef.typeFullName shouldBe "Bar"
  }

}
