package io.joern.javasrc2cpg.querying

import io.joern.javasrc2cpg.testfixtures.JavaSrcCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.codepropertygraph.generated.nodes.{Identifier, TypeRef}
import io.shiftleft.semanticcpg.language.*

class SpecialOperatorTests extends JavaSrcCode2CpgFixture {

  val cpg = code("""
      |public class Foo {
      |  public void foo(Object o) {
      |    if (o instanceof String) {
      |      System.out.println("o is a String");
      |    }
      |  }
      |
      |  public void bar(Object o) {
      |    String s = (String) o;
      |    System.out.println(s);
      |  }
      |}
      |""".stripMargin)

  "it should create a call to `<operator>.instanceOf` with the correct arguments" in {
    val call = cpg.call.nameExact("<operator>.instanceOf").head

    call.argument.size shouldBe 2
    call.order shouldBe 1
    call.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH

    val List(o: Identifier, t: TypeRef) = call.argument.l: @unchecked
    o.code shouldBe "o"
    o.order shouldBe 1
    o.argumentIndex shouldBe 1
    o.lineNumber shouldBe Some(4)
    o.columnNumber shouldBe Some(9)
    o.typeFullName shouldBe "java.lang.Object"

    t.code shouldBe "String"
    t.order shouldBe 2
    t.argumentIndex shouldBe 2
    t.lineNumber shouldBe Some(4)
    t.columnNumber shouldBe Some(22)
    t.typeFullName shouldBe "java.lang.String"
  }

  "it should create a call to `<operator>.cast` with the correct arguments" in {
    val call = cpg.call.nameExact("<operator>.cast").head

    call.argument.size shouldBe 2
    call.order shouldBe 2
    call.argumentIndex shouldBe 2
    call.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH

    val List(t: TypeRef, i: Identifier) = call.argument.l: @unchecked

    t.order shouldBe 1
    t.argumentIndex shouldBe 1
    t.code shouldBe "String"
    t.typeFullName shouldBe "java.lang.String"
    t.lineNumber shouldBe Some(10)
    t.columnNumber shouldBe Some(16)

    i.order shouldBe 2
    i.argumentIndex shouldBe 2
    i.code shouldBe "o"
    i.name shouldBe "o"
    i.typeFullName shouldBe "java.lang.Object"
    i.lineNumber shouldBe Some(10)
    i.columnNumber shouldBe Some(25)
  }
}
