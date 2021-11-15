package io.joern.javasrc2cpg.querying

import io.joern.javasrc2cpg.testfixtures.JavaSrcCodeToCpgFixture
import io.shiftleft.codepropertygraph.generated.nodes.{Identifier, TypeRef}
import io.shiftleft.proto.cpg.Cpg.DispatchTypes
import io.shiftleft.semanticcpg.language._

class InstanceOfTests extends JavaSrcCodeToCpgFixture {

  override val code: String =
    """
      |public class Foo {
      |  public void foo(Object o) {
      |    if (o instanceof String) {
      |      System.out.println("o is a String");
      |    }
      |  }
      |}
      |""".stripMargin

  "it should create a call to `<operator>.instanceOf` with the correct arguments" in {
    val call = cpg.call.nameExact("<operator>.instanceOf").head

    call.argument.size shouldBe 2
    call.order shouldBe 1
    call.argumentIndex shouldBe 1
    call.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH.toString


    val List(o: Identifier, t: TypeRef) = call.argument.l
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
}
