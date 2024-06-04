package io.joern.jimple2cpg.querying

import io.joern.jimple2cpg.testfixtures.JimpleCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language._

class MethodReturnTests extends JimpleCode2CpgFixture {

  val cpg: Cpg = code("""class Foo {
      |  int foo() { return 1; }
      |}
      |""".stripMargin).cpg

  "should have METHOD_RETURN node with correct fields" in {
    val List(x) = cpg.method.name("foo").methodReturn.typeFullName("int").l

    x.code shouldBe "RET"
    x.typeFullName shouldBe "int"
    x.lineNumber shouldBe Some(1)
  }

  "should have a RETURN node with correct fields" in {
    val List(x) = cpg.method.name("foo").ast.isReturn.l
    x.code shouldBe "return 1;"
    x.astChildren.size shouldBe 1
    x.argumentOut.size shouldBe 1
  }

  "should allow traversing to method" in {
    cpg.methodReturn.typeFullName("int").method.name.l shouldBe List("foo")
  }

}
