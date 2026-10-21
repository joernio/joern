package io.joern.jimple2cpg.querying

import io.joern.jimple2cpg.testfixtures.JimpleCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Local
import io.shiftleft.semanticcpg.language.*
import org.scalatest.Ignore

class LocalTests extends JimpleCode2CpgFixture {

  val cpg: Cpg = code("""
      | @SuppressWarnings("deprecation")
      | class Foo {
      |   Integer foo() {
      |     int x;
      |     Integer y = null;
      |     x = 1;
      |     y = new Integer(x);
      |     return x + y;
      |   }
      | }
      |""".stripMargin).cpg

  "should contain locals `x` and `y` with correct fields set" in {
    val List(x: Local) = cpg.local("\\$stack4").l
    val List(y: Local) = cpg.local("y").l
    x.name shouldBe "$stack4"
    x.code shouldBe "int $stack4"
    x.typeFullName shouldBe "int"
    x.order shouldBe 2

    y.name shouldBe "y"
    y.code shouldBe "java.lang.Integer y"
    y.typeFullName shouldBe "java.lang.Integer"
    y.order shouldBe 1
  }

  "should allow traversing from local to identifier" in {
    val ys = cpg.local.nameExact("y").referencingIdentifiers.l
    ys.size shouldBe 5
    ys.head.name shouldBe "y"
    val xs = cpg.local.nameExact("$stack4").referencingIdentifiers.l
    xs.size shouldBe 4
    xs.head.name shouldBe "$stack4"
  }

}
