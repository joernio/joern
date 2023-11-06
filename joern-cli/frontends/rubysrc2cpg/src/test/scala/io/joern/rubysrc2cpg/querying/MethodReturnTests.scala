package io.joern.rubysrc2cpg.querying

import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.nodes.Return
import io.shiftleft.semanticcpg.language.*

class MethodReturnTests extends RubyCode2CpgFixture {

  "implicit RETURN node for `x * x` exists" in {
    val cpg = code("""
                     |def f(x) = x * x
                     |""".stripMargin)

    val List(f)         = cpg.method.name("f").l
    val List(r: Return) = f.methodReturn.cfgIn.l: @unchecked
    r.code shouldBe "x * x"
    r.lineNumber shouldBe Some(2)
  }

  "explicit RETURN node for `x * x` exists" in {
    val cpg = code("""
                     |def f(x)
                     | return x * x
                     |end
                     |""".stripMargin)

    val List(f)         = cpg.method.name("f").l
    val List(r: Return) = f.methodReturn.cfgIn.l: @unchecked
    r.code shouldBe "return x * x"
    r.lineNumber shouldBe Some(3)
  }

  "implicit RETURN node for `x = 1` exists" in {
    val cpg = code("""
                     |def f(x) = x = 1
                     |""".stripMargin)

    // Lowered as `def f(x) = x = 1; return x`
    val List(f)         = cpg.method.name("f").l
    val List(r: Return) = f.methodReturn.cfgIn.l: @unchecked
    r.code shouldBe "x = 1"
    r.lineNumber shouldBe Some(2)
  }

  "implicit RETURN node for `x` exists" in {
    val cpg = code("""
        |def f(x)
        | x
        |end
        |""".stripMargin)

    val List(f)         = cpg.method.name("f").l
    val List(r: Return) = f.methodReturn.cfgIn.l: @unchecked
    r.code shouldBe "x"
    r.lineNumber shouldBe Some(3)
  }

}
