package io.joern.rubysrc2cpg.querying

import io.joern.rubysrc2cpg.passes.Defines.RubyOperators
import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Literal, Return}
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

  "implicit RETURN node for `puts x` exists" in {
    val cpg = code("""
        |def f(x)
        | puts x
        |end
        |""".stripMargin)

    val List(f)         = cpg.method.name("f").l
    val List(r: Return) = f.methodReturn.cfgIn.l: @unchecked

    r.code shouldBe "puts x"
    r.lineNumber shouldBe Some(3)

    val List(c: Call) = r.astChildren.isCall.l
    c.methodFullName shouldBe "puts"
    c.lineNumber shouldBe Some(3)
    c.code shouldBe "puts x"
  }

  "implicit RETURN node for `x.class` exists" in {
    val cpg = code("""
        |def f(x)
        | x.class
        |end""".stripMargin)

    val List(f)         = cpg.method.name("f").l
    val List(r: Return) = f.methodReturn.cfgIn.l: @unchecked

    r.code shouldBe "x.class"
    r.lineNumber shouldBe Some(3)
  }

  "implicit RETURN node for `[]` exists" in {
    val cpg = code("""
        |def f
        | []
        |end
        |""".stripMargin)

    val List(f)         = cpg.method.name("f").l
    val List(r: Return) = f.methodReturn.cfgIn.l: @unchecked

    r.code shouldBe "[]"
    r.lineNumber shouldBe Some(3)

    val List(c: Call) = r.astChildren.isCall.l
    c.methodFullName shouldBe Operators.arrayInitializer
  }

  "implicit RETURN node for `{x:0}` exists" in {
    val cpg = code("""
        |def f = {x:0}
        |""".stripMargin)

    val List(f)         = cpg.method.name("f").l
    val List(r: Return) = f.methodReturn.cfgIn.l: @unchecked

    r.code shouldBe "{x:0}"
    r.lineNumber shouldBe Some(2)

    val List(c: Call) = r.astChildren.isCall.l
    c.methodFullName shouldBe RubyOperators.hashInitializer
  }

  "implicit RETURN node for `def f ... end` exists" in {
    val cpg = code("""
        |def f
        | def g = 0
        |end
        |""".stripMargin)

    // Lowered as `def f; def g = 0; return :g; end`
    val List(f)         = cpg.method.name("f").l
    val List(r: Return) = f.methodReturn.cfgIn.l: @unchecked

    val List(s: Literal) = r.astChildren.isLiteral.l
    s.code shouldBe ":g"
    s.typeFullName shouldBe "__builtin.Symbol"
  }

  "explicit RETURN node for `\"\"` exists" in {
    val cpg = code("""
        |def foo
        | return ""
        |end
        |""".stripMargin)

    val List(f)         = cpg.method.name("foo").l
    val List(r: Return) = f.methodReturn.cfgIn.l: @unchecked
    r.code shouldBe "return \"\""
    r.lineNumber shouldBe Some(3)
  }

  "implicit RETURN node for `if-end` expression" in {
    val cpg = code("""
        |def f
        | if true then
        |   20
        | end
        |end""".stripMargin)

    // Lowered as `def f; return true ? 20 : nil; end`
    val List(f)         = cpg.method.name("f").l
    val List(r: Return) = f.methodReturn.cfgIn.l: @unchecked
    val List(c: Call)   = r.astChildren.isCall.l

    c.methodFullName shouldBe Operators.conditional
    val test      = c.argument(1)
    val blockThen = c.argument(2)
    val blockElse = c.argument(3)

    test.code shouldBe "true"
    test.lineNumber shouldBe Some(3)
    blockThen.isBlock shouldBe true
    blockElse.isBlock shouldBe true

    val List(twenty: Literal) = blockThen.astChildren.l: @unchecked
    twenty.code shouldBe "20"
    twenty.lineNumber shouldBe Some(4)
    twenty.typeFullName shouldBe "__builtin.Integer"

    val List(nil: Literal) = blockElse.astChildren.l: @unchecked
    nil.code shouldBe "nil"
    nil.typeFullName shouldBe "__builtin.NilClass"
  }

  "implicit RETURN node for `if-else-end` expression" in {
    val cpg = code("""
        |def f
        | if true then
        |   20
        | else
        |   40
        | end
        |end
        |""".stripMargin)

    val List(f)         = cpg.method.name("f").l
    val List(r: Return) = f.methodReturn.cfgIn.l: @unchecked
    val List(c: Call)   = r.astChildren.isCall.l

    c.methodFullName shouldBe Operators.conditional
    val test      = c.argument(1)
    val blockThen = c.argument(2)
    val blockElse = c.argument(3)

    test.code shouldBe "true"
    test.lineNumber shouldBe Some(3)
    blockThen.isBlock shouldBe true
    blockElse.isBlock shouldBe true

    val List(twenty: Literal) = blockThen.astChildren.l: @unchecked
    twenty.code shouldBe "20"
    twenty.lineNumber shouldBe Some(4)
    twenty.typeFullName shouldBe "__builtin.Integer"

    val List(forty: Literal) = blockElse.astChildren.l: @unchecked
    forty.code shouldBe "40"
    forty.lineNumber shouldBe Some(6)
    forty.typeFullName shouldBe "__builtin.Integer"
  }

}
