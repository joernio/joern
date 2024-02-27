package io.joern.rubysrc2cpg.querying

import io.joern.rubysrc2cpg.passes.Defines.RubyOperators
import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Literal, Return}
import io.shiftleft.semanticcpg.language.*

class MethodReturnTests extends RubyCode2CpgFixture(withDataFlow = true) {

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
    c.methodFullName shouldBe "__builtin:puts"
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

  "implicit RETURN node for index access exists" in {
    val cpg = code("""
                     |def f
                     | l = [1, 2, 3]
                     | l[1]
                     |end
                     |""".stripMargin)

    val List(f)         = cpg.method.name("f").l
    val List(r: Return) = f.methodReturn.cfgIn.l: @unchecked

    r.code shouldBe "l[1]"
    r.lineNumber shouldBe Some(4)

    val List(c: Call) = r.astChildren.isCall.l
    c.methodFullName shouldBe Operators.indexAccess
    val List(arg1, arg2) = c.argument.l
    arg1.argumentIndex shouldBe 1
    arg2.argumentIndex shouldBe 2
  }

  "implicit RETURN node for `{x:0}` exists" in {
    val cpg = code("""
        |def f = {x:0}
        |""".stripMargin)

    val List(f)         = cpg.method.name("f").l
    val List(r: Return) = f.methodReturn.cfgIn.l: @unchecked

    r.code shouldBe "{x:0}"
    r.lineNumber shouldBe Some(2)

    val List(c: Call) = r.astChildren.isBlock.astChildren.isCall.l
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

    // Lowered as `def f; if true then return 20 else return nil end`

    inside(cpg.method.name("f").l) {
      case f :: Nil =>
        // Check the two return statements
        inside(f.methodReturn.toReturn.l) {
          case return20 :: returnNil :: Nil =>
            return20.code shouldBe "20"
            return20.lineNumber shouldBe Some(4)
            val List(twenty: Literal) = return20.astChildren.l: @unchecked
            twenty.code shouldBe "20"
            twenty.lineNumber shouldBe Some(4)
            twenty.typeFullName shouldBe "__builtin.Integer"

            returnNil.code shouldBe "return nil"
            returnNil.lineNumber shouldBe Some(3)
            val List(nil: Literal) = returnNil.astChildren.l: @unchecked
            nil.code shouldBe "nil"
            nil.lineNumber shouldBe Some(3)
            nil.typeFullName shouldBe "__builtin.NilClass"
          case xs => fail(s"Expected exactly two return nodes, instead got [${xs.code.mkString(",")}]")
        }
      case xs => fail(s"Expected exactly one method with the name `f`, instead got [${xs.code.mkString(",")}]")
    }
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

    inside(cpg.method.name("f").l) {
      case f :: Nil =>
        // Check the two return statements
        inside(f.methodReturn.toReturn.l) {
          case return20 :: return40 :: Nil =>
            return20.code shouldBe "20"
            return20.lineNumber shouldBe Some(4)
            val List(twenty: Literal) = return20.astChildren.l: @unchecked
            twenty.code shouldBe "20"
            twenty.lineNumber shouldBe Some(4)
            twenty.typeFullName shouldBe "__builtin.Integer"

            return40.code shouldBe "40"
            return40.lineNumber shouldBe Some(6)
            val List(forty: Literal) = return40.astChildren.l: @unchecked
            forty.code shouldBe "40"
            forty.lineNumber shouldBe Some(6)
            forty.typeFullName shouldBe "__builtin.Integer"
          case xs => fail(s"Expected exactly two return nodes, instead got [${xs.code.mkString(",")}]")
        }
      case xs => fail(s"Expected exactly one method with the name `f`, instead got [${xs.code.mkString(",")}]")
    }
  }

  "implicit RETURN node for ternary expression" in {
    val cpg = code("""
        |def f(x) = x ? 20 : 40
        |""".stripMargin)

    inside(cpg.method.name("f").l) {
      case f :: Nil =>
        // Check the two return statements
        inside(f.methodReturn.toReturn.l) {
          case return20 :: return40 :: Nil =>
            return20.code shouldBe "20"
            return20.lineNumber shouldBe Some(2)
            val List(twenty: Literal) = return20.astChildren.l: @unchecked
            twenty.code shouldBe "20"
            twenty.lineNumber shouldBe Some(2)
            twenty.typeFullName shouldBe "__builtin.Integer"

            return40.code shouldBe "40"
            return40.lineNumber shouldBe Some(2)
            val List(forty: Literal) = return40.astChildren.l: @unchecked
            forty.code shouldBe "40"
            forty.lineNumber shouldBe Some(2)
            forty.typeFullName shouldBe "__builtin.Integer"
          case xs => fail(s"Expected exactly two return nodes, instead got [${xs.code.mkString(",")}]")
        }
      case xs => fail(s"Expected exactly one method with the name `f`, instead got [${xs.code.mkString(",")}]")
    }
  }

}
