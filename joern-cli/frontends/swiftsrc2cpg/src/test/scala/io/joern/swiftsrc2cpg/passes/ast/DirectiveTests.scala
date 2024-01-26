package io.joern.swiftsrc2cpg.passes.ast

import io.shiftleft.semanticcpg.language._

class DirectiveTests extends AbstractPassTest {

  private val IfConfigExpressionCode = """
   |foo()
   |#if CONFIG1
   |config1()
   |#if CONFIG2
   |config2()
   |#if CONFIG3
   |  #if INNER1
   |  inner1()
   |  #endif
   |#elseif CONFIG4
   |  config4()
   |#else
   |  elseConfig4()
   |#endif
   |bar()
   |#endif
   |#endif""".stripMargin

  "DirectiveTests" should {

    "testConfigExpression1 (no defines given)" in AstFixture(IfConfigExpressionCode) { cpg =>
      cpg.call.code.l shouldBe List("foo()")
    }

    "testConfigExpression2 (one define given)" in AstFixture(IfConfigExpressionCode, defines = Set("CONFIG1")) { cpg =>
      cpg.call.code.l shouldBe List("foo()", "config1()")
    }

    "testConfigExpression3 (multiple defines given)" in AstFixture(
      IfConfigExpressionCode,
      defines = Set("CONFIG1", "CONFIG2")
    ) { cpg =>
      cpg.call.code.l shouldBe List("foo()", "config1()", "config2()", "elseConfig4()", "bar()")
    }

    "testConfigExpression4 (all non-conflicting defines given)" in AstFixture(
      IfConfigExpressionCode,
      defines = Set("CONFIG1", "CONFIG2", "CONFIG3", "INNER1")
    ) { cpg =>
      cpg.call.code.l shouldBe List("foo()", "config1()", "config2()", "inner1()", "bar()")
    }

    "testConfigExpression5 (all non-conflicting defines given for elseif)" in AstFixture(
      IfConfigExpressionCode,
      defines = Set("CONFIG1", "CONFIG2", "CONFIG4")
    ) { cpg =>
      cpg.call.code.l shouldBe List("foo()", "config1()", "config2()", "config4()", "bar()")
    }

    "testConfigExpression6 (call behind define)" in AstFixture(
      """
        |foo
        |#if CONFIG1
        |.bar()
        |#else
        |.baz()
        |#endif
        |""".stripMargin,
      defines = Set("CONFIG1")
    ) { cpg =>
      cpg.call.code.l shouldBe List("foo.bar()", "foo.bar")
    }

    "testConfigExpression7 (call behind define with trailing call)" in AstFixture(
      """
        |foo
        |#if CONFIG1
        |.bar()
        |#else
        |.baz()
        |#endif
        |.oneMore(x: 1)
        |""".stripMargin,
      defines = Set("CONFIG1")
    ) { cpg =>
      cpg.call.code.l shouldBe List(
        "(_tmp_0 = foo.bar()).oneMore(x: 1)",
        "(_tmp_0 = foo.bar()).oneMore",
        "(_tmp_0 = foo.bar())",
        "foo.bar()",
        "foo.bar",
        "x: 1"
      )
    }

    "testSourceLocation1" ignore AstFixture("#sourceLocation()") { cpg => ??? }

    "testSourceLocation2" ignore AstFixture("""#sourceLocation(file: "foo", line: 42)""") { cpg => ??? }

    "testHasAttribute" ignore AstFixture("""
      |@frozen
      |#if hasAttribute(foo)
      |@foo
      |#endif
      |public struct S2 { }
      |""".stripMargin) { cpg => ??? }
  }

}
