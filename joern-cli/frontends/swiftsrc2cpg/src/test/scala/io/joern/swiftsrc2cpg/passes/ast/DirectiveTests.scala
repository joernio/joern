package io.joern.swiftsrc2cpg.passes.ast

import io.joern.swiftsrc2cpg.testfixtures.AstSwiftSrc2CpgSuite
import io.joern.swiftsrc2cpg.Config
import io.shiftleft.semanticcpg.language.*

class DirectiveTests extends AstSwiftSrc2CpgSuite {

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

    "testConfigExpression1 (no defines given)" in {
      val cpg = code(IfConfigExpressionCode)
      cpg.call.code.l shouldBe List("foo()")
    }

    "testConfigExpression2 (one define given)" in {
      val cpg = code(IfConfigExpressionCode).withConfig(Config(Set("CONFIG1")))
      cpg.call.code.l shouldBe List("foo()", "config1()")
    }

    "testConfigExpression3 (multiple defines given)" in {
      val cpg = code(IfConfigExpressionCode).withConfig(Config(Set("CONFIG1", "CONFIG2")))
      cpg.call.code.l shouldBe List("foo()", "config1()", "config2()", "elseConfig4()", "bar()")
    }

    "testConfigExpression4 (all non-conflicting defines given)" in {
      val cpg = code(IfConfigExpressionCode).withConfig(Config(Set("CONFIG1", "CONFIG2", "CONFIG3", "INNER1")))
      cpg.call.code.l shouldBe List("foo()", "config1()", "config2()", "inner1()", "bar()")
    }

    "testConfigExpression5 (all non-conflicting defines given for elseif)" in {
      val cpg = code(IfConfigExpressionCode).withConfig(Config(Set("CONFIG1", "CONFIG2", "CONFIG4")))
      cpg.call.code.l shouldBe List("foo()", "config1()", "config2()", "config4()", "bar()")
    }

    "testConfigExpression6 (call behind define)" in {
      val cpg = code("""
        |foo
        |#if CONFIG1
        |.bar()
        |#else
        |.baz()
        |#endif
        |""".stripMargin).withConfig(Config(Set("CONFIG1")))
      cpg.call.code.l shouldBe List("foo.bar()", "foo.bar")
    }

    "testConfigExpression7 (call behind define with trailing call)" in {
      val cpg = code("""
        |foo
        |#if CONFIG1
        |.bar()
        |#else
        |.baz()
        |#endif
        |.oneMore(x: 1)
        |""".stripMargin).withConfig(Config(Set("CONFIG1")))
      cpg.call.code.l shouldBe List(
        "(_tmp_0 = foo.bar()).oneMore(x: 1)",
        "(_tmp_0 = foo.bar()).oneMore",
        "(_tmp_0 = foo.bar())",
        "foo.bar()",
        "foo.bar",
        "x: 1"
      )
    }

    "testSourceLocation1" ignore {
      val cpg = code("#sourceLocation()")
      ???
    }

    "testSourceLocation2" ignore {
      val cpg = code("""#sourceLocation(file: "foo", line: 42)""")
      ???
    }

    "testHasAttribute" ignore {
      val cpg = code("""
      |@frozen
      |#if hasAttribute(foo)
      |@foo
      |#endif
      |public struct S2 { }
      |""".stripMargin)
      ???
    }
  }

}
