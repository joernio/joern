package io.joern.swiftsrc2cpg.passes.ast

import io.joern.swiftsrc2cpg.testfixtures.SwiftSrc2CpgSuite
import io.joern.swiftsrc2cpg.Config
import io.shiftleft.semanticcpg.language.*

class DirectiveTests extends SwiftSrc2CpgSuite {

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
      cpg.call.code.l shouldBe List("foo.bar()")
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
      cpg.call.code.l shouldBe List("foo.bar().oneMore(x: 1)", "foo.bar()")
    }

    "testSourceLocation1" in {
      val cpg = code("#sourceLocation()")
      // #sourceLocation is a parser-only directive — it must not produce CPG calls/locals.
      cpg.call.l shouldBe empty
      cpg.local.l shouldBe empty
      cpg.method.nameExact("<global>").size shouldBe 1
    }

    "testSourceLocation2" in {
      val cpg = code("""#sourceLocation(file: "foo", line: 42)""")
      // Same as above: parser-only directive, no CPG calls.
      cpg.call.l shouldBe empty
      cpg.local.l shouldBe empty
      cpg.method.nameExact("<global>").size shouldBe 1
    }

    "testHasAttribute" in {
      val cpg = code("""
      |@frozen
      |#if hasAttribute(foo)
      |@foo
      |#endif
      |public struct S2 { }
      |""".stripMargin)
      val List(s2) = cpg.typeDecl.nameExact("S2").l
      s2.fullName shouldBe "Test0.swift:<global>.S2"
      // Conditional `#if hasAttribute(foo) @foo #endif` block is skipped by default
      // and contributes no calls; struct still gets its synthesized init.
      cpg.call.l shouldBe empty
      cpg.method.nameExact("init").fullName.l should contain("Test0.swift:<global>.S2.init:()->Test0.swift:<global>.S2")
    }
  }

}
