package io.joern.c2cpg.passes

import io.joern.c2cpg.fixtures.CpgCfgFixture
import io.joern.x2cpg.passes.controlflow.cfgcreation.Cfg._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class CfgCreationPassTests extends AnyWordSpec with Matchers {

  "Cfg" should {

    "contain an entry and exit node at least" in new CpgCfgFixture("") {
      succOf("RET func ()") shouldBe expected(("RET", AlwaysEdge))
      succOf("RET") shouldBe expected()
    }

    "be correct for decl statement with assignment" in new CpgCfgFixture("int x = 1;") {
      succOf("RET func ()") shouldBe expected(("x", AlwaysEdge))
      succOf("x") shouldBe expected(("1", AlwaysEdge))
      succOf("1") shouldBe expected(("x = 1", AlwaysEdge))
      succOf("x = 1") shouldBe expected(("RET", AlwaysEdge))
    }

    "be correct for nested expression" in new CpgCfgFixture("x = y + 1;") {
      succOf("RET func ()") shouldBe expected(("x", AlwaysEdge))
      succOf("x") shouldBe expected(("y", AlwaysEdge))
      succOf("y") shouldBe expected(("1", AlwaysEdge))
      succOf("1") shouldBe expected(("y + 1", AlwaysEdge))
      succOf("y + 1") shouldBe expected(("x = y + 1", AlwaysEdge))
      succOf("x = y + 1") shouldBe expected(("RET", AlwaysEdge))
    }

    "be correct for return statement" in new CpgCfgFixture("return x;") {
      succOf("RET func ()") shouldBe expected(("x", AlwaysEdge))
      succOf("x") shouldBe expected(("return x;", AlwaysEdge))
      succOf("return x;") shouldBe expected(("RET", AlwaysEdge))
    }

    "be correct for consecutive return statements" in new CpgCfgFixture("return x; return y;") {
      succOf("RET func ()") shouldBe expected(("x", AlwaysEdge))
      succOf("x") shouldBe expected(("return x;", AlwaysEdge))
      succOf("y") shouldBe expected(("return y;", AlwaysEdge))
      succOf("return x;") shouldBe expected(("RET", AlwaysEdge))
      succOf("return y;") shouldBe expected(("RET", AlwaysEdge))
    }

    "be correct for void return statement" in new CpgCfgFixture("return;") {
      succOf("RET func ()") shouldBe expected(("return;", AlwaysEdge))
      succOf("return;") shouldBe expected(("RET", AlwaysEdge))
    }

    "be correct for call expression" in new CpgCfgFixture("foo(a + 1, b);") {
      succOf("RET func ()") shouldBe expected(("a", AlwaysEdge))
      succOf("a") shouldBe expected(("1", AlwaysEdge))
      succOf("1") shouldBe expected(("a + 1", AlwaysEdge))
      succOf("a + 1") shouldBe expected(("b", AlwaysEdge))
      succOf("b") shouldBe expected(("foo(a + 1, b)", AlwaysEdge))
      succOf("foo(a + 1, b)") shouldBe expected(("RET", AlwaysEdge))
    }

    "be correct for unary expression '+'" in new CpgCfgFixture("+x;") {
      succOf("RET func ()") shouldBe expected(("x", AlwaysEdge))
      succOf("x") shouldBe expected(("+x", AlwaysEdge))
      succOf("+x") shouldBe expected(("RET", AlwaysEdge))
    }

    "be correct for unary expression '++'" in new CpgCfgFixture("++x;") {
      succOf("RET func ()") shouldBe expected(("x", AlwaysEdge))
      succOf("x") shouldBe expected(("++x", AlwaysEdge))
      succOf("++x") shouldBe expected(("RET", AlwaysEdge))
    }

    "be correct for conditional expression" in new CpgCfgFixture("x ? y : z;") {
      succOf("RET func ()") shouldBe expected(("x", AlwaysEdge))
      succOf("x") shouldBe expected(("y", TrueEdge), ("z", FalseEdge))
      succOf("y") shouldBe expected(("x ? y : z", AlwaysEdge))
      succOf("z") shouldBe expected(("x ? y : z", AlwaysEdge))
      succOf("x ? y : z") shouldBe expected(("RET", AlwaysEdge))
    }

    "be correct for conditional expression with empty then" in new CpgCfgFixture("x ? : z;") {
      succOf("RET func ()") shouldBe expected(("x", AlwaysEdge))
      succOf("x") shouldBe expected(("x ? : z", TrueEdge), ("z", FalseEdge))
      succOf("z") shouldBe expected(("x ? : z", AlwaysEdge))
      succOf("x ? : z") shouldBe expected(("RET", AlwaysEdge))
    }

    "be correct for short-circuit AND expression" in new CpgCfgFixture("int z = x && y;") {
      succOf("RET func ()") shouldBe expected(("z", AlwaysEdge))
      succOf("z") shouldBe expected(("x", AlwaysEdge))
      succOf("x") shouldBe expected(("y", TrueEdge), ("x && y", FalseEdge))
      succOf("y") shouldBe expected(("x && y", AlwaysEdge))
      succOf("x && y") shouldBe expected(("z = x && y", AlwaysEdge))
      succOf("z = x && y") shouldBe expected(("RET", AlwaysEdge))
    }

    "be correct for short-circuit OR expression" in new CpgCfgFixture("x || y;") {
      succOf("RET func ()") shouldBe expected(("x", AlwaysEdge))
      succOf("x") shouldBe expected(("y", FalseEdge), ("x || y", TrueEdge))
      succOf("y") shouldBe expected(("x || y", AlwaysEdge))
      succOf("x || y") shouldBe expected(("RET", AlwaysEdge))
    }
  }

  "Cfg for while-loop" should {
    "be correct" in new CpgCfgFixture("while (x < 1) { y = 2; }") {
      succOf("RET func ()") shouldBe expected(("x", AlwaysEdge))
      succOf("x") shouldBe expected(("1", AlwaysEdge))
      succOf("1") shouldBe expected(("x < 1", AlwaysEdge))
      succOf("x < 1") shouldBe expected(("y", TrueEdge), ("RET", FalseEdge))
      succOf("y") shouldBe expected(("2", AlwaysEdge))
      succOf("2") shouldBe expected(("y = 2", AlwaysEdge))
      succOf("y = 2") shouldBe expected(("x", AlwaysEdge))
    }

    "be correct with break" in new CpgCfgFixture("while (x < 1) { break; y; }") {
      succOf("RET func ()") shouldBe expected(("x", AlwaysEdge))
      succOf("x") shouldBe expected(("1", AlwaysEdge))
      succOf("1") shouldBe expected(("x < 1", AlwaysEdge))
      succOf("x < 1") shouldBe expected(("break;", TrueEdge), ("RET", FalseEdge))
      succOf("break;") shouldBe expected(("RET", AlwaysEdge))
      succOf("y") shouldBe expected(("x", AlwaysEdge))
    }

    "be correct with continue" in new CpgCfgFixture("while (x < 1) { continue; y; }") {
      succOf("RET func ()") shouldBe expected(("x", AlwaysEdge))
      succOf("x") shouldBe expected(("1", AlwaysEdge))
      succOf("1") shouldBe expected(("x < 1", AlwaysEdge))
      succOf("x < 1") shouldBe expected(("continue;", TrueEdge), ("RET", FalseEdge))
      succOf("continue;") shouldBe expected(("x", AlwaysEdge))
      succOf("y") shouldBe expected(("x", AlwaysEdge))
    }

    "be correct with nested while-loop" in new CpgCfgFixture("while (x) { while (y) { z; }}") {
      succOf("RET func ()") shouldBe expected(("x", AlwaysEdge))
      succOf("x") shouldBe expected(("y", TrueEdge), ("RET", FalseEdge))
      succOf("y") shouldBe expected(("z", TrueEdge), ("x", FalseEdge))
      succOf("z") shouldBe expected(("y", AlwaysEdge))
    }
  }

  "Cfg for do-while-loop" should {
    "be correct" in new CpgCfgFixture("do { y = 2; } while (x < 1);") {
      succOf("RET func ()") shouldBe expected(("y", AlwaysEdge))
      succOf("y") shouldBe expected(("2", AlwaysEdge))
      succOf("2") shouldBe expected(("y = 2", AlwaysEdge))
      succOf("y = 2") shouldBe expected(("x", AlwaysEdge))
      succOf("x") shouldBe expected(("1", AlwaysEdge))
      succOf("1") shouldBe expected(("x < 1", AlwaysEdge))
      succOf("x < 1") shouldBe expected(("y", TrueEdge), ("RET", FalseEdge))
    }

    "be correct with break" in new CpgCfgFixture("do { break; y; } while (x < 1);") {
      succOf("RET func ()") shouldBe expected(("break;", AlwaysEdge))
      succOf("break;") shouldBe expected(("RET", AlwaysEdge))
      succOf("y") shouldBe expected(("x", AlwaysEdge))
      succOf("x") shouldBe expected(("1", AlwaysEdge))
      succOf("1") shouldBe expected(("x < 1", AlwaysEdge))
      succOf("x < 1") shouldBe expected(("break;", TrueEdge), ("RET", FalseEdge))
    }

    "be correct with continue" in new CpgCfgFixture("do { continue; y; } while (x < 1);") {
      succOf("RET func ()") shouldBe expected(("continue;", AlwaysEdge))
      succOf("continue;") shouldBe expected(("x", AlwaysEdge))
      succOf("y") shouldBe expected(("x", AlwaysEdge))
      succOf("x") shouldBe expected(("1", AlwaysEdge))
      succOf("1") shouldBe expected(("x < 1", AlwaysEdge))
      succOf("x < 1") shouldBe expected(("continue;", TrueEdge), ("RET", FalseEdge))
    }

    "be correct with nested do-while-loop" in new CpgCfgFixture("do { do { x; } while (y); } while (z);") {
      succOf("RET func ()") shouldBe expected(("x", AlwaysEdge))
      succOf("x") shouldBe expected(("y", AlwaysEdge))
      succOf("y") shouldBe expected(("x", TrueEdge), ("z", FalseEdge))
      succOf("z") shouldBe expected(("x", TrueEdge), ("RET", FalseEdge))
    }

    "be correct for do-while-loop with empty body" in new CpgCfgFixture("do { } while(x > 1);") {
      succOf("RET func ()") shouldBe expected(("x", AlwaysEdge))
      succOf("1") shouldBe expected(("x > 1", AlwaysEdge))
      succOf("x > 1") shouldBe expected(("x", TrueEdge), ("RET", FalseEdge))
    }

  }

  "Cfg for for-loop" should {
    "be correct" in new CpgCfgFixture("for (x = 0; y < 1; z += 2) { a = 3; }") {
      succOf("RET func ()") shouldBe expected(("x", AlwaysEdge))
      succOf("x") shouldBe expected(("0", AlwaysEdge))
      succOf("0") shouldBe expected(("x = 0", AlwaysEdge))
      succOf("x = 0") shouldBe expected(("y", AlwaysEdge))
      succOf("y") shouldBe expected(("1", AlwaysEdge))
      succOf("1") shouldBe expected(("y < 1", AlwaysEdge))
      succOf("y < 1") shouldBe expected(("a", TrueEdge), ("RET", FalseEdge))
      succOf("a") shouldBe expected(("3", AlwaysEdge))
      succOf("3") shouldBe expected(("a = 3", AlwaysEdge))
      succOf("a = 3") shouldBe expected(("z", AlwaysEdge))
      succOf("z") shouldBe expected(("2", AlwaysEdge))
      succOf("2") shouldBe expected(("z += 2", AlwaysEdge))
      succOf("z += 2") shouldBe expected(("y", AlwaysEdge))
    }

    "be correct with break" in new CpgCfgFixture("for (x = 0; y < 1; z += 2) { break; a = 3; }") {
      succOf("RET func ()") shouldBe expected(("x", AlwaysEdge))
      succOf("x") shouldBe expected(("0", AlwaysEdge))
      succOf("x = 0") shouldBe expected(("y", AlwaysEdge))
      succOf("y") shouldBe expected(("1", AlwaysEdge))
      succOf("1") shouldBe expected(("y < 1", AlwaysEdge))
      succOf("y < 1") shouldBe expected(("break;", TrueEdge), ("RET", FalseEdge))
      succOf("break;") shouldBe expected(("RET", AlwaysEdge))
      succOf("a") shouldBe expected(("3", AlwaysEdge))
      succOf("3") shouldBe expected(("a = 3", AlwaysEdge))
      succOf("a = 3") shouldBe expected(("z", AlwaysEdge))
      succOf("z") shouldBe expected(("2", AlwaysEdge))
      succOf("2") shouldBe expected(("z += 2", AlwaysEdge))
      succOf("z += 2") shouldBe expected(("y", AlwaysEdge))
    }

    "be correct with continue" in new CpgCfgFixture("for (x = 0; y < 1; z += 2) { continue; a = 3; }") {
      succOf("RET func ()") shouldBe expected(("x", AlwaysEdge))
      succOf("x") shouldBe expected(("0", AlwaysEdge))
      succOf("0") shouldBe expected(("x = 0", AlwaysEdge))
      succOf("x = 0") shouldBe expected(("y", AlwaysEdge))
      succOf("y") shouldBe expected(("1", AlwaysEdge))
      succOf("1") shouldBe expected(("y < 1", AlwaysEdge))
      succOf("y < 1") shouldBe expected(("continue;", TrueEdge), ("RET", FalseEdge))
      succOf("continue;") shouldBe expected(("z", AlwaysEdge))
      succOf("a") shouldBe expected(("3", AlwaysEdge))
      succOf("3") shouldBe expected(("a = 3", AlwaysEdge))
      succOf("a = 3") shouldBe expected(("z", AlwaysEdge))
      succOf("z") shouldBe expected(("2", AlwaysEdge))
      succOf("2") shouldBe expected(("z += 2", AlwaysEdge))
      succOf("z += 2") shouldBe expected(("y", AlwaysEdge))
    }

    "be correct with nested for-loop" in new CpgCfgFixture("for (x; y; z) { for (a; b; c) { u; } }") {
      succOf("RET func ()") shouldBe expected(("x", AlwaysEdge))
      succOf("x") shouldBe expected(("y", AlwaysEdge))
      succOf("y") shouldBe expected(("a", TrueEdge), ("RET", FalseEdge))
      succOf("z") shouldBe expected(("y", AlwaysEdge))
      succOf("a") shouldBe expected(("b", AlwaysEdge))
      succOf("b") shouldBe expected(("u", TrueEdge), ("z", FalseEdge))
      succOf("c") shouldBe expected(("b", AlwaysEdge))
      succOf("u") shouldBe expected(("c", AlwaysEdge))
    }

    "be correct with empty condition" in new CpgCfgFixture("for (;;) { a = 1; }") {
      succOf("RET func ()") shouldBe expected(("a", AlwaysEdge))
      succOf("a") shouldBe expected(("1", AlwaysEdge))
      succOf("1") shouldBe expected(("a = 1", AlwaysEdge))
      succOf("a = 1") shouldBe expected(("a", AlwaysEdge))
    }

    "be correct with empty condition with break" in new CpgCfgFixture("for (;;) { break; }") {
      succOf("RET func ()") shouldBe expected(("break;", AlwaysEdge))
      succOf("break;") shouldBe expected(("RET", AlwaysEdge))
    }

    "be correct with empty condition with continue" in new CpgCfgFixture("for (;;) { continue ; }") {
      succOf("RET func ()") shouldBe expected(("continue ;", AlwaysEdge))
      succOf("continue ;") shouldBe expected(("continue ;", AlwaysEdge))
    }

    "be correct with empty condition with nested empty for-loop" in new CpgCfgFixture("for (;;) { for (;;) { x; } }") {
      succOf("RET func ()") shouldBe expected(("x", AlwaysEdge))
      succOf("x") shouldBe expected(("x", AlwaysEdge))
    }

    "be correct with empty condition with empty block" in new CpgCfgFixture("for (;;) ;") {
      succOf("RET func ()") shouldBe expected(("RET", AlwaysEdge))
    }

    "be correct when empty for-loop is skipped" in new CpgCfgFixture("for (;;) {}; return;") {
      succOf("RET func ()") shouldBe expected(("return;", AlwaysEdge))
      succOf("return;") shouldBe expected(("RET", AlwaysEdge))
    }

    "be correct with function call condition with empty block" in new CpgCfgFixture("for (; x(1);) ;") {
      succOf("RET func ()") shouldBe expected(("1", AlwaysEdge))
      succOf("1") shouldBe expected(("x(1)", AlwaysEdge))
      succOf("x(1)") shouldBe expected(("1", TrueEdge), ("RET", FalseEdge))
    }
  }

  "Cfg for goto" should {
    "be correct for single label" in new CpgCfgFixture("x; goto l1; y; l1: ;") {
      succOf("RET func ()") shouldBe expected(("x", AlwaysEdge))
      succOf("x") shouldBe expected(("goto l1;", AlwaysEdge))
      succOf("goto l1;") shouldBe expected(("l1: ;", AlwaysEdge))
      succOf("l1: ;") shouldBe expected(("RET", AlwaysEdge))
      succOf("y") shouldBe expected(("l1: ;", AlwaysEdge))
    }

    "be correct for GNU goto labels as values" in new CpgCfgFixture("""
        |void *ptr = &&foo;
        |goto *ptr;
        |otherCall();
        |foo: someCall();
        |""".stripMargin) {
      succOf("RET func ()") shouldBe expected(("ptr", AlwaysEdge))
      succOf("ptr") shouldBe expected(("*ptr", AlwaysEdge))
      succOf("foo") shouldBe expected(("&&foo", AlwaysEdge))
      succOf("*ptr = &&foo") shouldBe expected(("goto *;", AlwaysEdge))
      succOf("goto *;") shouldBe expected(("foo: someCall();", AlwaysEdge))
      succOf("foo: someCall();") shouldBe expected(("someCall()", AlwaysEdge))
      succOf("otherCall()") shouldBe expected(("foo: someCall();", AlwaysEdge))
      succOf("someCall()") shouldBe expected(("RET", AlwaysEdge))
    }

    "be correct for multiple labels" in new CpgCfgFixture("x; goto l1; l2: y; l1: ;") {
      succOf("RET func ()") shouldBe expected(("x", AlwaysEdge))
      succOf("x") shouldBe expected(("goto l1;", AlwaysEdge))
      succOf("goto l1;") shouldBe expected(("l1: ;", AlwaysEdge))
      succOf("y") shouldBe expected(("l1: ;", AlwaysEdge))
      succOf("l1: ;") shouldBe expected(("RET", AlwaysEdge))
    }

    "be correct for multiple labels on same spot" in new CpgCfgFixture("x; goto l2; y; l1: ;l2: ;") {
      succOf("RET func ()") shouldBe expected(("x", AlwaysEdge))
      succOf("x") shouldBe expected(("goto l2;", AlwaysEdge))
      succOf("goto l2;") shouldBe expected(("l2: ;", AlwaysEdge))
      succOf("y") shouldBe expected(("l1: ;", AlwaysEdge))
      succOf("l1: ;") shouldBe expected(("l2: ;", AlwaysEdge))
      succOf("l2: ;") shouldBe expected(("RET", AlwaysEdge))
    }

    "work correctly with if block" in new CpgCfgFixture("if(foo) goto end; if(bar) { f(x); } end: ;") {
      succOf("RET func ()") shouldBe expected(("foo", AlwaysEdge))
      succOf("goto end;") shouldBe expected(("end: ;", AlwaysEdge))
    }

  }

  "Cfg for switch" should {
    "be correct with one case" in new CpgCfgFixture("switch (x) { case 1: y; }") {
      succOf("RET func ()") shouldBe expected(("x", AlwaysEdge))
      succOf("x") shouldBe expected(("case 1:", CaseEdge), ("RET", CaseEdge))
      succOf("case 1:") shouldBe expected(("1", AlwaysEdge))
      succOf("1") shouldBe expected(("y", AlwaysEdge))
      succOf("y") shouldBe expected(("RET", AlwaysEdge))
    }

    "be correct with multiple cases" in new CpgCfgFixture("switch (x) { case 1: y; case 2: z;}") {
      succOf("RET func ()") shouldBe expected(("x", AlwaysEdge))
      succOf("x") shouldBe expected(("case 1:", CaseEdge), ("case 2:", CaseEdge), ("RET", CaseEdge))
      succOf("case 1:") shouldBe expected(("1", AlwaysEdge))
      succOf("1") shouldBe expected(("y", AlwaysEdge))
      succOf("y") shouldBe expected(("case 2:", AlwaysEdge))
      succOf("case 2:") shouldBe expected(("2", AlwaysEdge))
      succOf("2") shouldBe expected(("z", AlwaysEdge))
      succOf("z") shouldBe expected(("RET", AlwaysEdge))
    }

    "be correct with multiple cases on same spot" in new CpgCfgFixture("switch (x) { case 1: case 2: y; }") {
      succOf("RET func ()") shouldBe expected(("x", AlwaysEdge))
      succOf("x") shouldBe expected(("case 1:", CaseEdge), ("case 2:", CaseEdge), ("RET", CaseEdge))
      succOf("case 1:") shouldBe expected(("1", AlwaysEdge))
      succOf("1") shouldBe expected(("case 2:", AlwaysEdge))
      succOf("case 2:") shouldBe expected(("2", AlwaysEdge))
      succOf("2") shouldBe expected(("y", AlwaysEdge))
      succOf("y") shouldBe expected(("RET", AlwaysEdge))
    }

    "be correct with multiple cases and multiple cases on same spot" in new CpgCfgFixture(
      "switch (x) { case 1: case 2: y; case 3: z;}"
    ) {
      succOf("RET func ()") shouldBe expected(("x", AlwaysEdge))
      succOf("x") shouldBe expected(
        ("case 1:", CaseEdge),
        ("case 2:", CaseEdge),
        ("case 3:", CaseEdge),
        ("RET", CaseEdge)
      )
      succOf("case 1:") shouldBe expected(("1", AlwaysEdge))
      succOf("1") shouldBe expected(("case 2:", AlwaysEdge))
      succOf("case 2:") shouldBe expected(("2", AlwaysEdge))
      succOf("2") shouldBe expected(("y", AlwaysEdge))
      succOf("y") shouldBe expected(("case 3:", AlwaysEdge))
      succOf("case 3:") shouldBe expected(("3", AlwaysEdge))
      succOf("3") shouldBe expected(("z", AlwaysEdge))
      succOf("z") shouldBe expected(("RET", AlwaysEdge))
    }

    "be correct with default case" in new CpgCfgFixture("switch (x) { default: y; }") {
      succOf("RET func ()") shouldBe expected(("x", AlwaysEdge))
      succOf("x") shouldBe expected(("default:", CaseEdge))
      succOf("default:") shouldBe expected(("y", AlwaysEdge))
      succOf("y") shouldBe expected(("RET", AlwaysEdge))
    }

    "be correct for case and default combined" in new CpgCfgFixture("switch (x) { case 1: y; break; default: z;}") {
      succOf("RET func ()") shouldBe expected(("x", AlwaysEdge))
      succOf("x") shouldBe expected(("case 1:", CaseEdge), ("default:", CaseEdge))
      succOf("case 1:") shouldBe expected(("1", AlwaysEdge))
      succOf("1") shouldBe expected(("y", AlwaysEdge))
      succOf("y") shouldBe expected(("break;", AlwaysEdge))
      succOf("break;") shouldBe expected(("RET", AlwaysEdge))
      succOf("default:") shouldBe expected(("z", AlwaysEdge))
      succOf("z") shouldBe expected(("RET", AlwaysEdge))
    }

    "be correct for nested switch" in new CpgCfgFixture("switch (x) { case 1: switch(y) { default: z; } }") {
      succOf("RET func ()") shouldBe expected(("x", AlwaysEdge))
      succOf("x") shouldBe expected(("case 1:", CaseEdge), ("RET", AlwaysEdge))
      succOf("case 1:") shouldBe expected(("1", AlwaysEdge))
      succOf("1") shouldBe expected(("y", AlwaysEdge))
      succOf("y") shouldBe expected(("default:", CaseEdge))
      succOf("default:") shouldBe expected(("z", AlwaysEdge))
      succOf("z") shouldBe expected(("RET", AlwaysEdge))
    }
  }

  "Cfg for if" should {
    "be correct" in new CpgCfgFixture("if (x) { y; }") {
      succOf("RET func ()") shouldBe expected(("x", AlwaysEdge))
      succOf("x") shouldBe expected(("y", TrueEdge), ("RET", FalseEdge))
      succOf("y") shouldBe expected(("RET", AlwaysEdge))
    }

    "be correct with else block" in new CpgCfgFixture("if (x) { y; } else { z; }") {
      succOf("RET func ()") shouldBe expected(("x", AlwaysEdge))
      succOf("x") shouldBe expected(("y", TrueEdge), ("z", FalseEdge))
      succOf("y") shouldBe expected(("RET", AlwaysEdge))
      succOf("z") shouldBe expected(("RET", AlwaysEdge))
    }

    "be correct with nested if" in new CpgCfgFixture("if (x) { if (y) { z; } }") {
      succOf("RET func ()") shouldBe expected(("x", AlwaysEdge))
      succOf("x") shouldBe expected(("y", TrueEdge), ("RET", FalseEdge))
      succOf("y") shouldBe expected(("z", TrueEdge), ("RET", FalseEdge))
      succOf("z") shouldBe expected(("RET", AlwaysEdge))
    }

    "be correct with else if chain" in new CpgCfgFixture("if (a) { b; } else if (c) { d;} else { e; }") {
      succOf("RET func ()") shouldBe expected(("a", AlwaysEdge))
      succOf("a") shouldBe expected(("b", TrueEdge), ("c", FalseEdge))
      succOf("b") shouldBe expected(("RET", AlwaysEdge))
      succOf("c") shouldBe expected(("d", TrueEdge), ("e", FalseEdge))
      succOf("d") shouldBe expected(("RET", AlwaysEdge))
      succOf("e") shouldBe expected(("RET", AlwaysEdge))
    }
  }

  "Cfg for try" should {
    "be correct for try with a single catch" in new CpgCfgFixture(
      "try { a; } catch (int x) { b; }",
      fileExtension = ".cpp"
    ) {
      succOf("RET func ()") shouldBe expected(("a", AlwaysEdge))
      succOf("a") shouldBe expected(("b", AlwaysEdge), ("RET", AlwaysEdge))
      succOf("b") shouldBe expected(("RET", AlwaysEdge))
    }

    "be correct for try with multiple catches" in new CpgCfgFixture(
      """
        |try {
        |  a;
        |} catch (short x) {
        |  b;
        |} catch (int y) {
        |  c;
        |} catch (long z) {
        |  d;
        |}
        |""".stripMargin,
      fileExtension = ".cpp"
    ) {

      succOf("RET func ()") shouldBe expected(("a", AlwaysEdge))
      // Try should have an edge to all catches and return
      succOf("a") shouldBe expected(("b", AlwaysEdge), ("c", AlwaysEdge), ("d", AlwaysEdge), ("RET", AlwaysEdge))
      // But catches should only have edges to return
      succOf("b") shouldBe expected(("RET", AlwaysEdge))
      succOf("c") shouldBe expected(("RET", AlwaysEdge))
      succOf("d") shouldBe expected(("RET", AlwaysEdge))
    }
  }
}
