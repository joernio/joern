package io.joern.c2cpg.passes.cfg

import io.joern.c2cpg.parser.FileDefaults
import io.joern.c2cpg.testfixtures.CCfgTestCpg
import io.joern.x2cpg.passes.controlflow.cfgcreation.Cfg.*
import io.joern.x2cpg.testfixtures.CfgTestFixture
import io.shiftleft.codepropertygraph.generated.Cpg

class CfgCreationPassTests extends CfgTestFixture(() => new CCfgTestCpg) {
  override def code(code: String): CCfgTestCpg = {
    super.code(s"RET func() { $code }")
  }

  "Cfg" should {
    "contain an entry and exit node at least" in {
      implicit val cpg: Cpg = code("")
      succOf("func") should contain theSameElementsAs expected(("RET", AlwaysEdge))
      succOf("RET") should contain theSameElementsAs expected()
    }

    "be correct for decl statement with assignment" in {
      implicit val cpg: Cpg = code("int x = 1;")
      succOf("func") should contain theSameElementsAs expected(("x", AlwaysEdge))
      succOf("x") should contain theSameElementsAs expected(("1", AlwaysEdge))
      succOf("1") should contain theSameElementsAs expected(("x = 1", AlwaysEdge))
      succOf("x = 1") should contain theSameElementsAs expected(("RET", AlwaysEdge))
    }

    "be correct for nested expression" in {
      implicit val cpg: Cpg = code("x = y + 1;")
      succOf("func") should contain theSameElementsAs expected(("x", AlwaysEdge))
      succOf("x") should contain theSameElementsAs expected(("y", AlwaysEdge))
      succOf("y") should contain theSameElementsAs expected(("1", AlwaysEdge))
      succOf("1") should contain theSameElementsAs expected(("y + 1", AlwaysEdge))
      succOf("y + 1") should contain theSameElementsAs expected(("x = y + 1", AlwaysEdge))
      succOf("x = y + 1") should contain theSameElementsAs expected(("RET", AlwaysEdge))
    }

    "be correct for return statement" in {
      implicit val cpg: Cpg = code("return x;")
      succOf("func") should contain theSameElementsAs expected(("x", AlwaysEdge))
      succOf("x") should contain theSameElementsAs expected(("return x;", AlwaysEdge))
      succOf("return x;") should contain theSameElementsAs expected(("RET", AlwaysEdge))
    }

    "be correct for consecutive return statements" in {
      implicit val cpg: Cpg = code("return x; return y;")
      succOf("func") should contain theSameElementsAs expected(("x", AlwaysEdge))
      succOf("x") should contain theSameElementsAs expected(("return x;", AlwaysEdge))
      succOf("y") should contain theSameElementsAs expected(("return y;", AlwaysEdge))
      succOf("return x;") should contain theSameElementsAs expected(("RET", AlwaysEdge))
      succOf("return y;") should contain theSameElementsAs expected(("RET", AlwaysEdge))
    }

    "be correct for void return statement" in {
      implicit val cpg: Cpg = code("return;")
      succOf("func") should contain theSameElementsAs expected(("return;", AlwaysEdge))
      succOf("return;") should contain theSameElementsAs expected(("RET", AlwaysEdge))
    }

    "be correct for call expression" in {
      implicit val cpg: Cpg = code("foo(a + 1, b);")
      succOf("func") should contain theSameElementsAs expected(("a", AlwaysEdge))
      succOf("a") should contain theSameElementsAs expected(("1", AlwaysEdge))
      succOf("1") should contain theSameElementsAs expected(("a + 1", AlwaysEdge))
      succOf("a + 1") should contain theSameElementsAs expected(("b", AlwaysEdge))
      succOf("b") should contain theSameElementsAs expected(("foo(a + 1, b)", AlwaysEdge))
      succOf("foo(a + 1, b)") should contain theSameElementsAs expected(("RET", AlwaysEdge))
    }

    "be correct for unary expression '+'" in {
      implicit val cpg: Cpg = code("+x;")
      succOf("func") should contain theSameElementsAs expected(("x", AlwaysEdge))
      succOf("x") should contain theSameElementsAs expected(("+x", AlwaysEdge))
      succOf("+x") should contain theSameElementsAs expected(("RET", AlwaysEdge))
    }

    "be correct for unary expression '++'" in {
      implicit val cpg: Cpg = code("++x;")
      succOf("func") should contain theSameElementsAs expected(("x", AlwaysEdge))
      succOf("x") should contain theSameElementsAs expected(("++x", AlwaysEdge))
      succOf("++x") should contain theSameElementsAs expected(("RET", AlwaysEdge))
    }

    "be correct for conditional expression" in {
      implicit val cpg: Cpg = code("x ? y : z;")
      succOf("func") should contain theSameElementsAs expected(("x", AlwaysEdge))
      succOf("x") should contain theSameElementsAs expected(("y", TrueEdge), ("z", FalseEdge))
      succOf("y") should contain theSameElementsAs expected(("x ? y : z", AlwaysEdge))
      succOf("z") should contain theSameElementsAs expected(("x ? y : z", AlwaysEdge))
      succOf("x ? y : z") should contain theSameElementsAs expected(("RET", AlwaysEdge))
    }

    "be correct for conditional expression with empty then" in {
      implicit val cpg: Cpg = code("x ? : z;")
      succOf("func") should contain theSameElementsAs expected(("x", AlwaysEdge))
      succOf("x") should contain theSameElementsAs expected(("x ? : z", TrueEdge), ("z", FalseEdge))
      succOf("z") should contain theSameElementsAs expected(("x ? : z", AlwaysEdge))
      succOf("x ? : z") should contain theSameElementsAs expected(("RET", AlwaysEdge))
    }

    "be correct for short-circuit AND expression" in {
      implicit val cpg: Cpg = code("int z = x && y;")
      succOf("func") should contain theSameElementsAs expected(("z", AlwaysEdge))
      succOf("z") should contain theSameElementsAs expected(("x", AlwaysEdge))
      succOf("x") should contain theSameElementsAs expected(("y", TrueEdge), ("x && y", FalseEdge))
      succOf("y") should contain theSameElementsAs expected(("x && y", AlwaysEdge))
      succOf("x && y") should contain theSameElementsAs expected(("z = x && y", AlwaysEdge))
      succOf("z = x && y") should contain theSameElementsAs expected(("RET", AlwaysEdge))
    }

    "be correct for short-circuit OR expression" in {
      implicit val cpg: Cpg = code("x || y;")
      succOf("func") should contain theSameElementsAs expected(("x", AlwaysEdge))
      succOf("x") should contain theSameElementsAs expected(("y", FalseEdge), ("x || y", TrueEdge))
      succOf("y") should contain theSameElementsAs expected(("x || y", AlwaysEdge))
      succOf("x || y") should contain theSameElementsAs expected(("RET", AlwaysEdge))
    }
  }

  "Cfg for while-loop" should {
    "be correct" in {
      implicit val cpg: Cpg = code("while (x < 1) { y = 2; }")
      succOf("func") should contain theSameElementsAs expected(("x", AlwaysEdge))
      succOf("x") should contain theSameElementsAs expected(("1", AlwaysEdge))
      succOf("1") should contain theSameElementsAs expected(("x < 1", AlwaysEdge))
      succOf("x < 1") should contain theSameElementsAs expected(("y", TrueEdge), ("RET", FalseEdge))
      succOf("y") should contain theSameElementsAs expected(("2", AlwaysEdge))
      succOf("2") should contain theSameElementsAs expected(("y = 2", AlwaysEdge))
      succOf("y = 2") should contain theSameElementsAs expected(("x", AlwaysEdge))
    }

    "be correct with break" in {
      implicit val cpg: Cpg = code("while (x < 1) { break; y; }")
      succOf("func") should contain theSameElementsAs expected(("x", AlwaysEdge))
      succOf("x") should contain theSameElementsAs expected(("1", AlwaysEdge))
      succOf("1") should contain theSameElementsAs expected(("x < 1", AlwaysEdge))
      succOf("x < 1") should contain theSameElementsAs expected(("break;", TrueEdge), ("RET", FalseEdge))
      succOf("break;") should contain theSameElementsAs expected(("RET", AlwaysEdge))
      succOf("y") should contain theSameElementsAs expected(("x", AlwaysEdge))
    }

    "be correct with continue" in {
      implicit val cpg: Cpg = code("while (x < 1) { continue; y; }")
      succOf("func") should contain theSameElementsAs expected(("x", AlwaysEdge))
      succOf("x") should contain theSameElementsAs expected(("1", AlwaysEdge))
      succOf("1") should contain theSameElementsAs expected(("x < 1", AlwaysEdge))
      succOf("x < 1") should contain theSameElementsAs expected(("continue;", TrueEdge), ("RET", FalseEdge))
      succOf("continue;") should contain theSameElementsAs expected(("x", AlwaysEdge))
      succOf("y") should contain theSameElementsAs expected(("x", AlwaysEdge))
    }

    "be correct with nested while-loop" in {
      implicit val cpg: Cpg = code("while (x) { while (y) { z; }}")
      succOf("func") should contain theSameElementsAs expected(("x", AlwaysEdge))
      succOf("x") should contain theSameElementsAs expected(("y", TrueEdge), ("RET", FalseEdge))
      succOf("y") should contain theSameElementsAs expected(("z", TrueEdge), ("x", FalseEdge))
      succOf("z") should contain theSameElementsAs expected(("y", AlwaysEdge))
    }
  }

  "Cfg for do-while-loop" should {
    "be correct" in {
      implicit val cpg: Cpg = code("do { y = 2; } while (x < 1);")
      succOf("func") should contain theSameElementsAs expected(("y", AlwaysEdge))
      succOf("y") should contain theSameElementsAs expected(("2", AlwaysEdge))
      succOf("2") should contain theSameElementsAs expected(("y = 2", AlwaysEdge))
      succOf("y = 2") should contain theSameElementsAs expected(("x", AlwaysEdge))
      succOf("x") should contain theSameElementsAs expected(("1", AlwaysEdge))
      succOf("1") should contain theSameElementsAs expected(("x < 1", AlwaysEdge))
      succOf("x < 1") should contain theSameElementsAs expected(("y", TrueEdge), ("RET", FalseEdge))
    }

    "be correct with break" in {
      implicit val cpg: Cpg = code("do { break; y; } while (x < 1);")
      succOf("func") should contain theSameElementsAs expected(("break;", AlwaysEdge))
      succOf("break;") should contain theSameElementsAs expected(("RET", AlwaysEdge))
      succOf("y") should contain theSameElementsAs expected(("x", AlwaysEdge))
      succOf("x") should contain theSameElementsAs expected(("1", AlwaysEdge))
      succOf("1") should contain theSameElementsAs expected(("x < 1", AlwaysEdge))
      succOf("x < 1") should contain theSameElementsAs expected(("break;", TrueEdge), ("RET", FalseEdge))
    }

    "be correct with continue" in {
      implicit val cpg: Cpg = code("do { continue; y; } while (x < 1);")
      succOf("func") should contain theSameElementsAs expected(("continue;", AlwaysEdge))
      succOf("continue;") should contain theSameElementsAs expected(("x", AlwaysEdge))
      succOf("y") should contain theSameElementsAs expected(("x", AlwaysEdge))
      succOf("x") should contain theSameElementsAs expected(("1", AlwaysEdge))
      succOf("1") should contain theSameElementsAs expected(("x < 1", AlwaysEdge))
      succOf("x < 1") should contain theSameElementsAs expected(("continue;", TrueEdge), ("RET", FalseEdge))
    }

    "be correct with nested do-while-loop" in {
      implicit val cpg: Cpg = code("do { do { x; } while (y); } while (z);")
      succOf("func") should contain theSameElementsAs expected(("x", AlwaysEdge))
      succOf("x") should contain theSameElementsAs expected(("y", AlwaysEdge))
      succOf("y") should contain theSameElementsAs expected(("x", TrueEdge), ("z", FalseEdge))
      succOf("z") should contain theSameElementsAs expected(("x", TrueEdge), ("RET", FalseEdge))
    }

    "be correct for do-while-loop with empty body" in {
      implicit val cpg: Cpg = code("do { } while(x > 1);")
      succOf("func") should contain theSameElementsAs expected(("x", AlwaysEdge))
      succOf("1") should contain theSameElementsAs expected(("x > 1", AlwaysEdge))
      succOf("x > 1") should contain theSameElementsAs expected(("x", TrueEdge), ("RET", FalseEdge))
    }

    "be correct with multiple macro calls" in {
      implicit val cpg: Cpg = code(
        """
          |#define deleteReset(ptr) do { delete ptr; ptr = nullptr; } while(0)
          |void func(void) {
          |  int *foo = new int;
          |  int *bar = new int;
          |  int *baz = new int;
          |  deleteReset(foo);
          |  deleteReset(bar);
          |  deleteReset(baz);
          |}
          |""".stripMargin,
        "foo.cc"
      )
      succOf("deleteReset(foo)") should contain theSameElementsAs expected(("foo", 2, AlwaysEdge), ("bar", AlwaysEdge))
      succOf("foo", 2) should contain theSameElementsAs expected(("delete foo", AlwaysEdge))
      succOf("deleteReset(bar)") should contain theSameElementsAs expected(("bar", 2, AlwaysEdge), ("baz", AlwaysEdge))
      succOf("bar", 2) should contain theSameElementsAs expected(("delete bar", AlwaysEdge))
      succOf("deleteReset(baz)") should contain theSameElementsAs expected(("baz", 2, AlwaysEdge), ("RET", AlwaysEdge))
      succOf("baz", 2) should contain theSameElementsAs expected(("delete baz", AlwaysEdge))
    }
  }

  "Cfg for for-loop" should {
    "be correct" in {
      implicit val cpg: Cpg = code("for (x = 0; y < 1; z += 2) { a = 3; }")
      succOf("func") should contain theSameElementsAs expected(("x", AlwaysEdge))
      succOf("x") should contain theSameElementsAs expected(("0", AlwaysEdge))
      succOf("0") should contain theSameElementsAs expected(("x = 0", AlwaysEdge))
      succOf("x = 0") should contain theSameElementsAs expected(("y", AlwaysEdge))
      succOf("y") should contain theSameElementsAs expected(("1", AlwaysEdge))
      succOf("1") should contain theSameElementsAs expected(("y < 1", AlwaysEdge))
      succOf("y < 1") should contain theSameElementsAs expected(("a", TrueEdge), ("RET", FalseEdge))
      succOf("a") should contain theSameElementsAs expected(("3", AlwaysEdge))
      succOf("3") should contain theSameElementsAs expected(("a = 3", AlwaysEdge))
      succOf("a = 3") should contain theSameElementsAs expected(("z", AlwaysEdge))
      succOf("z") should contain theSameElementsAs expected(("2", AlwaysEdge))
      succOf("2") should contain theSameElementsAs expected(("z += 2", AlwaysEdge))
      succOf("z += 2") should contain theSameElementsAs expected(("y", AlwaysEdge))
    }

    "be correct with break" in {
      implicit val cpg: Cpg = code("for (x = 0; y < 1; z += 2) { break; a = 3; }")
      succOf("func") should contain theSameElementsAs expected(("x", AlwaysEdge))
      succOf("x") should contain theSameElementsAs expected(("0", AlwaysEdge))
      succOf("x = 0") should contain theSameElementsAs expected(("y", AlwaysEdge))
      succOf("y") should contain theSameElementsAs expected(("1", AlwaysEdge))
      succOf("1") should contain theSameElementsAs expected(("y < 1", AlwaysEdge))
      succOf("y < 1") should contain theSameElementsAs expected(("break;", TrueEdge), ("RET", FalseEdge))
      succOf("break;") should contain theSameElementsAs expected(("RET", AlwaysEdge))
      succOf("a") should contain theSameElementsAs expected(("3", AlwaysEdge))
      succOf("3") should contain theSameElementsAs expected(("a = 3", AlwaysEdge))
      succOf("a = 3") should contain theSameElementsAs expected(("z", AlwaysEdge))
      succOf("z") should contain theSameElementsAs expected(("2", AlwaysEdge))
      succOf("2") should contain theSameElementsAs expected(("z += 2", AlwaysEdge))
      succOf("z += 2") should contain theSameElementsAs expected(("y", AlwaysEdge))
    }

    "be correct with continue" in {
      implicit val cpg: Cpg = code("for (x = 0; y < 1; z += 2) { continue; a = 3; }")
      succOf("func") should contain theSameElementsAs expected(("x", AlwaysEdge))
      succOf("x") should contain theSameElementsAs expected(("0", AlwaysEdge))
      succOf("0") should contain theSameElementsAs expected(("x = 0", AlwaysEdge))
      succOf("x = 0") should contain theSameElementsAs expected(("y", AlwaysEdge))
      succOf("y") should contain theSameElementsAs expected(("1", AlwaysEdge))
      succOf("1") should contain theSameElementsAs expected(("y < 1", AlwaysEdge))
      succOf("y < 1") should contain theSameElementsAs expected(("continue;", TrueEdge), ("RET", FalseEdge))
      succOf("continue;") should contain theSameElementsAs expected(("z", AlwaysEdge))
      succOf("a") should contain theSameElementsAs expected(("3", AlwaysEdge))
      succOf("3") should contain theSameElementsAs expected(("a = 3", AlwaysEdge))
      succOf("a = 3") should contain theSameElementsAs expected(("z", AlwaysEdge))
      succOf("z") should contain theSameElementsAs expected(("2", AlwaysEdge))
      succOf("2") should contain theSameElementsAs expected(("z += 2", AlwaysEdge))
      succOf("z += 2") should contain theSameElementsAs expected(("y", AlwaysEdge))
    }

    "be correct with nested for-loop" in {
      implicit val cpg: Cpg = code("for (x; y; z) { for (a; b; c) { u; } }")
      succOf("func") should contain theSameElementsAs expected(("x", AlwaysEdge))
      succOf("x") should contain theSameElementsAs expected(("y", AlwaysEdge))
      succOf("y") should contain theSameElementsAs expected(("a", TrueEdge), ("RET", FalseEdge))
      succOf("z") should contain theSameElementsAs expected(("y", AlwaysEdge))
      succOf("a") should contain theSameElementsAs expected(("b", AlwaysEdge))
      succOf("b") should contain theSameElementsAs expected(("u", TrueEdge), ("z", FalseEdge))
      succOf("c") should contain theSameElementsAs expected(("b", AlwaysEdge))
      succOf("u") should contain theSameElementsAs expected(("c", AlwaysEdge))
    }

    "be correct with empty condition" in {
      implicit val cpg: Cpg = code("for (;;) { a = 1; }")
      succOf("func") should contain theSameElementsAs expected(("a", AlwaysEdge))
      succOf("a") should contain theSameElementsAs expected(("1", AlwaysEdge))
      succOf("1") should contain theSameElementsAs expected(("a = 1", AlwaysEdge))
      succOf("a = 1") should contain theSameElementsAs expected(("a", AlwaysEdge))
    }

    "be correct with empty condition with break" in {
      implicit val cpg: Cpg = code("for (;;) { break; }")
      succOf("func") should contain theSameElementsAs expected(("break;", AlwaysEdge))
      succOf("break;") should contain theSameElementsAs expected(("RET", AlwaysEdge))
    }

    "be correct with empty condition with continue" in {
      implicit val cpg: Cpg = code("for (;;) { continue ; }")
      succOf("func") should contain theSameElementsAs expected(("continue ;", AlwaysEdge))
      succOf("continue ;") should contain theSameElementsAs expected(("continue ;", AlwaysEdge))
    }

    "be correct with empty condition with nested empty for-loop" in {
      implicit val cpg: Cpg = code("for (;;) { for (;;) { x; } }")

      succOf("func") should contain theSameElementsAs expected(("x", AlwaysEdge))
      succOf("x") should contain theSameElementsAs expected(("x", AlwaysEdge))
    }

    "be correct with empty condition with empty block" in {
      implicit val cpg: Cpg = code("for (;;) ;")
      succOf("func") should contain theSameElementsAs expected(("RET", AlwaysEdge))
    }

    "be correct when empty for-loop is skipped" in {
      implicit val cpg: Cpg = code("for (;;) {}; return;")
      succOf("func") should contain theSameElementsAs expected(("return;", AlwaysEdge))
      succOf("return;") should contain theSameElementsAs expected(("RET", AlwaysEdge))
    }

    "be correct with function call condition with empty block" in {
      implicit val cpg: Cpg = code("for (; x(1);) ;")
      succOf("func") should contain theSameElementsAs expected(("1", AlwaysEdge))
      succOf("1") should contain theSameElementsAs expected(("x(1)", AlwaysEdge))
      succOf("x(1)") should contain theSameElementsAs expected(("1", TrueEdge), ("RET", FalseEdge))
    }
  }

  "Cfg for goto" should {
    "be correct for single label" in {
      implicit val cpg: Cpg = code("x; goto l1; y; l1: ;")
      succOf("func") should contain theSameElementsAs expected(("x", AlwaysEdge))
      succOf("x") should contain theSameElementsAs expected(("goto l1;", AlwaysEdge))
      succOf("goto l1;") should contain theSameElementsAs expected(("l1: ;", AlwaysEdge))
      succOf("l1: ;") should contain theSameElementsAs expected(("RET", AlwaysEdge))
      succOf("y") should contain theSameElementsAs expected(("l1: ;", AlwaysEdge))
    }

    "be correct for GNU goto labels as values" in {
      implicit val cpg: Cpg = code("""
                                           |void *ptr = &&foo;
                                           |goto *ptr;
                                           |otherCall();
                                           |foo: someCall();
                                           |""".stripMargin)
      succOf("func") should contain theSameElementsAs expected(("ptr", AlwaysEdge))
      succOf("ptr") should contain theSameElementsAs expected(("foo", AlwaysEdge))
      succOf("ptr", 1) should contain theSameElementsAs expected(("*ptr", AlwaysEdge))
      succOf("foo") should contain theSameElementsAs expected(("&&foo", AlwaysEdge))
      succOf("*ptr = &&foo") should contain theSameElementsAs expected(("goto *;", AlwaysEdge))
      succOf("goto *;") should contain theSameElementsAs expected(("foo: someCall();", AlwaysEdge))
      succOf("foo: someCall();") should contain theSameElementsAs expected(("someCall()", AlwaysEdge))
      succOf("otherCall()") should contain theSameElementsAs expected(("foo: someCall();", AlwaysEdge))
      succOf("someCall()") should contain theSameElementsAs expected(("RET", AlwaysEdge))
    }

    "be correct for multiple labels" in {
      implicit val cpg: Cpg = code("x; goto l1; l2: y; l1: ;")
      succOf("func") should contain theSameElementsAs expected(("x", AlwaysEdge))
      succOf("x") should contain theSameElementsAs expected(("goto l1;", AlwaysEdge))
      succOf("goto l1;") should contain theSameElementsAs expected(("l1: ;", AlwaysEdge))
      succOf("y") should contain theSameElementsAs expected(("l1: ;", AlwaysEdge))
      succOf("l1: ;") should contain theSameElementsAs expected(("RET", AlwaysEdge))
    }

    "be correct for multiple labels on same spot" in {
      implicit val cpg: Cpg = code("x; goto l2; y; l1: ;l2: ;")
      succOf("func") should contain theSameElementsAs expected(("x", AlwaysEdge))
      succOf("x") should contain theSameElementsAs expected(("goto l2;", AlwaysEdge))
      succOf("goto l2;") should contain theSameElementsAs expected(("l2: ;", AlwaysEdge))
      succOf("y") should contain theSameElementsAs expected(("l1: ;", AlwaysEdge))
      succOf("l1: ;") should contain theSameElementsAs expected(("l2: ;", AlwaysEdge))
      succOf("l2: ;") should contain theSameElementsAs expected(("RET", AlwaysEdge))
    }

    "work correctly with if block" in {
      implicit val cpg: Cpg = code("if(foo) goto end; if(bar) { f(x); } end: ;")
      succOf("func") should contain theSameElementsAs expected(("foo", AlwaysEdge))
      succOf("goto end;") should contain theSameElementsAs expected(("end: ;", AlwaysEdge))
    }

  }

  "Cfg for switch" should {
    "be correct with one case" in {
      implicit val cpg: Cpg = code("switch (x) { case 1: y; }")
      succOf("func") should contain theSameElementsAs expected(("x", AlwaysEdge))
      succOf("x") should contain theSameElementsAs expected(("case 1:", CaseEdge), ("RET", CaseEdge))
      succOf("case 1:") should contain theSameElementsAs expected(("1", AlwaysEdge))
      succOf("1") should contain theSameElementsAs expected(("y", AlwaysEdge))
      succOf("y") should contain theSameElementsAs expected(("RET", AlwaysEdge))
    }

    "be correct with multiple cases" in {
      implicit val cpg: Cpg = code("switch (x) { case 1: y; case 2: z;}")
      succOf("func") should contain theSameElementsAs expected(("x", AlwaysEdge))
      succOf("x") should contain theSameElementsAs expected(
        ("case 1:", CaseEdge),
        ("case 2:", CaseEdge),
        ("RET", CaseEdge)
      )
      succOf("case 1:") should contain theSameElementsAs expected(("1", AlwaysEdge))
      succOf("1") should contain theSameElementsAs expected(("y", AlwaysEdge))
      succOf("y") should contain theSameElementsAs expected(("case 2:", AlwaysEdge))
      succOf("case 2:") should contain theSameElementsAs expected(("2", AlwaysEdge))
      succOf("2") should contain theSameElementsAs expected(("z", AlwaysEdge))
      succOf("z") should contain theSameElementsAs expected(("RET", AlwaysEdge))
    }

    "be correct with multiple cases on same spot" in {
      implicit val cpg: Cpg = code("switch (x) { case 1: case 2: y; }")
      succOf("func") should contain theSameElementsAs expected(("x", AlwaysEdge))
      succOf("x") should contain theSameElementsAs expected(
        ("case 1:", CaseEdge),
        ("case 2:", CaseEdge),
        ("RET", CaseEdge)
      )
      succOf("case 1:") should contain theSameElementsAs expected(("1", AlwaysEdge))
      succOf("1") should contain theSameElementsAs expected(("case 2:", AlwaysEdge))
      succOf("case 2:") should contain theSameElementsAs expected(("2", AlwaysEdge))
      succOf("2") should contain theSameElementsAs expected(("y", AlwaysEdge))
      succOf("y") should contain theSameElementsAs expected(("RET", AlwaysEdge))
    }

    "be correct with multiple cases and multiple cases on same spot" in {
      implicit val cpg: Cpg = code("switch (x) { case 1: case 2: y; case 3: z;}")
      succOf("func") should contain theSameElementsAs expected(("x", AlwaysEdge))
      succOf("x") should contain theSameElementsAs expected(
        ("case 1:", CaseEdge),
        ("case 2:", CaseEdge),
        ("case 3:", CaseEdge),
        ("RET", CaseEdge)
      )
      succOf("case 1:") should contain theSameElementsAs expected(("1", AlwaysEdge))
      succOf("1") should contain theSameElementsAs expected(("case 2:", AlwaysEdge))
      succOf("case 2:") should contain theSameElementsAs expected(("2", AlwaysEdge))
      succOf("2") should contain theSameElementsAs expected(("y", AlwaysEdge))
      succOf("y") should contain theSameElementsAs expected(("case 3:", AlwaysEdge))
      succOf("case 3:") should contain theSameElementsAs expected(("3", AlwaysEdge))
      succOf("3") should contain theSameElementsAs expected(("z", AlwaysEdge))
      succOf("z") should contain theSameElementsAs expected(("RET", AlwaysEdge))
    }

    "be correct with default case" in {
      implicit val cpg: Cpg = code("switch (x) { default: y; }")
      succOf("func") should contain theSameElementsAs expected(("x", AlwaysEdge))
      succOf("x") should contain theSameElementsAs expected(("default:", CaseEdge))
      succOf("default:") should contain theSameElementsAs expected(("y", AlwaysEdge))
      succOf("y") should contain theSameElementsAs expected(("RET", AlwaysEdge))
    }

    "be correct for case and default combined" in {
      implicit val cpg: Cpg = code("switch (x) { case 1: y; break; default: z;}")

      succOf("func") should contain theSameElementsAs expected(("x", AlwaysEdge))
      succOf("x") should contain theSameElementsAs expected(("case 1:", CaseEdge), ("default:", CaseEdge))
      succOf("case 1:") should contain theSameElementsAs expected(("1", AlwaysEdge))
      succOf("1") should contain theSameElementsAs expected(("y", AlwaysEdge))
      succOf("y") should contain theSameElementsAs expected(("break;", AlwaysEdge))
      succOf("break;") should contain theSameElementsAs expected(("RET", AlwaysEdge))
      succOf("default:") should contain theSameElementsAs expected(("z", AlwaysEdge))
      succOf("z") should contain theSameElementsAs expected(("RET", AlwaysEdge))
    }

    "be correct for nested switch" in {
      implicit val cpg: Cpg = code("switch (x) { case 1: switch(y) { default: z; } }")
      succOf("func") should contain theSameElementsAs expected(("x", AlwaysEdge))
      succOf("x") should contain theSameElementsAs expected(("case 1:", CaseEdge), ("RET", AlwaysEdge))
      succOf("case 1:") should contain theSameElementsAs expected(("1", AlwaysEdge))
      succOf("1") should contain theSameElementsAs expected(("y", AlwaysEdge))
      succOf("y") should contain theSameElementsAs expected(("default:", CaseEdge))
      succOf("default:") should contain theSameElementsAs expected(("z", AlwaysEdge))
      succOf("z") should contain theSameElementsAs expected(("RET", AlwaysEdge))
    }

    "be correct for switch containing continue statement" in {
      implicit val cpg: Cpg = code("""
          |while (i < 1) {
          |  switch (j) {
          |    case 0:
          |      continue;
          |  }
          |}
          |""".stripMargin)
      succOf("continue;") should contain theSameElementsAs expected(("i", AlwaysEdge))
    }
  }

  "Cfg for if" should {
    "be correct" in {
      implicit val cpg: Cpg = code("if (x) { y; }")
      succOf("func") should contain theSameElementsAs expected(("x", AlwaysEdge))
      succOf("x") should contain theSameElementsAs expected(("y", TrueEdge), ("RET", FalseEdge))
      succOf("y") should contain theSameElementsAs expected(("RET", AlwaysEdge))
    }

    "be correct with else block" in {
      implicit val cpg: Cpg = code("if (x) { y; } else { z; }")
      succOf("func") should contain theSameElementsAs expected(("x", AlwaysEdge))
      succOf("x") should contain theSameElementsAs expected(("y", TrueEdge), ("z", FalseEdge))
      succOf("y") should contain theSameElementsAs expected(("RET", AlwaysEdge))
      succOf("z") should contain theSameElementsAs expected(("RET", AlwaysEdge))
    }

    "be correct with nested if" in {
      implicit val cpg: Cpg = code("if (x) { if (y) { z; } }")
      succOf("func") should contain theSameElementsAs expected(("x", AlwaysEdge))
      succOf("x") should contain theSameElementsAs expected(("y", TrueEdge), ("RET", FalseEdge))
      succOf("y") should contain theSameElementsAs expected(("z", TrueEdge), ("RET", FalseEdge))
      succOf("z") should contain theSameElementsAs expected(("RET", AlwaysEdge))
    }

    "be correct with else if chain" in {
      implicit val cpg: Cpg = code("if (a) { b; } else if (c) { d;} else { e; }")
      succOf("func") should contain theSameElementsAs expected(("a", AlwaysEdge))
      succOf("a") should contain theSameElementsAs expected(("b", TrueEdge), ("c", FalseEdge))
      succOf("b") should contain theSameElementsAs expected(("RET", AlwaysEdge))
      succOf("c") should contain theSameElementsAs expected(("d", TrueEdge), ("e", FalseEdge))
      succOf("d") should contain theSameElementsAs expected(("RET", AlwaysEdge))
      succOf("e") should contain theSameElementsAs expected(("RET", AlwaysEdge))
    }

    "be correct for empty 'then' block" in {
      implicit val cpg: Cpg = code("if (cond()) {} else { foo(); }")
      succOf("func") should contain theSameElementsAs expected(("cond()", AlwaysEdge))
      succOf("cond()") should contain theSameElementsAs expected(("RET", TrueEdge), ("foo()", FalseEdge))
      succOf("foo()") should contain theSameElementsAs expected(("RET", AlwaysEdge))
    }

    "be correct for empty 'else' block" in {
      implicit val cpg: Cpg = code("if (cond()) {foo();} else {}")
      succOf("func") should contain theSameElementsAs expected(("cond()", AlwaysEdge))
      succOf("cond()") should contain theSameElementsAs expected(("RET", FalseEdge), ("foo()", TrueEdge))
      succOf("foo()") should contain theSameElementsAs expected(("RET", AlwaysEdge))
    }

    "be correct for empty 'then' and 'else' block" in {
      implicit val cpg: Cpg = code("if (cond()) {} else {}")
      succOf("func") should contain theSameElementsAs expected(("cond()", AlwaysEdge))
      succOf("cond()") should contain theSameElementsAs expected(("RET", AlwaysEdge))
    }
  }
}

class CppCfgCreationPassTests extends CfgTestFixture(() => new CCfgTestCpg(FileDefaults.CppExt)) {
  override def code(code: String): CCfgTestCpg = {
    super.code(s"RET func() { $code }")
  }

  "Cfg for try" should {
    "be correct for try with a single catch" in {
      implicit val cpg: Cpg = code("try { a; } catch (int x) { b; }")

      succOf("func") should contain theSameElementsAs expected(("a", AlwaysEdge))
      succOf("a") should contain theSameElementsAs expected(("b", AlwaysEdge), ("RET", AlwaysEdge))
      succOf("b") should contain theSameElementsAs expected(("RET", AlwaysEdge))
    }

    "be correct for try with multiple catches" in {
      implicit val cpg: Cpg = code("""
       |try {
       |  a;
       |} catch (short x) {
       |  b;
       |} catch (int y) {
       |  c;
       |} catch (long z) {
       |  d;
       |}
       |""".stripMargin)
      succOf("func") should contain theSameElementsAs expected(("a", AlwaysEdge))
      // Try should have an edge to all catches and return
      succOf("a") should contain theSameElementsAs expected(
        ("b", AlwaysEdge),
        ("c", AlwaysEdge),
        ("d", AlwaysEdge),
        ("RET", AlwaysEdge)
      )
      // But catches should only have edges to return
      succOf("b") should contain theSameElementsAs expected(("RET", AlwaysEdge))
      succOf("c") should contain theSameElementsAs expected(("RET", AlwaysEdge))
      succOf("d") should contain theSameElementsAs expected(("RET", AlwaysEdge))
    }

    "be correct for try with multiple returns" in {
      implicit val cpg: Cpg = code("""
          |try {
          |  if (1+1) {
          |    return foo();
          |  }
          |  return bar();
          |} catch (int x) {}
          |""".stripMargin)
      succOf("foo()") should contain theSameElementsAs expected(("return foo();", AlwaysEdge))
      succOf("return foo();") should contain theSameElementsAs expected(("RET", AlwaysEdge))
      succOf("bar()") should contain theSameElementsAs expected(("return bar();", AlwaysEdge), ("RET", AlwaysEdge))
    }

    "be correct for throw statement" in {
      implicit val cpg: Cpg = code("""
       |throw foo();
       |bar();
       |""".stripMargin)
      succOf("func") should contain theSameElementsAs expected(("foo()", AlwaysEdge))
      succOf("foo()") should contain theSameElementsAs expected(("throw foo()", AlwaysEdge))
      succOf("throw foo()") should contain theSameElementsAs expected(("RET", AlwaysEdge))
      succOf("bar()") should contain theSameElementsAs expected(("RET", AlwaysEdge))
    }

    "be correct for throw statement in if-else" in {
      implicit val cpg: Cpg = code("""
       |if (true) throw foo();
       |else bar();
       |""".stripMargin)
      succOf("func") should contain theSameElementsAs expected(("true", AlwaysEdge))
      succOf("true") should contain theSameElementsAs expected(("foo()", TrueEdge), ("bar()", FalseEdge))
      succOf("foo()") should contain theSameElementsAs expected(("throw foo()", AlwaysEdge))
      succOf("throw foo()") should contain theSameElementsAs expected(("RET", AlwaysEdge))
      succOf("bar()") should contain theSameElementsAs expected(("RET", AlwaysEdge))
    }
  }
}
