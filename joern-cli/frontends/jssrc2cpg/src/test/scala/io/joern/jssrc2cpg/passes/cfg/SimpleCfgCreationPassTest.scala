package io.joern.jssrc2cpg.passes.cfg

import io.joern.x2cpg.passes.controlflow.cfgcreation.Cfg.AlwaysEdge
import io.joern.x2cpg.passes.controlflow.cfgcreation.Cfg.CaseEdge
import io.joern.x2cpg.passes.controlflow.cfgcreation.Cfg.FalseEdge
import io.joern.x2cpg.passes.controlflow.cfgcreation.Cfg.TrueEdge
import io.shiftleft.codepropertygraph.generated.NodeTypes
import io.shiftleft.codepropertygraph.Cpg

class SimpleCfgCreationPassTest extends AbstractCfgPassTest {

  "CFG generation for simple fragments" should {
    "have correct structure for block expression" in CfgFixture("let x = (class Foo {}, bar())") { implicit cpg =>
      succOf(":program") shouldBe expected(("x", AlwaysEdge))
      succOf("x") shouldBe expected(("class Foo", AlwaysEdge))
      succOf("class Foo") shouldBe expected(("bar", AlwaysEdge))
      succOf("bar") shouldBe expected(("this", AlwaysEdge))
      succOf("this", NodeTypes.IDENTIFIER) shouldBe expected(("bar()", AlwaysEdge))
      succOf("bar()") shouldBe expected(("x = (class Foo {}, bar())", AlwaysEdge))
      succOf("x = (class Foo {}, bar())") shouldBe expected(("RET", AlwaysEdge))
    }

    "have correct structure for empty array literal" in CfgFixture("var x = []") { implicit cpg =>
      succOf(":program") shouldBe expected(("x", AlwaysEdge))
      succOf("x") shouldBe expected(("__ecma.Array.factory()", AlwaysEdge))
      succOf("__ecma.Array.factory()") shouldBe expected(("x = []", AlwaysEdge))
    }

    "have correct structure for array literal with values" in CfgFixture("var x = [1, 2]") { implicit cpg =>
      succOf(":program") shouldBe expected(("x", AlwaysEdge))
      succOf("x") shouldBe expected(("_tmp_0", AlwaysEdge))
      succOf("_tmp_0") shouldBe expected(("__ecma.Array.factory()", AlwaysEdge))
      succOf("__ecma.Array.factory()") shouldBe expected(("_tmp_0 = __ecma.Array.factory()", AlwaysEdge))

      succOf("_tmp_0 = __ecma.Array.factory()") shouldBe expected(("_tmp_0", 1, AlwaysEdge))
      succOf("_tmp_0", 1) shouldBe expected(("push", AlwaysEdge))
      succOf("push") shouldBe expected(("_tmp_0.push", AlwaysEdge))
      succOf("_tmp_0.push") shouldBe expected(("_tmp_0", 2, AlwaysEdge))
      succOf("_tmp_0", 2) shouldBe expected(("1", AlwaysEdge))
      succOf("1") shouldBe expected(("_tmp_0.push(1)", AlwaysEdge))

      succOf("_tmp_0.push(1)") shouldBe expected(("_tmp_0", 3, AlwaysEdge))
      succOf("_tmp_0", 3) shouldBe expected(("push", 1, AlwaysEdge))
      succOf("push", 1) shouldBe expected(("_tmp_0.push", 1, AlwaysEdge))
      succOf("_tmp_0.push", 1) shouldBe expected(("_tmp_0", 4, AlwaysEdge))
      succOf("_tmp_0", 4) shouldBe expected(("2", AlwaysEdge))
      succOf("2") shouldBe expected(("_tmp_0.push(2)", AlwaysEdge))

      succOf("_tmp_0.push(2)") shouldBe expected(("_tmp_0", 5, AlwaysEdge))
      succOf("_tmp_0", 5) shouldBe expected(("x = [1, 2]", AlwaysEdge))
      succOf("x = [1, 2]") shouldBe expected(("RET", AlwaysEdge))
    }

    "have correct structure for untagged runtime node in call" in CfgFixture(s"foo(`Hello $${world}!`)") {
      implicit cpg =>
        succOf(":program") shouldBe expected(("foo", AlwaysEdge))
        succOf("foo") shouldBe expected(("this", AlwaysEdge))
        succOf("this", NodeTypes.IDENTIFIER) shouldBe expected(("\"Hello \"", AlwaysEdge))
        succOf("\"Hello \"") shouldBe expected(("world", AlwaysEdge))
        succOf("world") shouldBe expected(("\"!\"", AlwaysEdge))
        succOf("\"!\"") shouldBe expected(("__Runtime.TO_STRING(\"Hello \", world, \"!\")", AlwaysEdge))
        succOf("__Runtime.TO_STRING(\"Hello \", world, \"!\")") shouldBe expected(
          (s"foo(`Hello $${world}!`)", AlwaysEdge)
        )
        succOf(s"foo(`Hello $${world}!`)") shouldBe expected(("RET", AlwaysEdge))
    }

    "have correct structure for untagged runtime node" in CfgFixture(s"`$${x + 1}`") { implicit cpg =>
      succOf(":program") shouldBe expected(("\"\"", AlwaysEdge))
      succOf("\"\"") shouldBe expected(("x", AlwaysEdge))
      succOf("x") shouldBe expected(("1", AlwaysEdge))
      succOf("1") shouldBe expected(("x + 1", AlwaysEdge))
      succOf("x + 1") shouldBe expected(("\"\"", 1, AlwaysEdge))
      succOf("\"\"", 1) shouldBe expected(("__Runtime.TO_STRING(\"\", x + 1, \"\")", AlwaysEdge))
      succOf("__Runtime.TO_STRING(\"\", x + 1, \"\")") shouldBe expected(("RET", AlwaysEdge))
    }

    "have correct structure for tagged runtime node" in CfgFixture(s"String.raw`../$${42}\\..`") { implicit cpg =>
      succOf(":program") shouldBe expected(("\"../\"", AlwaysEdge))
      succOf("\"../\"") shouldBe expected(("42", AlwaysEdge))
      succOf("42") shouldBe expected(("\"\\..\"", AlwaysEdge))
      succOf("\"\\..\"") shouldBe expected(("__Runtime.TO_STRING(\"../\", 42, \"\\..\")", AlwaysEdge))
      succOf("__Runtime.TO_STRING(\"../\", 42, \"\\..\")") shouldBe expected(
        ("String.raw(__Runtime.TO_STRING(\"../\", 42, \"\\..\"))", AlwaysEdge)
      )
      succOf("String.raw(__Runtime.TO_STRING(\"../\", 42, \"\\..\"))") shouldBe expected(("RET", AlwaysEdge))
    }

    "be correct for try" in CfgFixture("""
       |try {
       | open()
       |} catch(err) {
       | handle()
       |} finally {
       | close()
       |}
       |""".stripMargin) { implicit cpg =>
      succOf(":program") shouldBe expected(("open", AlwaysEdge))
      succOf("open") shouldBe expected(("this", AlwaysEdge))
      succOf("this", NodeTypes.IDENTIFIER) shouldBe expected(("open()", AlwaysEdge))
      succOf("open()") shouldBe expected(("handle", AlwaysEdge), ("close", AlwaysEdge))
      succOf("handle()") shouldBe expected(("close", AlwaysEdge))
      succOf("close()") shouldBe expected(("RET", AlwaysEdge))
    }

    "be correct for try with multiple CFG exit nodes in try block" in CfgFixture("""
       |try {
       | if (true) {
       |   doA()
       | } else {
       |   doB()
       | }
       |} catch(err) {
       | handle()
       |} finally {
       | close()
       |}
       |""".stripMargin) { implicit cpg =>
      succOf(":program") shouldBe expected(("true", AlwaysEdge))
      succOf("true") shouldBe expected(("doA", TrueEdge), ("doB", FalseEdge))
      succOf("doA()") shouldBe expected(("handle", AlwaysEdge), ("close", AlwaysEdge))
      succOf("doB()") shouldBe expected(("handle", AlwaysEdge), ("close", AlwaysEdge))
      succOf("handle()") shouldBe expected(("close", AlwaysEdge))
      succOf("close()") shouldBe expected(("RET", AlwaysEdge))
    }

    "be correct for 1 object with simple values" in CfgFixture("""
       |var x = {
       | key1: "value",
       | key2: 2
       |}
       |""".stripMargin) { implicit cpg =>
      succOf(":program") shouldBe expected(("x", AlwaysEdge))
      succOf("x") shouldBe expected(("_tmp_0", AlwaysEdge))
      succOf("_tmp_0") shouldBe expected(("key1", AlwaysEdge))
      succOf("key1") shouldBe expected(("_tmp_0.key1", AlwaysEdge))
      succOf("_tmp_0.key1") shouldBe expected(("\"value\"", AlwaysEdge))
      succOf("\"value\"") shouldBe expected(("_tmp_0.key1 = \"value\"", AlwaysEdge))

      succOf("_tmp_0.key1 = \"value\"") shouldBe expected(("_tmp_0", 1, AlwaysEdge))
      succOf("_tmp_0", 1) shouldBe expected(("key2", AlwaysEdge))
      succOf("key2") shouldBe expected(("_tmp_0.key2", AlwaysEdge))
      succOf("_tmp_0.key2") shouldBe expected(("2", AlwaysEdge))
      succOf("2") shouldBe expected(("_tmp_0.key2 = 2", AlwaysEdge))

      succOf("_tmp_0.key2 = 2") shouldBe expected(("_tmp_0", 2, AlwaysEdge))
      succOf("_tmp_0", 2) shouldBe expected(("x = {\n key1: \"value\",\n key2: 2\n}", AlwaysEdge))
      succOf("x = {\n key1: \"value\",\n key2: 2\n}") shouldBe expected(("RET", AlwaysEdge))
    }

    "be correct for member access used in an assignment (chained)" in CfgFixture("a.b = c.z;") { implicit cpg =>
      succOf(":program") shouldBe expected(("a", AlwaysEdge))
      succOf("a") shouldBe expected(("b", AlwaysEdge))
      succOf("b") shouldBe expected(("a.b", AlwaysEdge))
      succOf("a.b") shouldBe expected(("c", AlwaysEdge))
      succOf("c") shouldBe expected(("z", AlwaysEdge))
      succOf("z") shouldBe expected(("c.z", AlwaysEdge))
      succOf("c.z") shouldBe expected(("a.b = c.z", AlwaysEdge))
      succOf("a.b = c.z") shouldBe expected(("RET", AlwaysEdge))
    }

    "be correct for decl statement with assignment" in CfgFixture("var x = 1;") { implicit cpg =>
      succOf(":program") shouldBe expected(("x", AlwaysEdge))
      succOf("x") shouldBe expected(("1", AlwaysEdge))
      succOf("1") shouldBe expected(("x = 1", AlwaysEdge))
      succOf("x = 1") shouldBe expected(("RET", AlwaysEdge))
    }

    "be correct for nested expression" in CfgFixture("x = y + 1;") { implicit cpg =>
      succOf(":program") shouldBe expected(("x", AlwaysEdge))
      succOf("x") shouldBe expected(("y", AlwaysEdge))
      succOf("y") shouldBe expected(("1", AlwaysEdge))
      succOf("1") shouldBe expected(("y + 1", AlwaysEdge))
      succOf("y + 1") shouldBe expected(("x = y + 1", AlwaysEdge))
      succOf("x = y + 1") shouldBe expected(("RET", AlwaysEdge))
    }

    "be correct for return statement" in CfgFixture("function foo(x) { return x; }") { implicit cpg =>
      succOf("foo", NodeTypes.METHOD) shouldBe expected(("x", AlwaysEdge))
      succOf("x", NodeTypes.IDENTIFIER) shouldBe expected(("return x", AlwaysEdge))
      succOf("return x") shouldBe expected(("RET", AlwaysEdge))
    }

    "be correct for consecutive return statements" in CfgFixture("function foo(x, y) { return x; return y; }") {
      implicit cpg =>
        succOf("foo", NodeTypes.METHOD) shouldBe expected(("x", AlwaysEdge))
        succOf("x", NodeTypes.IDENTIFIER) shouldBe expected(("return x", AlwaysEdge))
        succOf("y", NodeTypes.IDENTIFIER) shouldBe expected(("return y", AlwaysEdge))
        succOf("return x") shouldBe expected(("RET", AlwaysEdge))
        succOf("return y") shouldBe expected(("RET", AlwaysEdge))
    }

    "be correct for outer program function which declares foo function object" in CfgFixture(
      "function foo(x, y) { return; }"
    ) { implicit cpg =>
      succOf(":program") shouldBe expected(("foo", 1, AlwaysEdge))
      succOf("foo", NodeTypes.IDENTIFIER) shouldBe expected(("foo", 2, AlwaysEdge))
      succOf("foo", NodeTypes.METHOD_REF) shouldBe expected(
        ("function foo = function foo(x, y) { return; }", AlwaysEdge)
      )
      succOf("function foo = function foo(x, y) { return; }") shouldBe expected(("RET", AlwaysEdge))
    }

    "be correct for void return statement" in CfgFixture("function foo() { return; }") { implicit cpg =>
      succOf("foo", NodeTypes.METHOD) shouldBe expected(("return", AlwaysEdge))
      succOf("return") shouldBe expected(("RET", AlwaysEdge))
    }

    "be correct for call expression" in CfgFixture("foo(a + 1, b);") { implicit cpg =>
      succOf(":program") shouldBe expected(("foo", AlwaysEdge))
      succOf("foo") shouldBe expected(("this", AlwaysEdge))
      succOf("this", NodeTypes.IDENTIFIER) shouldBe expected(("a", AlwaysEdge))
      succOf("a") shouldBe expected(("1", AlwaysEdge))
      succOf("1") shouldBe expected(("a + 1", AlwaysEdge))
      succOf("a + 1") shouldBe expected(("b", AlwaysEdge))
      succOf("b") shouldBe expected(("foo(a + 1, b)", AlwaysEdge))
      succOf("foo(a + 1, b)") shouldBe expected(("RET", AlwaysEdge))
    }

    "be correct for chained calls" ignore CfgFixture("x.foo(y).bar(z)") { _ =>
      // TODO the current style of writing this tests in unmaintainable.
    }

    "be correct for unary expression '++'" in CfgFixture("x++") { implicit cpg =>
      succOf(":program") shouldBe expected(("x", AlwaysEdge))
      succOf("x") shouldBe expected(("x++", AlwaysEdge))
      succOf("x++") shouldBe expected(("RET", AlwaysEdge))
    }

    "be correct for conditional expression" in CfgFixture("x ? y : z;") { implicit cpg =>
      succOf(":program") shouldBe expected(("x", AlwaysEdge))
      succOf("x") shouldBe expected(("y", TrueEdge), ("z", FalseEdge))
      succOf("y") shouldBe expected(("x ? y : z", AlwaysEdge))
      succOf("z") shouldBe expected(("x ? y : z", AlwaysEdge))
      succOf("x ? y : z") shouldBe expected(("RET", AlwaysEdge))
    }

    "be correct for labeled expressions with continue" in CfgFixture("""
       |var i, j;
       |loop1: for (i = 0; i < 3; i++) {
       |   loop2: for (j = 0; j < 3; j++) {
       |      if (i === 1 && j === 1) {
       |         continue loop1;
       |      }
       |      console.log("");
       |   }
       |}
       |""".stripMargin) { implicit cpg =>
      succOf(":program") shouldBe expected(("loop1:", AlwaysEdge))
      succOf("loop1:") shouldBe expected(("i", AlwaysEdge))
      succOf("i") shouldBe expected(("0", AlwaysEdge))
      succOf("0") shouldBe expected(("i = 0", AlwaysEdge))
      succOf("i = 0") shouldBe expected(("i", 1, AlwaysEdge))
      succOf("i", 1) shouldBe expected(("3", AlwaysEdge))
      succOf("3") shouldBe expected(("i < 3", AlwaysEdge))
      succOf("i < 3") shouldBe expected(("loop2:", AlwaysEdge), ("RET", AlwaysEdge))
      succOf("loop2:") shouldBe expected(("j", AlwaysEdge))
      succOf("j") shouldBe expected(("0", 1, AlwaysEdge))
      succOf("0", 1) shouldBe expected(("j = 0", AlwaysEdge))
      succOf("j = 0") shouldBe expected(("j", 1, AlwaysEdge))
      succOf("j", 1) shouldBe expected(("3", 1, AlwaysEdge))
      succOf("3", 1) shouldBe expected(("j < 3", AlwaysEdge))
      succOf("j < 3") shouldBe expected(("i", 2, AlwaysEdge))
      succOf("i", 2) shouldBe expected(("i++", AlwaysEdge))
      succOf("i++") shouldBe expected(("i", 3, AlwaysEdge))
      succOf("i", 3) shouldBe expected(("1", AlwaysEdge))
      succOf("1") shouldBe expected(("i === 1", AlwaysEdge))
      succOf("i === 1") shouldBe expected(("j", AlwaysEdge), ("i === 1 && j === 1", AlwaysEdge))
      succOf("i === 1 && j === 1") shouldBe expected(("continue loop1;", AlwaysEdge), ("console", AlwaysEdge))
      succOf("continue loop1;") shouldBe expected(("loop1:", AlwaysEdge))
      succOf("console") shouldBe expected(("log", AlwaysEdge))
      succOf("log") shouldBe expected(("console.log", AlwaysEdge))
    }

    // WHILE Loops
    "be correct for plain while loop" in CfgFixture("while (x < 1) { y = 2; }") { implicit cpg =>
      succOf(":program") shouldBe expected(("x", AlwaysEdge))
      succOf("x") shouldBe expected(("1", AlwaysEdge))
      succOf("1") shouldBe expected(("x < 1", AlwaysEdge))
      succOf("x < 1") shouldBe expected(("y", TrueEdge), ("RET", FalseEdge))
      succOf("y") shouldBe expected(("2", AlwaysEdge))
      succOf("2") shouldBe expected(("y = 2", AlwaysEdge))
      succOf("y = 2") shouldBe expected(("x", AlwaysEdge))
      succOf("x") shouldBe expected(("1", AlwaysEdge))
    }

    "be correct for plain while loop with break" in CfgFixture("while (x < 1) { break; y; }") { implicit cpg =>
      succOf(":program") shouldBe expected(("x", AlwaysEdge))
      succOf("x") shouldBe expected(("1", AlwaysEdge))
      succOf("1") shouldBe expected(("x < 1", AlwaysEdge))
      succOf("x < 1") shouldBe expected(("break;", TrueEdge), ("RET", FalseEdge))
      succOf("break;") shouldBe expected(("RET", AlwaysEdge))
      succOf("y") shouldBe expected(("x", AlwaysEdge))
    }

    "be correct for plain while loop with continue" in CfgFixture("while (x < 1) { continue; y; }") { implicit cpg =>
      succOf(":program") shouldBe expected(("x", AlwaysEdge))
      succOf("x") shouldBe expected(("1", AlwaysEdge))
      succOf("1") shouldBe expected(("x < 1", AlwaysEdge))
      succOf("x < 1") shouldBe expected(("continue;", TrueEdge), ("RET", FalseEdge))
      succOf("continue;") shouldBe expected(("x", AlwaysEdge))
      succOf("y") shouldBe expected(("x", AlwaysEdge))
    }

    "be correct for nested while loop" in CfgFixture("while (x) {while(y) {z;}}") { implicit cpg =>
      succOf(":program") shouldBe expected(("x", AlwaysEdge))
      succOf("x") shouldBe expected(("y", TrueEdge), ("RET", FalseEdge))
      succOf("y") shouldBe expected(("z", TrueEdge), ("x", FalseEdge))
    }

    "be correct for nested while loop with break" in CfgFixture("while (x) { while(y) { break; z;} a;} b;") {
      implicit cpg =>
        succOf(":program") shouldBe expected(("x", AlwaysEdge))
        succOf("x") shouldBe expected(("y", TrueEdge), ("b", FalseEdge))
        succOf("y") shouldBe expected(("break;", TrueEdge), ("a", FalseEdge))
        succOf("a") shouldBe expected(("x", AlwaysEdge))
        succOf("b") shouldBe expected(("RET", AlwaysEdge))
    }

    "be correct for another nested while loop with break" in CfgFixture(
      "while (x) { while(y) { break; z;} a; break; b; } c;"
    ) { implicit cpg =>
      succOf(":program") shouldBe expected(("x", AlwaysEdge))
      succOf("x") shouldBe expected(("y", TrueEdge), ("c", FalseEdge))
      succOf("y") shouldBe expected(("break;", TrueEdge), ("a", FalseEdge))
      succOf("break;") shouldBe expected(("a", AlwaysEdge))
      succOf("a") shouldBe expected(("break;", 1, AlwaysEdge))
      succOf("break;", 1) shouldBe expected(("c", AlwaysEdge))
      succOf("c") shouldBe expected(("RET", AlwaysEdge))
    }

    "nested while loop with conditional break" in CfgFixture(s"""
        |while (x) {
        |  if (y) {
        |	   break;
        |	 }
        |	 while (z) {
        |    break;
        |  }
        |}
      """.stripMargin) { implicit cpg =>
      succOf(":program") shouldBe expected(("x", AlwaysEdge))
      succOf("x") shouldBe expected(("y", TrueEdge), ("RET", FalseEdge))
      succOf("y") shouldBe expected(("break;", TrueEdge), ("z", FalseEdge))
      succOf("break;") shouldBe expected(("RET", AlwaysEdge))
      succOf("break;", 1) shouldBe expected(("x", AlwaysEdge))
      succOf("z") shouldBe expected(("break;", 1, TrueEdge), ("x", FalseEdge))
    }

    // DO-WHILE Loops
    "be correct for plain do-while loop" in CfgFixture("do { y = 2; } while (x < 1);") { implicit cpg =>
      succOf(":program") shouldBe expected(("y", AlwaysEdge))
      succOf("y") shouldBe expected(("2", AlwaysEdge))
      succOf("2") shouldBe expected(("y = 2", AlwaysEdge))
      succOf("y = 2") shouldBe expected(("x", AlwaysEdge))
      succOf("x") shouldBe expected(("1", AlwaysEdge))
      succOf("1") shouldBe expected(("x < 1", AlwaysEdge))
      succOf("x < 1") shouldBe expected(("y", TrueEdge), ("RET", FalseEdge))
    }

    "be correct for plain do-while loop with break" in CfgFixture("do { break; y; } while (x < 1);") { implicit cpg =>
      succOf(":program") shouldBe expected(("break;", AlwaysEdge))
      succOf("break;") shouldBe expected(("RET", AlwaysEdge))
      succOf("y") shouldBe expected(("x", AlwaysEdge))
      succOf("x") shouldBe expected(("1", AlwaysEdge))
      succOf("1") shouldBe expected(("x < 1", AlwaysEdge))
      succOf("x < 1") shouldBe expected(("break;", TrueEdge), ("RET", FalseEdge))
    }

    "be correct for plain do-while loop with continue" in CfgFixture("do { continue; y; } while (x < 1);") {
      implicit cpg =>
        succOf(":program") shouldBe expected(("continue;", AlwaysEdge))
        succOf("continue;") shouldBe expected(("x", AlwaysEdge))
        succOf("y") shouldBe expected(("x", AlwaysEdge))
        succOf("x") shouldBe expected(("1", AlwaysEdge))
        succOf("1") shouldBe expected(("x < 1", AlwaysEdge))
        succOf("x < 1") shouldBe expected(("continue;", TrueEdge), ("RET", FalseEdge))
    }

    "be correct for nested do-while loop with continue" in CfgFixture("do { do { x; } while (y); } while (z);") {
      implicit cpg =>
        succOf(":program") shouldBe expected(("x", AlwaysEdge))
        succOf("x") shouldBe expected(("y", AlwaysEdge))
        succOf("y") shouldBe expected(("x", TrueEdge), ("z", FalseEdge))
        succOf("z") shouldBe expected(("x", TrueEdge), ("RET", FalseEdge))
    }

    "be correct for nested while/do-while loops with break" in CfgFixture(
      "while (x) { do { while(y) { break; a; } z; } while (x < 1); } c;"
    ) { implicit cpg =>
      succOf(":program") shouldBe expected(("x", AlwaysEdge))
      succOf("x") shouldBe expected(("y", TrueEdge), ("c", FalseEdge))
      succOf("y") shouldBe expected(("break;", TrueEdge), ("z", FalseEdge))
      succOf("break;") shouldBe expected(("z", AlwaysEdge))
      succOf("z") shouldBe expected(("x", 1, AlwaysEdge))
      succOf("x", 1) shouldBe expected(("1", AlwaysEdge))
      succOf("1") shouldBe expected(("x < 1", AlwaysEdge))
      succOf("x < 1") shouldBe expected(("y", TrueEdge), ("x", FalseEdge))
      succOf("c") shouldBe expected(("RET", AlwaysEdge))
    }

    "be correct for nested while/do-while loops with break and continue" in CfgFixture(
      "while(x) { do { break; } while (y) } o;"
    ) { implicit cpg =>
      succOf(":program") shouldBe expected(("x", AlwaysEdge))
      succOf("x") shouldBe expected(("break;", TrueEdge), ("o", FalseEdge))
      succOf("break;") shouldBe expected(("x", AlwaysEdge))
      succOf("o") shouldBe expected(("RET", AlwaysEdge))
    }

    "be correct for two nested while loop with inner break" in CfgFixture("while(y) { while(z) { break; x; } }") {
      implicit cpg =>
        succOf(":program") shouldBe expected(("y", AlwaysEdge))
        succOf("y") shouldBe expected(("z", TrueEdge), ("RET", FalseEdge))
        succOf("z") shouldBe expected(("break;", TrueEdge), ("y", FalseEdge))
        succOf("break;") shouldBe expected(("y", AlwaysEdge))
    }

    // FOR Loops
    "be correct for plain for-loop" in CfgFixture("for (x = 0; y < 1; z += 2) { a = 3; }") { implicit cpg =>
      succOf(":program") shouldBe expected(("x", AlwaysEdge))
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

    "be correct for plain for-loop with break" in CfgFixture("for (x = 0; y < 1; z += 2) { break; a = 3; }") {
      implicit cpg =>
        succOf(":program") shouldBe expected(("x", AlwaysEdge))
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

    "be correct for plain for-loop with continue" in CfgFixture("for (x = 0; y < 1; z += 2) { continue; a = 3; }") {
      implicit cpg =>
        succOf(":program") shouldBe expected(("x", AlwaysEdge))
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

    "be correct for for-loop with for-in" in CfgFixture("""
        |for (var i in arr) {
        |   foo(i)
        |}
        |""".stripMargin) { implicit cpg => testForInOrOf() }

    "be correct for for-loop with for-of" in CfgFixture("""
        |for (var i of arr) {
        |   foo(i)
        |}
        |""".stripMargin) { implicit cpg => testForInOrOf() }

    "be correct for nested for-loop" in CfgFixture("for (x; y; z) { for (a; b; c) { u; } }") { implicit cpg =>
      succOf(":program") shouldBe expected(("x", AlwaysEdge))
      succOf("x") shouldBe expected(("y", AlwaysEdge))
      succOf("y") shouldBe expected(("a", TrueEdge), ("RET", FalseEdge))
      succOf("z") shouldBe expected(("y", AlwaysEdge))
      succOf("a") shouldBe expected(("b", AlwaysEdge))
      succOf("b") shouldBe expected(("u", TrueEdge), ("z", FalseEdge))
      succOf("c") shouldBe expected(("b", AlwaysEdge))
      succOf("u") shouldBe expected(("c", AlwaysEdge))
    }

    "be correct for for-loop with empty condition" in CfgFixture("for (;;) { a = 1; }") { implicit cpg =>
      succOf(":program") shouldBe expected(("true", AlwaysEdge))
      succOf("true") shouldBe expected(("a", TrueEdge), ("RET", FalseEdge))
      succOf("a") shouldBe expected(("1", AlwaysEdge))
      succOf("1") shouldBe expected(("a = 1", AlwaysEdge))
      succOf("a = 1") shouldBe expected(("true", AlwaysEdge))
    }

    "be correct for for-loop with empty condition and break" in CfgFixture("for (;;) { break; }") { implicit cpg =>
      succOf(":program") shouldBe expected(("true", AlwaysEdge))
      succOf("true") shouldBe expected(("break;", TrueEdge), ("RET", FalseEdge))
      succOf("break;") shouldBe expected(("RET", AlwaysEdge))
    }

    "be correct for for-loop with empty condition and continue" in CfgFixture("for (;;) { continue; }") {
      implicit cpg =>
        succOf(":program") shouldBe expected(("true", AlwaysEdge))
        succOf("true") shouldBe expected(("continue;", TrueEdge), ("RET", FalseEdge))
        succOf("continue;") shouldBe expected(("true", AlwaysEdge))
    }

    "be correct with empty condition with nested empty for-loop" in CfgFixture("for (;;) { for (;;) { x; } }") {
      implicit cpg =>
        succOf(":program") shouldBe expected(("true", AlwaysEdge))
        succOf("true") shouldBe expected(("true", 1, TrueEdge), ("RET", FalseEdge))
        succOf("true", 1) shouldBe expected(("x", TrueEdge), ("true", 0, FalseEdge))
        succOf("x") shouldBe expected(("true", 1, AlwaysEdge))
    }

    "be correct for for-loop with empty block" in CfgFixture("for (;;) ;") { implicit cpg =>
      succOf(":program") shouldBe expected(("true", AlwaysEdge))
      succOf("true") shouldBe expected(("true", TrueEdge), ("RET", FalseEdge))
    }

    // IF Statement
    "be correct for simple if statement" in CfgFixture("if (x) { y; }") { implicit cpg =>
      succOf(":program") shouldBe expected(("x", AlwaysEdge))
      succOf("x") shouldBe expected(("y", TrueEdge), ("RET", FalseEdge))
      succOf("y") shouldBe expected(("RET", AlwaysEdge))
    }

    "be correct for simple if statement with else block" in CfgFixture("if (x) { y; } else { z; }") { implicit cpg =>
      succOf(":program") shouldBe expected(("x", AlwaysEdge))
      succOf("x") shouldBe expected(("y", TrueEdge), ("z", FalseEdge))
      succOf("y") shouldBe expected(("RET", AlwaysEdge))
      succOf("z") shouldBe expected(("RET", AlwaysEdge))
    }

    "be correct for nested if statement" in CfgFixture("if (x) { if (y) { z; } }") { implicit cpg =>
      succOf(":program") shouldBe expected(("x", AlwaysEdge))
      succOf("x") shouldBe expected(("y", TrueEdge), ("RET", FalseEdge))
      succOf("y") shouldBe expected(("z", TrueEdge), ("RET", FalseEdge))
      succOf("z") shouldBe expected(("RET", AlwaysEdge))
    }

    "be correct for nested if statement with else-if chains" in CfgFixture(
      "if (a) { b; } else if (c) { d;} else { e; }"
    ) { implicit cpg =>
      succOf(":program") shouldBe expected(("a", AlwaysEdge))
      succOf("a") shouldBe expected(("b", TrueEdge), ("c", FalseEdge))
      succOf("b") shouldBe expected(("RET", AlwaysEdge))
      succOf("c") shouldBe expected(("d", TrueEdge), ("e", FalseEdge))
      succOf("d") shouldBe expected(("RET", AlwaysEdge))
      succOf("e") shouldBe expected(("RET", AlwaysEdge))
    }

    "be correct for switch-case with single case" in CfgFixture("switch (x) { case 1: y;}") { implicit cpg =>
      succOf(":program") shouldBe expected(("x", AlwaysEdge))
      succOf("x") shouldBe expected(("case 1:", CaseEdge), ("RET", CaseEdge))
      succOf("case 1:") shouldBe expected(("1", AlwaysEdge))
      succOf("1") shouldBe expected(("y", AlwaysEdge))
      succOf("y") shouldBe expected(("RET", AlwaysEdge))
    }

    "be correct for switch-case with multiple cases" in CfgFixture("switch (x) { case 1: y; case 2: z;}") {
      implicit cpg =>
        succOf(":program") shouldBe expected(("x", AlwaysEdge))
        succOf("x") shouldBe expected(("case 1:", CaseEdge), ("case 2:", CaseEdge), ("RET", CaseEdge))
        succOf("case 1:") shouldBe expected(("1", AlwaysEdge))
        succOf("1") shouldBe expected(("y", AlwaysEdge))
        succOf("y") shouldBe expected(("case 2:", AlwaysEdge))
        succOf("case 2:") shouldBe expected(("2", AlwaysEdge))
        succOf("2") shouldBe expected(("z", AlwaysEdge))
        succOf("z") shouldBe expected(("RET", AlwaysEdge))
    }

    "be correct for switch-case with multiple cases on the same spot" in CfgFixture(
      "switch (x) { case 1: case 2: y; }"
    ) { implicit cpg =>
      succOf(":program") shouldBe expected(("x", AlwaysEdge))
      succOf("x") shouldBe expected(("case 1:", CaseEdge), ("case 2:", CaseEdge), ("RET", CaseEdge))
      succOf("case 1:") shouldBe expected(("1", AlwaysEdge))
      succOf("1") shouldBe expected(("case 2:", AlwaysEdge))
      succOf("case 2:") shouldBe expected(("2", AlwaysEdge))
      succOf("2") shouldBe expected(("y", AlwaysEdge))
      succOf("y") shouldBe expected(("RET", AlwaysEdge))
    }

    "be correct for switch-case with default case" in CfgFixture("switch (x) { default: y; }") { implicit cpg =>
      succOf(":program") shouldBe expected(("x", AlwaysEdge))
      succOf("x") shouldBe expected(("default:", CaseEdge))
      succOf("default:") shouldBe expected(("y", AlwaysEdge))
      succOf("y") shouldBe expected(("RET", AlwaysEdge))
    }

    "be correct for switch-case with multiple cases and default combined" in CfgFixture(
      "switch (x) { case 1: y; break; default: z;}"
    ) { implicit cpg =>
      succOf(":program") shouldBe expected(("x", AlwaysEdge))
      succOf("x") shouldBe expected(("case 1:", CaseEdge), ("default:", CaseEdge))
      succOf("case 1:") shouldBe expected(("1", AlwaysEdge))
      succOf("1") shouldBe expected(("y", AlwaysEdge))
      succOf("y") shouldBe expected(("break;", AlwaysEdge))
      succOf("break;") shouldBe expected(("RET", AlwaysEdge))
      succOf("default:") shouldBe expected(("z", AlwaysEdge))
      succOf("z") shouldBe expected(("RET", AlwaysEdge))
    }

    "be correct for constructor call with new" in CfgFixture("""var x = new MyClass(arg1, arg2)""") { implicit cpg =>
      succOf(":program") shouldBe expected(("x", AlwaysEdge))
      succOf("x") shouldBe expected(("_tmp_0", AlwaysEdge))
      succOf("_tmp_0") shouldBe expected((".alloc", AlwaysEdge))
      succOf(".alloc") shouldBe expected(("_tmp_0 = .alloc", AlwaysEdge))
      succOf("_tmp_0 = .alloc") shouldBe expected(("MyClass", AlwaysEdge))
      succOf("MyClass") shouldBe expected(("_tmp_0", 1, AlwaysEdge))
      succOf("_tmp_0", 1) shouldBe expected(("arg1", AlwaysEdge))
      succOf("arg1") shouldBe expected(("arg2", AlwaysEdge))
      succOf("arg2") shouldBe expected(("new MyClass(arg1, arg2)", AlwaysEdge))
      succOf("new MyClass(arg1, arg2)", NodeTypes.CALL) shouldBe expected(("_tmp_0", 2, AlwaysEdge))
      succOf("_tmp_0", 2) shouldBe expected(("x = new MyClass(arg1, arg2)", AlwaysEdge))
      succOf("x = new MyClass(arg1, arg2)") shouldBe expected(("RET", AlwaysEdge))
    }
  }

  private def testForInOrOf()(implicit cpg: Cpg): Unit = {
    succOf(":program") shouldBe expected(("_iterator_0", AlwaysEdge))
    succOf("_iterator_0") shouldBe expected(("arr", AlwaysEdge))
    succOf("arr") shouldBe expected(("Object.keys(arr)", AlwaysEdge))
    succOf("Object.keys(arr)") shouldBe expected(("Symbol", AlwaysEdge))
    succOf("Symbol") shouldBe expected(("iterator", AlwaysEdge))
    succOf("iterator") shouldBe expected(("Symbol.iterator", AlwaysEdge))
    succOf("Symbol.iterator") shouldBe expected(("Object.keys(arr)[Symbol.iterator]", AlwaysEdge))
    succOf("Object.keys(arr)[Symbol.iterator]") shouldBe expected(("this", AlwaysEdge))
    succOf("this", NodeTypes.IDENTIFIER) shouldBe expected(("Object.keys(arr)[Symbol.iterator]()", AlwaysEdge))
    succOf("Object.keys(arr)[Symbol.iterator]()") shouldBe expected(
      ("_iterator_0 = Object.keys(arr)[Symbol.iterator]()", AlwaysEdge)
    )

    succOf("_iterator_0 = Object.keys(arr)[Symbol.iterator]()") shouldBe expected(("_result_0", AlwaysEdge))

    succOf("_result_0") shouldBe expected(("i", AlwaysEdge))

    succOf("i") shouldBe expected(("_result_0", 1, AlwaysEdge))
    succOf("_result_0", 1) shouldBe expected(("_iterator_0", 1, AlwaysEdge))
    succOf("_iterator_0", 1) shouldBe expected(("next", AlwaysEdge))
    succOf("next") shouldBe expected(("_iterator_0.next", AlwaysEdge))
    succOf("_iterator_0.next") shouldBe expected(("_iterator_0", 2, AlwaysEdge))
    succOf("_iterator_0", 2) shouldBe expected(("_iterator_0.next()", AlwaysEdge))
    succOf("_iterator_0.next()") shouldBe expected(("(_result_0 = _iterator_0.next())", AlwaysEdge))
    succOf("(_result_0 = _iterator_0.next())") shouldBe expected(("done", AlwaysEdge))
    succOf("done") shouldBe expected(("(_result_0 = _iterator_0.next()).done", AlwaysEdge))
    succOf("(_result_0 = _iterator_0.next()).done") shouldBe expected(
      ("!(_result_0 = _iterator_0.next()).done", AlwaysEdge)
    )

    succOf("!(_result_0 = _iterator_0.next()).done") shouldBe expected(("i", 1, TrueEdge), ("RET", FalseEdge))

    succOf("i", 1) shouldBe expected(("_result_0", 2, AlwaysEdge))
    succOf("_result_0", 2) shouldBe expected(("value", AlwaysEdge))
    succOf("value") shouldBe expected(("_result_0.value", AlwaysEdge))
    succOf("_result_0.value") shouldBe expected(("i = _result_0.value", AlwaysEdge))
    succOf("i = _result_0.value") shouldBe expected(("foo", AlwaysEdge))
    succOf("foo") shouldBe expected(("this", 1, AlwaysEdge))
    succOf("this", 2) shouldBe expected(("i", 2, AlwaysEdge))
    succOf("i", 2) shouldBe expected(("foo(i)", AlwaysEdge))
    succOf("foo(i)") shouldBe expected(("_result_0", 1, AlwaysEdge))
  }

}
