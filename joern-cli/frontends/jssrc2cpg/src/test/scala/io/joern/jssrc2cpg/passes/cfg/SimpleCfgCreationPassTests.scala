package io.joern.jssrc2cpg.passes.cfg

import io.joern.jssrc2cpg.testfixtures.JsSrcCfgTestCpg
import io.joern.x2cpg.passes.controlflow.cfgcreation.Cfg.{AlwaysEdge, CaseEdge, FalseEdge, TrueEdge}
import io.joern.x2cpg.testfixtures.CfgTestFixture
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.{NodeTypes, Operators}

class SimpleCfgCreationPassTests extends CfgTestFixture(() => new JsSrcCfgTestCpg()) {

  "CFG generation for simple fragments" should {
    "have correct structure for block expression" in {
      implicit val cpg: Cpg = code("let x = (class Foo {}, bar())")
      succOf(":program") shouldBe expected(("x", AlwaysEdge))
      succOf("x") shouldBe expected(("class Foo", AlwaysEdge))
      succOf("class Foo") shouldBe expected(("bar", AlwaysEdge))
      succOf("bar") shouldBe expected(("this", AlwaysEdge))
      succOf("this", NodeTypes.IDENTIFIER) shouldBe expected(("bar()", AlwaysEdge))
      succOf("bar()") shouldBe expected(("class Foo {}, bar()", AlwaysEdge))
      succOf("class Foo {}, bar()") shouldBe expected(("let x = (class Foo {}, bar())", AlwaysEdge))
      succOf("let x = (class Foo {}, bar())") shouldBe expected(("RET", AlwaysEdge))
    }

    "have correct structure for empty array literal" in {
      implicit val cpg: Cpg = code("var x = []")
      succOf(":program") shouldBe expected(("x", AlwaysEdge))
      succOf("x") shouldBe expected(("__ecma.Array.factory()", AlwaysEdge))
      succOf("__ecma.Array.factory()") shouldBe expected(("var x = []", AlwaysEdge))
    }

    "have correct structure for array literal with values" in {
      implicit val cpg: Cpg = code("var x = [1, 2]")
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
      succOf("_tmp_0", 5) shouldBe expected(("[1, 2]", AlwaysEdge))
      succOf("[1, 2]") shouldBe expected(("var x = [1, 2]", AlwaysEdge))
      succOf("var x = [1, 2]") shouldBe expected(("RET", AlwaysEdge))
    }

    "have correct structure for untagged runtime node in call" in {
      implicit val cpg: Cpg = code(s"foo(`Hello $${world}!`)")
      succOf(":program") shouldBe expected(("foo", AlwaysEdge))
      succOf("foo") shouldBe expected(("this", AlwaysEdge))
      succOf("this", NodeTypes.IDENTIFIER) shouldBe expected(("\"Hello \"", AlwaysEdge))
      succOf("\"Hello \"") shouldBe expected(("world", AlwaysEdge))
      succOf("world") shouldBe expected(("\"!\"", AlwaysEdge))
      succOf("\"!\"") shouldBe expected((s"${Operators.formatString}(\"Hello \", world, \"!\")", AlwaysEdge))
      succOf(s"${Operators.formatString}(\"Hello \", world, \"!\")") shouldBe expected(
        (s"foo(`Hello $${world}!`)", AlwaysEdge)
      )
      succOf(s"foo(`Hello $${world}!`)") shouldBe expected(("RET", AlwaysEdge))
    }

    "have correct structure for untagged runtime node" in {
      implicit val cpg: Cpg = code(s"`$${x + 1}`")
      succOf(":program") shouldBe expected(("\"\"", AlwaysEdge))
      succOf("\"\"") shouldBe expected(("x", AlwaysEdge))
      succOf("x") shouldBe expected(("1", AlwaysEdge))
      succOf("1") shouldBe expected(("x + 1", AlwaysEdge))
      succOf("x + 1") shouldBe expected(("\"\"", 1, AlwaysEdge))
      succOf("\"\"", 1) shouldBe expected((s"${Operators.formatString}(\"\", x + 1, \"\")", AlwaysEdge))
      succOf(s"${Operators.formatString}(\"\", x + 1, \"\")") shouldBe expected(("RET", AlwaysEdge))
    }

    "have correct structure for tagged runtime node" in {
      implicit val cpg: Cpg = code(s"String.raw`../$${42}\\..`")
      succOf(":program") shouldBe expected(("\"../\"", AlwaysEdge))
      succOf("\"../\"") shouldBe expected(("42", AlwaysEdge))
      succOf("42") shouldBe expected(("\"\\..\"", AlwaysEdge))
      succOf("\"\\..\"") shouldBe expected((s"${Operators.formatString}(\"../\", 42, \"\\..\")", AlwaysEdge))
      succOf(s"${Operators.formatString}(\"../\", 42, \"\\..\")") shouldBe expected(
        (s"String.raw(${Operators.formatString}(\"../\", 42, \"\\..\"))", AlwaysEdge)
      )
      succOf(s"String.raw(${Operators.formatString}(\"../\", 42, \"\\..\"))") shouldBe expected(("RET", AlwaysEdge))
    }

    "be correct for try" in {
      implicit val cpg: Cpg = code("""
         |try {
         | open()
         |} catch(err) {
         | handle()
         |} finally {
         | close()
         |}
         |""".stripMargin)
      succOf(":program") shouldBe expected(("open", AlwaysEdge))
      succOf("open") shouldBe expected(("this", AlwaysEdge))
      succOf("this", NodeTypes.IDENTIFIER) shouldBe expected(("open()", AlwaysEdge))
      succOf("open()") shouldBe expected(("err", AlwaysEdge), ("close", AlwaysEdge))
      succOf("err") shouldBe expected(("handle", AlwaysEdge))
      succOf("handle()") shouldBe expected(("close", AlwaysEdge))
      succOf("close()") shouldBe expected(("RET", AlwaysEdge))
    }

    "be correct for try with multiple CFG exit nodes in try block" in {
      implicit val cpg: Cpg = code("""
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
        |""".stripMargin)
      succOf(":program") shouldBe expected(("true", AlwaysEdge))
      succOf("true") shouldBe expected(("doA", TrueEdge), ("doB", FalseEdge))
      succOf("doA()") shouldBe expected(("err", AlwaysEdge), ("close", AlwaysEdge))
      succOf("err") shouldBe expected(("handle", AlwaysEdge))
      succOf("doB()") shouldBe expected(("err", AlwaysEdge), ("close", AlwaysEdge))
      succOf("err") shouldBe expected(("handle", AlwaysEdge))
      succOf("handle()") shouldBe expected(("close", AlwaysEdge))
      succOf("close()") shouldBe expected(("RET", AlwaysEdge))
    }

    "be correct for 1 object with simple values" in {
      implicit val cpg: Cpg = code("""
         |var x = {
         | key1: "value",
         | key2: 2
         |}
         |""".stripMargin)
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
      succOf("_tmp_0", 2) shouldBe expected(("{\n key1: \"value\",\n key2: 2\n}", AlwaysEdge))
      succOf("{\n key1: \"value\",\n key2: 2\n}") shouldBe expected(
        ("var x = {\n key1: \"value\",\n key2: 2\n}", AlwaysEdge)
      )
      succOf("var x = {\n key1: \"value\",\n key2: 2\n}") shouldBe expected(("RET", AlwaysEdge))
    }

    "be correct for member access used in an assignment (chained)" in {
      implicit val cpg: Cpg = code("a.b = c.z;")
      succOf(":program") shouldBe expected(("a", AlwaysEdge))
      succOf("a") shouldBe expected(("b", AlwaysEdge))
      succOf("b") shouldBe expected(("a.b", AlwaysEdge))
      succOf("a.b") shouldBe expected(("c", AlwaysEdge))
      succOf("c") shouldBe expected(("z", AlwaysEdge))
      succOf("z") shouldBe expected(("c.z", AlwaysEdge))
      succOf("c.z") shouldBe expected(("a.b = c.z", AlwaysEdge))
      succOf("a.b = c.z") shouldBe expected(("RET", AlwaysEdge))
    }

    "be correct for decl statement with assignment" in {
      implicit val cpg: Cpg = code("var x = 1;")
      succOf(":program") shouldBe expected(("x", AlwaysEdge))
      succOf("x") shouldBe expected(("1", AlwaysEdge))
      succOf("1") shouldBe expected(("var x = 1", AlwaysEdge))
      succOf("var x = 1") shouldBe expected(("RET", AlwaysEdge))
    }

    "be correct for nested expression" in {
      implicit val cpg: Cpg = code("x = y + 1;")
      succOf(":program") shouldBe expected(("x", AlwaysEdge))
      succOf("x") shouldBe expected(("y", AlwaysEdge))
      succOf("y") shouldBe expected(("1", AlwaysEdge))
      succOf("1") shouldBe expected(("y + 1", AlwaysEdge))
      succOf("y + 1") shouldBe expected(("x = y + 1", AlwaysEdge))
      succOf("x = y + 1") shouldBe expected(("RET", AlwaysEdge))
    }

    "be correct for return statement" in {
      implicit val cpg: Cpg = code("function foo(x) { return x; }")
      succOf("foo", NodeTypes.METHOD) shouldBe expected(("x", AlwaysEdge))
      succOf("x", NodeTypes.IDENTIFIER) shouldBe expected(("return x", AlwaysEdge))
      succOf("return x") shouldBe expected(("RET", AlwaysEdge))
    }

    "be correct for consecutive return statements" in {
      implicit val cpg: Cpg = code("function foo(x, y) { return x; return y; }")
      succOf("foo", NodeTypes.METHOD) shouldBe expected(("x", AlwaysEdge))
      succOf("x", NodeTypes.IDENTIFIER) shouldBe expected(("return x", AlwaysEdge))
      succOf("y", NodeTypes.IDENTIFIER) shouldBe expected(("return y", AlwaysEdge))
      succOf("return x") shouldBe expected(("RET", AlwaysEdge))
      succOf("return y") shouldBe expected(("RET", AlwaysEdge))
    }

    "be correct for outer program function which declares foo function object" in {
      implicit val cpg: Cpg = code("function foo(x, y) { return; }")
      succOf(":program", NodeTypes.METHOD) shouldBe expected(("foo", 2, AlwaysEdge))
      succOf("foo", NodeTypes.IDENTIFIER) shouldBe expected(("foo", 3, AlwaysEdge))
      succOf("foo", NodeTypes.METHOD_REF) shouldBe expected(
        ("function foo = function foo(x, y) { return; }", AlwaysEdge)
      )
      succOf("function foo = function foo(x, y) { return; }") shouldBe expected(("RET", AlwaysEdge))
    }

    "be correct for void return statement" in {
      implicit val cpg: Cpg = code("function foo() { return; }")
      succOf("foo", NodeTypes.METHOD) shouldBe expected(("return", AlwaysEdge))
      succOf("return") shouldBe expected(("RET", AlwaysEdge))
    }

    "be correct for call expression" in {
      implicit val cpg: Cpg = code("foo(a + 1, b);")
      succOf(":program") shouldBe expected(("foo", AlwaysEdge))
      succOf("foo") shouldBe expected(("this", AlwaysEdge))
      succOf("this", NodeTypes.IDENTIFIER) shouldBe expected(("a", AlwaysEdge))
      succOf("a") shouldBe expected(("1", AlwaysEdge))
      succOf("1") shouldBe expected(("a + 1", AlwaysEdge))
      succOf("a + 1") shouldBe expected(("b", AlwaysEdge))
      succOf("b") shouldBe expected(("foo(a + 1, b)", AlwaysEdge))
      succOf("foo(a + 1, b)") shouldBe expected(("RET", AlwaysEdge))
    }

    "be correct for chained calls" in {
      implicit val cpg: Cpg = code("x.foo(y).bar(z)")
      succOf(":program") shouldBe expected(("_tmp_0", AlwaysEdge))
      succOf("_tmp_0") shouldBe expected(("x", AlwaysEdge))
      succOf("x") shouldBe expected(("foo", AlwaysEdge))
      succOf("foo") shouldBe expected(("x.foo", AlwaysEdge))
      succOf("x.foo") shouldBe expected(("x", 1, AlwaysEdge))
      succOf("x", 1) shouldBe expected(("y", AlwaysEdge))
      succOf("y") shouldBe expected(("x.foo(y)", AlwaysEdge))
      succOf("x.foo(y)") shouldBe expected(("(_tmp_0 = x.foo(y))", AlwaysEdge))
      succOf("(_tmp_0 = x.foo(y))") shouldBe expected(("bar", AlwaysEdge))
      succOf("bar") shouldBe expected(("(_tmp_0 = x.foo(y)).bar", AlwaysEdge))
      succOf("(_tmp_0 = x.foo(y)).bar") shouldBe expected(("_tmp_0", 1, AlwaysEdge))
      succOf("_tmp_0", 1) shouldBe expected(("z", AlwaysEdge))
      succOf("z") shouldBe expected(("x.foo(y).bar(z)", AlwaysEdge))
      succOf("x.foo(y).bar(z)") shouldBe expected(("RET", AlwaysEdge))
    }

    "be correct for unary expression '++'" in {
      implicit val cpg: Cpg = code("x++")
      succOf(":program") shouldBe expected(("x", AlwaysEdge))
      succOf("x") shouldBe expected(("x++", AlwaysEdge))
      succOf("x++") shouldBe expected(("RET", AlwaysEdge))
    }

    "be correct for conditional expression" in {
      implicit val cpg: Cpg = code("x ? y : z;")
      succOf(":program") shouldBe expected(("x", AlwaysEdge))
      succOf("x") shouldBe expected(("y", TrueEdge), ("z", FalseEdge))
      succOf("y") shouldBe expected(("x ? y : z", AlwaysEdge))
      succOf("z") shouldBe expected(("x ? y : z", AlwaysEdge))
      succOf("x ? y : z") shouldBe expected(("RET", AlwaysEdge))
    }

    "be correct for labeled expressions with continue" in {
      implicit val cpg: Cpg = code("""
         |var i, j;
         |loop1: for (i = 0; i < 3; i++) {
         |   loop2: for (j = 0; j < 3; j++) {
         |      if (i === 1 && j === 1) {
         |         continue loop1;
         |      }
         |      console.log("");
         |   }
         |}
         |""".stripMargin)
      succOf(":program") shouldBe expected(("var i, j;", AlwaysEdge))
      succOf("loop1:") shouldBe expected(("i", AlwaysEdge))
      succOf("i") shouldBe expected(("0", AlwaysEdge))
      succOf("0") shouldBe expected(("i = 0", AlwaysEdge))
      succOf("i = 0") shouldBe expected(("i", 1, AlwaysEdge))
      succOf("i", 1) shouldBe expected(("3", AlwaysEdge))
      succOf("3") shouldBe expected(("i < 3", AlwaysEdge))

      import io.shiftleft.semanticcpg.language._
      val codeStr = cpg.method.ast.code(".*loop1:.*").code.head
      succOf("i < 3") shouldBe expected(("loop2:", AlwaysEdge), (codeStr, AlwaysEdge))
      succOf(codeStr) shouldBe expected(("RET", AlwaysEdge))

      succOf("loop2:") shouldBe expected(("j", AlwaysEdge))
      succOf("j") shouldBe expected(("0", 1, AlwaysEdge))
      succOf("0", 1) shouldBe expected(("j = 0", AlwaysEdge))
      succOf("j = 0") shouldBe expected(("j", 1, AlwaysEdge))
      succOf("j", 1) shouldBe expected(("3", 1, AlwaysEdge))
      succOf("3", 1) shouldBe expected(("j < 3", AlwaysEdge))

      val code2 = cpg.method.ast.isBlock.code("loop2: for.*").code.head
      succOf("j < 3") shouldBe expected((code2, AlwaysEdge), ("i", 2, AlwaysEdge))
      succOf(code2) shouldBe expected(("i", 2, AlwaysEdge))

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

    "be correct for plain while loop" in {
      implicit val cpg: Cpg = code("while (x < 1) { y = 2; }")
      succOf(":program") shouldBe expected(("x", AlwaysEdge))
      succOf("x") shouldBe expected(("1", AlwaysEdge))
      succOf("1") shouldBe expected(("x < 1", AlwaysEdge))
      succOf("x < 1") shouldBe expected(("y", TrueEdge), ("RET", FalseEdge))
      succOf("y") shouldBe expected(("2", AlwaysEdge))
      succOf("2") shouldBe expected(("y = 2", AlwaysEdge))
      succOf("y = 2") shouldBe expected(("x", AlwaysEdge))
      succOf("x") shouldBe expected(("1", AlwaysEdge))
    }

    "be correct for plain while loop with break" in {
      implicit val cpg: Cpg = code("while (x < 1) { break; y; }")
      succOf(":program") shouldBe expected(("x", AlwaysEdge))
      succOf("x") shouldBe expected(("1", AlwaysEdge))
      succOf("1") shouldBe expected(("x < 1", AlwaysEdge))
      succOf("x < 1") shouldBe expected(("break;", TrueEdge), ("RET", FalseEdge))
      succOf("break;") shouldBe expected(("RET", AlwaysEdge))
      succOf("y") shouldBe expected(("x", AlwaysEdge))
    }

    "be correct for plain while loop with continue" in {
      implicit val cpg: Cpg = code("while (x < 1) { continue; y; }")
      succOf(":program") shouldBe expected(("x", AlwaysEdge))
      succOf("x") shouldBe expected(("1", AlwaysEdge))
      succOf("1") shouldBe expected(("x < 1", AlwaysEdge))
      succOf("x < 1") shouldBe expected(("continue;", TrueEdge), ("RET", FalseEdge))
      succOf("continue;") shouldBe expected(("x", AlwaysEdge))
      succOf("y") shouldBe expected(("x", AlwaysEdge))
    }

    "be correct for nested while loop" in {
      implicit val cpg: Cpg = code("while (x) {while(y) {z;}}")
      succOf(":program") shouldBe expected(("x", AlwaysEdge))
      succOf("x") shouldBe expected(("y", TrueEdge), ("RET", FalseEdge))
      succOf("y") shouldBe expected(("z", TrueEdge), ("x", FalseEdge))
    }

    "be correct for nested while loop with break" in {
      implicit val cpg: Cpg = code("while (x) { while(y) { break; z;} a;} b;")
      succOf(":program") shouldBe expected(("x", AlwaysEdge))
      succOf("x") shouldBe expected(("y", TrueEdge), ("b", FalseEdge))
      succOf("y") shouldBe expected(("break;", TrueEdge), ("a", FalseEdge))
      succOf("a") shouldBe expected(("x", AlwaysEdge))
      succOf("b") shouldBe expected(("RET", AlwaysEdge))
    }

    "be correct for another nested while loop with break" in {
      implicit val cpg: Cpg = code("while (x) { while(y) { break; z;} a; break; b; } c;")
      succOf(":program") shouldBe expected(("x", AlwaysEdge))
      succOf("x") shouldBe expected(("y", TrueEdge), ("c", FalseEdge))
      succOf("y") shouldBe expected(("break;", TrueEdge), ("a", FalseEdge))
      succOf("break;") shouldBe expected(("a", AlwaysEdge))
      succOf("a") shouldBe expected(("break;", 1, AlwaysEdge))
      succOf("break;", 1) shouldBe expected(("c", AlwaysEdge))
      succOf("c") shouldBe expected(("RET", AlwaysEdge))
    }

    "nested while loop with conditional break" in {
      implicit val cpg: Cpg = code(s"""
        |while (x) {
        |  if (y) {
        |	   break;
        |	 }
        |	 while (z) {
        |    break;
        |  }
        |}
      """.stripMargin)
      succOf(":program") shouldBe expected(("x", AlwaysEdge))
      succOf("x") shouldBe expected(("y", TrueEdge), ("RET", FalseEdge))
      succOf("y") shouldBe expected(("break;", TrueEdge), ("z", FalseEdge))
      succOf("break;") shouldBe expected(("RET", AlwaysEdge))
      succOf("break;", 1) shouldBe expected(("x", AlwaysEdge))
      succOf("z") shouldBe expected(("break;", 1, TrueEdge), ("x", FalseEdge))
    }

    // DO-WHILE Loops
    "be correct for plain do-while loop" in {
      implicit val cpg: Cpg = code("do { y = 2; } while (x < 1);")
      succOf(":program") shouldBe expected(("y", AlwaysEdge))
      succOf("y") shouldBe expected(("2", AlwaysEdge))
      succOf("2") shouldBe expected(("y = 2", AlwaysEdge))
      succOf("y = 2") shouldBe expected(("x", AlwaysEdge))
      succOf("x") shouldBe expected(("1", AlwaysEdge))
      succOf("1") shouldBe expected(("x < 1", AlwaysEdge))
      succOf("x < 1") shouldBe expected(("y", TrueEdge), ("RET", FalseEdge))
    }

    "be correct for plain do-while loop with break" in {
      implicit val cpg: Cpg = code("do { break; y; } while (x < 1);")
      succOf(":program") shouldBe expected(("break;", AlwaysEdge))
      succOf("break;") shouldBe expected(("RET", AlwaysEdge))
      succOf("y") shouldBe expected(("x", AlwaysEdge))
      succOf("x") shouldBe expected(("1", AlwaysEdge))
      succOf("1") shouldBe expected(("x < 1", AlwaysEdge))
      succOf("x < 1") shouldBe expected(("break;", TrueEdge), ("RET", FalseEdge))
    }

    "be correct for plain do-while loop with continue" in {
      implicit val cpg: Cpg = code("do { continue; y; } while (x < 1);")
      succOf(":program") shouldBe expected(("continue;", AlwaysEdge))
      succOf("continue;") shouldBe expected(("x", AlwaysEdge))
      succOf("y") shouldBe expected(("x", AlwaysEdge))
      succOf("x") shouldBe expected(("1", AlwaysEdge))
      succOf("1") shouldBe expected(("x < 1", AlwaysEdge))
      succOf("x < 1") shouldBe expected(("continue;", TrueEdge), ("RET", FalseEdge))
    }

    "be correct for nested do-while loop with continue" in {
      implicit val cpg: Cpg = code("do { do { x; } while (y); } while (z);")
      succOf(":program") shouldBe expected(("x", AlwaysEdge))
      succOf("x") shouldBe expected(("y", AlwaysEdge))
      succOf("y") shouldBe expected(("x", TrueEdge), ("z", FalseEdge))
      succOf("z") shouldBe expected(("x", TrueEdge), ("RET", FalseEdge))
    }

    "be correct for nested while/do-while loops with break" in {
      implicit val cpg: Cpg = code("while (x) { do { while(y) { break; a; } z; } while (x < 1); } c;")
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

    "be correct for nested while/do-while loops with break and continue" in {
      implicit val cpg: Cpg = code("while(x) { do { break; } while (y) } o;")
      succOf(":program") shouldBe expected(("x", AlwaysEdge))
      succOf("x") shouldBe expected(("break;", TrueEdge), ("o", FalseEdge))
      succOf("break;") shouldBe expected(("x", AlwaysEdge))
      succOf("o") shouldBe expected(("RET", AlwaysEdge))
    }

    "be correct for two nested while loop with inner break" in {
      implicit val cpg: Cpg = code("while(y) { while(z) { break; x; } }")
      succOf(":program") shouldBe expected(("y", AlwaysEdge))
      succOf("y") shouldBe expected(("z", TrueEdge), ("RET", FalseEdge))
      succOf("z") shouldBe expected(("break;", TrueEdge), ("y", FalseEdge))
      succOf("break;") shouldBe expected(("y", AlwaysEdge))
    }

    // FOR Loops
    "be correct for plain for-loop" in {
      implicit val cpg: Cpg = code("for (x = 0; y < 1; z += 2) { a = 3; }")
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

    "be correct for plain for-loop with break" in {
      implicit val cpg: Cpg = code("for (x = 0; y < 1; z += 2) { break; a = 3; }")
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

    "be correct for plain for-loop with continue" in {
      implicit val cpg: Cpg = code("for (x = 0; y < 1; z += 2) { continue; a = 3; }")
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

    "be correct for for-loop with for-in" in {
      implicit val cpg: Cpg = code("for (var i in arr) { foo(i) }")
      testForInOrOf()
    }

    "be correct for for-loop with for-of" in {
      implicit val cpg: Cpg = code("for (var i of arr) { foo(i) }")
      testForInOrOf()
    }

    "be correct for nested for-loop" in {
      implicit val cpg: Cpg = code("for (x; y; z) { for (a; b; c) { u; } }")
      succOf(":program") shouldBe expected(("x", AlwaysEdge))
      succOf("x") shouldBe expected(("y", AlwaysEdge))
      succOf("y") shouldBe expected(("a", TrueEdge), ("RET", FalseEdge))
      succOf("z") shouldBe expected(("y", AlwaysEdge))
      succOf("a") shouldBe expected(("b", AlwaysEdge))
      succOf("b") shouldBe expected(("u", TrueEdge), ("z", FalseEdge))
      succOf("c") shouldBe expected(("b", AlwaysEdge))
      succOf("u") shouldBe expected(("c", AlwaysEdge))
    }

    "be correct for for-loop with empty condition" in {
      implicit val cpg: Cpg = code("for (;;) { a = 1; }")
      succOf(":program") shouldBe expected(("true", AlwaysEdge))
      succOf("true") shouldBe expected(("a", TrueEdge), ("RET", FalseEdge))
      succOf("a") shouldBe expected(("1", AlwaysEdge))
      succOf("1") shouldBe expected(("a = 1", AlwaysEdge))
      succOf("a = 1") shouldBe expected(("true", AlwaysEdge))
    }

    "be correct for for-loop with empty condition and break" in {
      implicit val cpg: Cpg = code("for (;;) { break; }")
      succOf(":program") shouldBe expected(("true", AlwaysEdge))
      succOf("true") shouldBe expected(("break;", TrueEdge), ("RET", FalseEdge))
      succOf("break;") shouldBe expected(("RET", AlwaysEdge))
    }

    "be correct for for-loop with empty condition and continue" in {
      implicit val cpg: Cpg = code("for (;;) { continue; }")

      succOf(":program") shouldBe expected(("true", AlwaysEdge))
      succOf("true") shouldBe expected(("continue;", TrueEdge), ("RET", FalseEdge))
      succOf("continue;") shouldBe expected(("true", AlwaysEdge))
    }

    "be correct with empty condition with nested empty for-loop" in {
      implicit val cpg: Cpg = code("for (;;) { for (;;) { x; } }")
      succOf(":program") shouldBe expected(("true", AlwaysEdge))
      succOf("true") shouldBe expected(("true", 1, TrueEdge), ("RET", FalseEdge))
      succOf("true", 1) shouldBe expected(("x", TrueEdge), ("true", 0, FalseEdge))
      succOf("x") shouldBe expected(("true", 1, AlwaysEdge))
    }

    "be correct for for-loop with empty block" in {
      implicit val cpg: Cpg = code("for (;;) ;")
      succOf(":program") shouldBe expected(("true", AlwaysEdge))
      succOf("true") shouldBe expected(("true", TrueEdge), ("RET", FalseEdge))
    }

    "be correct for simple if statement" in {
      implicit val cpg: Cpg = code("if (x) { y; }")
      succOf(":program") shouldBe expected(("x", AlwaysEdge))
      succOf("x") shouldBe expected(("y", TrueEdge), ("RET", FalseEdge))
      succOf("y") shouldBe expected(("RET", AlwaysEdge))
    }

    "be correct for simple if statement with else block" in {
      implicit val cpg: Cpg = code("if (x) { y; } else { z; }")
      succOf(":program") shouldBe expected(("x", AlwaysEdge))
      succOf("x") shouldBe expected(("y", TrueEdge), ("z", FalseEdge))
      succOf("y") shouldBe expected(("RET", AlwaysEdge))
      succOf("z") shouldBe expected(("RET", AlwaysEdge))
    }

    "be correct for nested if statement" in {
      implicit val cpg: Cpg = code("if (x) { if (y) { z; } }")
      succOf(":program") shouldBe expected(("x", AlwaysEdge))
      succOf("x") shouldBe expected(("y", TrueEdge), ("RET", FalseEdge))
      succOf("y") shouldBe expected(("z", TrueEdge), ("RET", FalseEdge))
      succOf("z") shouldBe expected(("RET", AlwaysEdge))
    }

    "be correct for nested if statement with else-if chains" in {
      implicit val cpg: Cpg = code("if (a) { b; } else if (c) { d;} else { e; }")
      succOf(":program") shouldBe expected(("a", AlwaysEdge))
      succOf("a") shouldBe expected(("b", TrueEdge), ("c", FalseEdge))
      succOf("b") shouldBe expected(("RET", AlwaysEdge))
      succOf("c") shouldBe expected(("d", TrueEdge), ("e", FalseEdge))
      succOf("d") shouldBe expected(("RET", AlwaysEdge))
      succOf("e") shouldBe expected(("RET", AlwaysEdge))
    }

    "be correct for switch-case with single case" in {
      implicit val cpg: Cpg = code("switch (x) { case 1: y;}")
      succOf(":program") shouldBe expected(("x", AlwaysEdge))
      succOf("x") shouldBe expected(("case 1:", CaseEdge), ("RET", CaseEdge))
      succOf("case 1:") shouldBe expected(("1", AlwaysEdge))
      succOf("1") shouldBe expected(("y", AlwaysEdge))
      succOf("y") shouldBe expected(("RET", AlwaysEdge))
    }

    "be correct for switch-case with multiple cases" in {
      implicit val cpg: Cpg = code("switch (x) { case 1: y; case 2: z;}")
      succOf(":program") shouldBe expected(("x", AlwaysEdge))
      succOf("x") shouldBe expected(("case 1:", CaseEdge), ("case 2:", CaseEdge), ("RET", CaseEdge))
      succOf("case 1:") shouldBe expected(("1", AlwaysEdge))
      succOf("1") shouldBe expected(("y", AlwaysEdge))
      succOf("y") shouldBe expected(("case 2:", AlwaysEdge))
      succOf("case 2:") shouldBe expected(("2", AlwaysEdge))
      succOf("2") shouldBe expected(("z", AlwaysEdge))
      succOf("z") shouldBe expected(("RET", AlwaysEdge))
    }

    "be correct for switch-case with multiple cases on the same spot" in {
      implicit val cpg: Cpg = code("switch (x) { case 1: case 2: y; }")
      succOf(":program") shouldBe expected(("x", AlwaysEdge))
      succOf("x") shouldBe expected(("case 1:", CaseEdge), ("case 2:", CaseEdge), ("RET", CaseEdge))
      succOf("case 1:") shouldBe expected(("1", AlwaysEdge))
      succOf("1") shouldBe expected(("case 2:", AlwaysEdge))
      succOf("case 2:") shouldBe expected(("2", AlwaysEdge))
      succOf("2") shouldBe expected(("y", AlwaysEdge))
      succOf("y") shouldBe expected(("RET", AlwaysEdge))
    }

    "be correct for switch-case with default case" in {
      implicit val cpg: Cpg = code("switch (x) { default: y; }")
      succOf(":program") shouldBe expected(("x", AlwaysEdge))
      succOf("x") shouldBe expected(("default:", CaseEdge))
      succOf("default:") shouldBe expected(("y", AlwaysEdge))
      succOf("y") shouldBe expected(("RET", AlwaysEdge))
    }

    "be correct for switch-case with multiple cases and default combined" in {
      implicit val cpg: Cpg = code("switch (x) { case 1: y; break; default: z;}")
      succOf(":program") shouldBe expected(("x", AlwaysEdge))
      succOf("x") shouldBe expected(("case 1:", CaseEdge), ("default:", CaseEdge))
      succOf("case 1:") shouldBe expected(("1", AlwaysEdge))
      succOf("1") shouldBe expected(("y", AlwaysEdge))
      succOf("y") shouldBe expected(("break;", AlwaysEdge))
      succOf("break;") shouldBe expected(("RET", AlwaysEdge))
      succOf("default:") shouldBe expected(("z", AlwaysEdge))
      succOf("z") shouldBe expected(("RET", AlwaysEdge))
    }

    "be correct for constructor call with new" in {
      implicit val cpg: Cpg = code("""var x = new MyClass(arg1, arg2)""")
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
      succOf("_tmp_0", 2) shouldBe expected(("new MyClass(arg1, arg2)", AlwaysEdge))
      succOf("new MyClass(arg1, arg2)") shouldBe expected(("var x = new MyClass(arg1, arg2)", AlwaysEdge))
      succOf("var x = new MyClass(arg1, arg2)") shouldBe expected(("RET", AlwaysEdge))
    }
  }

  private def testForInOrOf()(implicit cpg: Cpg): Unit = {
    succOf(":program") shouldBe expected(("_iterator_0", AlwaysEdge))
    succOf("_iterator_0") shouldBe expected(("arr", AlwaysEdge))
    succOf("arr") shouldBe expected(("<operator>.iterator(arr)", AlwaysEdge))
    succOf("<operator>.iterator(arr)") shouldBe expected(("_iterator_0 = <operator>.iterator(arr)", AlwaysEdge))
    succOf("_iterator_0 = <operator>.iterator(arr)") shouldBe expected(("_result_0", AlwaysEdge))
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

    import io.shiftleft.semanticcpg.language._
    val code = cpg.method.ast.isBlock.code("for \\(var i.*foo.*}").code.head
    succOf("!(_result_0 = _iterator_0.next()).done") shouldBe expected(("i", 1, TrueEdge), (code, FalseEdge))
    succOf(code) shouldBe expected(("RET", AlwaysEdge))

    succOf("i", 1) shouldBe expected(("_result_0", 2, AlwaysEdge))
    succOf("_result_0", 2) shouldBe expected(("value", AlwaysEdge))
    succOf("value") shouldBe expected(("_result_0.value", AlwaysEdge))
    succOf("_result_0.value") shouldBe expected(("i = _result_0.value", AlwaysEdge))
    succOf("i = _result_0.value") shouldBe expected(("foo", AlwaysEdge))
    succOf("foo") shouldBe expected(("this", 1, AlwaysEdge))
    succOf("this", 1) shouldBe expected(("i", 2, AlwaysEdge))
    succOf("i", 2) shouldBe expected(("foo(i)", AlwaysEdge))
    val code2 = "{ foo(i) }"
    succOf("foo(i)") shouldBe expected((code2, AlwaysEdge))
    succOf(code2) shouldBe expected(("_result_0", 1, AlwaysEdge))
  }

}
