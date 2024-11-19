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
      succOf(":program") should contain theSameElementsAs expected(("x", AlwaysEdge))
      succOf("x") should contain theSameElementsAs expected(("class Foo", AlwaysEdge))
      succOf("class Foo") should contain theSameElementsAs expected(("bar", AlwaysEdge))
      succOf("bar") should contain theSameElementsAs expected(("this", AlwaysEdge))
      succOf("this", NodeTypes.IDENTIFIER) should contain theSameElementsAs expected(("bar()", AlwaysEdge))
      succOf("bar()") should contain theSameElementsAs expected(("class Foo {}, bar()", AlwaysEdge))
      succOf("class Foo {}, bar()") should contain theSameElementsAs expected(
        ("let x = (class Foo {}, bar())", AlwaysEdge)
      )
      succOf("let x = (class Foo {}, bar())") should contain theSameElementsAs expected(("RET", AlwaysEdge))
    }

    "have correct structure for empty array literal" in {
      implicit val cpg: Cpg = code("var x = []")
      succOf(":program") should contain theSameElementsAs expected(("x", AlwaysEdge))
      succOf("x") should contain theSameElementsAs expected(("__ecma.Array.factory()", AlwaysEdge))
      succOf("__ecma.Array.factory()") should contain theSameElementsAs expected(("var x = []", AlwaysEdge))
    }

    "have correct structure for array literal with values" in {
      implicit val cpg: Cpg = code("var x = [1, 2]")
      succOf(":program") should contain theSameElementsAs expected(("x", AlwaysEdge))
      succOf("x") should contain theSameElementsAs expected(("_tmp_0", AlwaysEdge))
      succOf("_tmp_0") should contain theSameElementsAs expected(("__ecma.Array.factory()", AlwaysEdge))
      succOf("__ecma.Array.factory()") should contain theSameElementsAs expected(
        ("_tmp_0 = __ecma.Array.factory()", AlwaysEdge)
      )

      succOf("_tmp_0 = __ecma.Array.factory()") should contain theSameElementsAs expected(("_tmp_0", 1, AlwaysEdge))
      succOf("_tmp_0", 1) should contain theSameElementsAs expected(("push", AlwaysEdge))
      succOf("push") should contain theSameElementsAs expected(("_tmp_0.push", AlwaysEdge))
      succOf("_tmp_0.push") should contain theSameElementsAs expected(("_tmp_0", 2, AlwaysEdge))
      succOf("_tmp_0", 2) should contain theSameElementsAs expected(("1", AlwaysEdge))
      succOf("1") should contain theSameElementsAs expected(("_tmp_0.push(1)", AlwaysEdge))

      succOf("_tmp_0.push(1)") should contain theSameElementsAs expected(("_tmp_0", 3, AlwaysEdge))
      succOf("_tmp_0", 3) should contain theSameElementsAs expected(("push", 1, AlwaysEdge))
      succOf("push", 1) should contain theSameElementsAs expected(("_tmp_0.push", 1, AlwaysEdge))
      succOf("_tmp_0.push", 1) should contain theSameElementsAs expected(("_tmp_0", 4, AlwaysEdge))
      succOf("_tmp_0", 4) should contain theSameElementsAs expected(("2", AlwaysEdge))
      succOf("2") should contain theSameElementsAs expected(("_tmp_0.push(2)", AlwaysEdge))

      succOf("_tmp_0.push(2)") should contain theSameElementsAs expected(("_tmp_0", 5, AlwaysEdge))
      succOf("_tmp_0", 5) should contain theSameElementsAs expected(("[1, 2]", AlwaysEdge))
      succOf("[1, 2]") should contain theSameElementsAs expected(("var x = [1, 2]", AlwaysEdge))
      succOf("var x = [1, 2]") should contain theSameElementsAs expected(("RET", AlwaysEdge))
    }

    "have correct structure for untagged runtime node in call" in {
      implicit val cpg: Cpg = code(s"foo(`Hello $${world}!`)")
      succOf(":program") should contain theSameElementsAs expected(("foo", AlwaysEdge))
      succOf("foo") should contain theSameElementsAs expected(("this", AlwaysEdge))
      succOf("this", NodeTypes.IDENTIFIER) should contain theSameElementsAs expected(("\"Hello \"", AlwaysEdge))
      succOf("\"Hello \"") should contain theSameElementsAs expected(("world", AlwaysEdge))
      succOf("world") should contain theSameElementsAs expected(("\"!\"", AlwaysEdge))
      succOf("\"!\"") should contain theSameElementsAs expected(
        (s"${Operators.formatString}(\"Hello \", world, \"!\")", AlwaysEdge)
      )
      succOf(s"${Operators.formatString}(\"Hello \", world, \"!\")") should contain theSameElementsAs expected(
        (s"foo(`Hello $${world}!`)", AlwaysEdge)
      )
      succOf(s"foo(`Hello $${world}!`)") should contain theSameElementsAs expected(("RET", AlwaysEdge))
    }

    "have correct structure for untagged runtime node" in {
      implicit val cpg: Cpg = code(s"`$${x + 1}`")
      succOf(":program") should contain theSameElementsAs expected(("\"\"", AlwaysEdge))
      succOf("\"\"") should contain theSameElementsAs expected(("x", AlwaysEdge))
      succOf("x") should contain theSameElementsAs expected(("1", AlwaysEdge))
      succOf("1") should contain theSameElementsAs expected(("x + 1", AlwaysEdge))
      succOf("x + 1") should contain theSameElementsAs expected(("\"\"", 1, AlwaysEdge))
      succOf("\"\"", 1) should contain theSameElementsAs expected(
        (s"${Operators.formatString}(\"\", x + 1, \"\")", AlwaysEdge)
      )
      succOf(s"${Operators.formatString}(\"\", x + 1, \"\")") should contain theSameElementsAs expected(
        ("RET", AlwaysEdge)
      )
    }

    "have correct structure for tagged runtime node" in {
      implicit val cpg: Cpg = code(s"String.raw`../$${42}\\..`")
      succOf(":program") should contain theSameElementsAs expected(("String", AlwaysEdge))
      succOf("String") should contain theSameElementsAs expected(("raw", AlwaysEdge))
      succOf("raw") should contain theSameElementsAs expected(("String.raw", AlwaysEdge))
      succOf("String.raw") should contain theSameElementsAs expected(("String", 1, AlwaysEdge))
      succOf("String", 1) should contain theSameElementsAs expected(("_tmp_0", AlwaysEdge))
      succOf("_tmp_0") should contain theSameElementsAs expected(("__ecma.Array.factory()", AlwaysEdge))
      succOf("__ecma.Array.factory()") should contain theSameElementsAs expected(
        ("_tmp_0 = __ecma.Array.factory()", AlwaysEdge)
      )
      succOf("_tmp_0 = __ecma.Array.factory()") should contain theSameElementsAs expected(("_tmp_0", 1, AlwaysEdge))
      succOf("_tmp_0", 1) should contain theSameElementsAs expected(("push", AlwaysEdge))
      succOf("push") should contain theSameElementsAs expected(("_tmp_0.push", AlwaysEdge))
      succOf("_tmp_0.push") should contain theSameElementsAs expected(("_tmp_0", 2, AlwaysEdge))
      succOf("_tmp_0", 2) should contain theSameElementsAs expected(("\"../\"", AlwaysEdge))
      succOf("\"../\"") should contain theSameElementsAs expected(("_tmp_0.push(\"../\")", AlwaysEdge))
      succOf("_tmp_0.push(\"../\")") should contain theSameElementsAs expected(("_tmp_0", 3, AlwaysEdge))
      succOf("_tmp_0", 3) should contain theSameElementsAs expected(("push", 1, AlwaysEdge))
      succOf("push", 1) should contain theSameElementsAs expected(("_tmp_0.push", 1, AlwaysEdge))
      succOf("_tmp_0.push", 1) should contain theSameElementsAs expected(("_tmp_0", 4, AlwaysEdge))
      succOf("_tmp_0", 4) should contain theSameElementsAs expected(("\"\\..\"", AlwaysEdge))
      succOf("\"\\..\"") should contain theSameElementsAs expected(("_tmp_0.push(\"\\..\")", AlwaysEdge))
      succOf("_tmp_0.push(\"\\..\")") should contain theSameElementsAs expected(("_tmp_0", 5, AlwaysEdge))
      succOf("_tmp_0", 5) should contain theSameElementsAs expected(("`../${42}\\..`", AlwaysEdge))
      succOf("`../${42}\\..`") should contain theSameElementsAs expected(("42", AlwaysEdge))
      succOf("42") should contain theSameElementsAs expected((s"String.raw`../$${42}\\..`", AlwaysEdge))
      succOf(s"String.raw`../$${42}\\..`") should contain theSameElementsAs expected(("RET", AlwaysEdge))
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
      succOf(":program") should contain theSameElementsAs expected(("open", AlwaysEdge))
      succOf("open") should contain theSameElementsAs expected(("this", AlwaysEdge))
      succOf("this", NodeTypes.IDENTIFIER) should contain theSameElementsAs expected(("open()", AlwaysEdge))
      succOf("open()") should contain theSameElementsAs expected(("err", AlwaysEdge), ("close", AlwaysEdge))
      succOf("err") should contain theSameElementsAs expected(("handle", AlwaysEdge))
      succOf("handle()") should contain theSameElementsAs expected(("close", AlwaysEdge))
      succOf("close()") should contain theSameElementsAs expected(("RET", AlwaysEdge))
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
      succOf(":program") should contain theSameElementsAs expected(("true", AlwaysEdge))
      succOf("true") should contain theSameElementsAs expected(("doA", TrueEdge), ("doB", FalseEdge))
      succOf("doA()") should contain theSameElementsAs expected(("err", AlwaysEdge), ("close", AlwaysEdge))
      succOf("err") should contain theSameElementsAs expected(("handle", AlwaysEdge))
      succOf("doB()") should contain theSameElementsAs expected(("err", AlwaysEdge), ("close", AlwaysEdge))
      succOf("err") should contain theSameElementsAs expected(("handle", AlwaysEdge))
      succOf("handle()") should contain theSameElementsAs expected(("close", AlwaysEdge))
      succOf("close()") should contain theSameElementsAs expected(("RET", AlwaysEdge))
    }

    "be correct for 1 object with simple values" in {
      implicit val cpg: Cpg = code("""
         |var x = {
         | key1: "value",
         | key2: 2
         |}
         |""".stripMargin)
      succOf(":program") should contain theSameElementsAs expected(("x", AlwaysEdge))
      succOf("x") should contain theSameElementsAs expected(("_tmp_0", AlwaysEdge))
      succOf("_tmp_0") should contain theSameElementsAs expected(("key1", AlwaysEdge))
      succOf("key1") should contain theSameElementsAs expected(("_tmp_0.key1", AlwaysEdge))
      succOf("_tmp_0.key1") should contain theSameElementsAs expected(("\"value\"", AlwaysEdge))
      succOf("\"value\"") should contain theSameElementsAs expected(("_tmp_0.key1 = \"value\"", AlwaysEdge))

      succOf("_tmp_0.key1 = \"value\"") should contain theSameElementsAs expected(("_tmp_0", 1, AlwaysEdge))
      succOf("_tmp_0", 1) should contain theSameElementsAs expected(("key2", AlwaysEdge))
      succOf("key2") should contain theSameElementsAs expected(("_tmp_0.key2", AlwaysEdge))
      succOf("_tmp_0.key2") should contain theSameElementsAs expected(("2", AlwaysEdge))
      succOf("2") should contain theSameElementsAs expected(("_tmp_0.key2 = 2", AlwaysEdge))

      succOf("_tmp_0.key2 = 2") should contain theSameElementsAs expected(("_tmp_0", 2, AlwaysEdge))
      succOf("_tmp_0", 2) should contain theSameElementsAs expected(("{\n key1: \"value\",\n key2: 2\n}", AlwaysEdge))
      succOf("{\n key1: \"value\",\n key2: 2\n}") should contain theSameElementsAs expected(
        ("var x = {\n key1: \"value\",\n key2: 2\n}", AlwaysEdge)
      )
      succOf("var x = {\n key1: \"value\",\n key2: 2\n}") should contain theSameElementsAs expected(("RET", AlwaysEdge))
    }

    "be correct for member access used in an assignment (chained)" in {
      implicit val cpg: Cpg = code("a.b = c.z;")
      succOf(":program") should contain theSameElementsAs expected(("a", AlwaysEdge))
      succOf("a") should contain theSameElementsAs expected(("b", AlwaysEdge))
      succOf("b") should contain theSameElementsAs expected(("a.b", AlwaysEdge))
      succOf("a.b") should contain theSameElementsAs expected(("c", AlwaysEdge))
      succOf("c") should contain theSameElementsAs expected(("z", AlwaysEdge))
      succOf("z") should contain theSameElementsAs expected(("c.z", AlwaysEdge))
      succOf("c.z") should contain theSameElementsAs expected(("a.b = c.z", AlwaysEdge))
      succOf("a.b = c.z") should contain theSameElementsAs expected(("RET", AlwaysEdge))
    }

    "be correct for decl statement with assignment" in {
      implicit val cpg: Cpg = code("var x = 1;")
      succOf(":program") should contain theSameElementsAs expected(("x", AlwaysEdge))
      succOf("x") should contain theSameElementsAs expected(("1", AlwaysEdge))
      succOf("1") should contain theSameElementsAs expected(("var x = 1", AlwaysEdge))
      succOf("var x = 1") should contain theSameElementsAs expected(("RET", AlwaysEdge))
    }

    "be correct for nested expression" in {
      implicit val cpg: Cpg = code("x = y + 1;")
      succOf(":program") should contain theSameElementsAs expected(("x", AlwaysEdge))
      succOf("x") should contain theSameElementsAs expected(("y", AlwaysEdge))
      succOf("y") should contain theSameElementsAs expected(("1", AlwaysEdge))
      succOf("1") should contain theSameElementsAs expected(("y + 1", AlwaysEdge))
      succOf("y + 1") should contain theSameElementsAs expected(("x = y + 1", AlwaysEdge))
      succOf("x = y + 1") should contain theSameElementsAs expected(("RET", AlwaysEdge))
    }

    "be correct for return statement" in {
      implicit val cpg: Cpg = code("function foo(x) { return x; }")
      succOf("foo", NodeTypes.METHOD) should contain theSameElementsAs expected(("x", AlwaysEdge))
      succOf("x", NodeTypes.IDENTIFIER) should contain theSameElementsAs expected(("return x", AlwaysEdge))
      succOf("return x") should contain theSameElementsAs expected(("RET", AlwaysEdge))
    }

    "be correct for consecutive return statements" in {
      implicit val cpg: Cpg = code("function foo(x, y) { return x; return y; }")
      succOf("foo", NodeTypes.METHOD) should contain theSameElementsAs expected(("x", AlwaysEdge))
      succOf("x", NodeTypes.IDENTIFIER) should contain theSameElementsAs expected(("return x", AlwaysEdge))
      succOf("y", NodeTypes.IDENTIFIER) should contain theSameElementsAs expected(("return y", AlwaysEdge))
      succOf("return x") should contain theSameElementsAs expected(("RET", AlwaysEdge))
      succOf("return y") should contain theSameElementsAs expected(("RET", AlwaysEdge))
    }

    "be correct for outer program function which declares foo function object" in {
      implicit val cpg: Cpg = code("function foo(x, y) { return; }")
      succOf(":program", NodeTypes.METHOD) should contain theSameElementsAs expected(("foo", 2, AlwaysEdge))
      succOf("foo", NodeTypes.IDENTIFIER) should contain theSameElementsAs expected(("foo", 3, AlwaysEdge))
      succOf("foo", NodeTypes.METHOD_REF) should contain theSameElementsAs expected(
        ("function foo = function foo(x, y) { return; }", AlwaysEdge)
      )
      succOf("function foo = function foo(x, y) { return; }") should contain theSameElementsAs expected(
        ("RET", AlwaysEdge)
      )
    }

    "be correct for void return statement" in {
      implicit val cpg: Cpg = code("function foo() { return; }")
      succOf("foo", NodeTypes.METHOD) should contain theSameElementsAs expected(("return", AlwaysEdge))
      succOf("return") should contain theSameElementsAs expected(("RET", AlwaysEdge))
    }

    "be correct for call expression" in {
      implicit val cpg: Cpg = code("foo(a + 1, b);")
      succOf(":program") should contain theSameElementsAs expected(("foo", AlwaysEdge))
      succOf("foo") should contain theSameElementsAs expected(("this", AlwaysEdge))
      succOf("this", NodeTypes.IDENTIFIER) should contain theSameElementsAs expected(("a", AlwaysEdge))
      succOf("a") should contain theSameElementsAs expected(("1", AlwaysEdge))
      succOf("1") should contain theSameElementsAs expected(("a + 1", AlwaysEdge))
      succOf("a + 1") should contain theSameElementsAs expected(("b", AlwaysEdge))
      succOf("b") should contain theSameElementsAs expected(("foo(a + 1, b)", AlwaysEdge))
      succOf("foo(a + 1, b)") should contain theSameElementsAs expected(("RET", AlwaysEdge))
    }

    "be correct for chained calls" in {
      implicit val cpg: Cpg = code("x.foo(y).bar(z)")
      succOf(":program") should contain theSameElementsAs expected(("_tmp_0", AlwaysEdge))
      succOf("_tmp_0") should contain theSameElementsAs expected(("x", AlwaysEdge))
      succOf("x") should contain theSameElementsAs expected(("foo", AlwaysEdge))
      succOf("foo") should contain theSameElementsAs expected(("x.foo", AlwaysEdge))
      succOf("x.foo") should contain theSameElementsAs expected(("x", 1, AlwaysEdge))
      succOf("x", 1) should contain theSameElementsAs expected(("y", AlwaysEdge))
      succOf("y") should contain theSameElementsAs expected(("x.foo(y)", AlwaysEdge))
      succOf("x.foo(y)") should contain theSameElementsAs expected(("(_tmp_0 = x.foo(y))", AlwaysEdge))
      succOf("(_tmp_0 = x.foo(y))") should contain theSameElementsAs expected(("bar", AlwaysEdge))
      succOf("bar") should contain theSameElementsAs expected(("(_tmp_0 = x.foo(y)).bar", AlwaysEdge))
      succOf("(_tmp_0 = x.foo(y)).bar") should contain theSameElementsAs expected(("_tmp_0", 1, AlwaysEdge))
      succOf("_tmp_0", 1) should contain theSameElementsAs expected(("z", AlwaysEdge))
      succOf("z") should contain theSameElementsAs expected(("x.foo(y).bar(z)", AlwaysEdge))
      succOf("x.foo(y).bar(z)") should contain theSameElementsAs expected(("RET", AlwaysEdge))
    }

    "be correct for unary expression '++'" in {
      implicit val cpg: Cpg = code("x++")
      succOf(":program") should contain theSameElementsAs expected(("x", AlwaysEdge))
      succOf("x") should contain theSameElementsAs expected(("x++", AlwaysEdge))
      succOf("x++") should contain theSameElementsAs expected(("RET", AlwaysEdge))
    }

    "be correct for conditional expression" in {
      implicit val cpg: Cpg = code("x ? y : z;")
      succOf(":program") should contain theSameElementsAs expected(("x", AlwaysEdge))
      succOf("x") should contain theSameElementsAs expected(("y", TrueEdge), ("z", FalseEdge))
      succOf("y") should contain theSameElementsAs expected(("x ? y : z", AlwaysEdge))
      succOf("z") should contain theSameElementsAs expected(("x ? y : z", AlwaysEdge))
      succOf("x ? y : z") should contain theSameElementsAs expected(("RET", AlwaysEdge))
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
      succOf(":program") should contain theSameElementsAs expected(("var i, j;", AlwaysEdge))
      succOf("loop1:") should contain theSameElementsAs expected(("i", AlwaysEdge))
      succOf("i") should contain theSameElementsAs expected(("0", AlwaysEdge))
      succOf("0") should contain theSameElementsAs expected(("i = 0", AlwaysEdge))
      succOf("i = 0") should contain theSameElementsAs expected(("i", 1, AlwaysEdge))
      succOf("i", 1) should contain theSameElementsAs expected(("3", AlwaysEdge))
      succOf("3") should contain theSameElementsAs expected(("i < 3", AlwaysEdge))

      import io.shiftleft.semanticcpg.language._
      val codeStr = cpg.method.ast.code(".*loop1:.*").code.head
      succOf("i < 3") should contain theSameElementsAs expected(("loop2:", AlwaysEdge), (codeStr, AlwaysEdge))
      succOf(codeStr) should contain theSameElementsAs expected(("RET", AlwaysEdge))

      succOf("loop2:") should contain theSameElementsAs expected(("j", AlwaysEdge))
      succOf("j") should contain theSameElementsAs expected(("0", 1, AlwaysEdge))
      succOf("0", 1) should contain theSameElementsAs expected(("j = 0", AlwaysEdge))
      succOf("j = 0") should contain theSameElementsAs expected(("j", 1, AlwaysEdge))
      succOf("j", 1) should contain theSameElementsAs expected(("3", 1, AlwaysEdge))
      succOf("3", 1) should contain theSameElementsAs expected(("j < 3", AlwaysEdge))

      val code2 = cpg.method.ast.isBlock.code("loop2: for.*").code.head
      succOf("j < 3") should contain theSameElementsAs expected((code2, AlwaysEdge), ("i", 2, AlwaysEdge))
      succOf(code2) should contain theSameElementsAs expected(("i", 2, AlwaysEdge))

      succOf("i", 2) should contain theSameElementsAs expected(("i++", AlwaysEdge))
      succOf("i++") should contain theSameElementsAs expected(("i", 3, AlwaysEdge))
      succOf("i", 3) should contain theSameElementsAs expected(("1", AlwaysEdge))
      succOf("1") should contain theSameElementsAs expected(("i === 1", AlwaysEdge))
      succOf("i === 1") should contain theSameElementsAs expected(("j", AlwaysEdge), ("i === 1 && j === 1", AlwaysEdge))
      succOf("i === 1 && j === 1") should contain theSameElementsAs expected(
        ("continue loop1;", AlwaysEdge),
        ("console", AlwaysEdge)
      )
      succOf("continue loop1;") should contain theSameElementsAs expected(("loop1:", AlwaysEdge))
      succOf("console") should contain theSameElementsAs expected(("log", AlwaysEdge))
      succOf("log") should contain theSameElementsAs expected(("console.log", AlwaysEdge))
    }

    "be correct for plain while loop" in {
      implicit val cpg: Cpg = code("while (x < 1) { y = 2; }")
      succOf(":program") should contain theSameElementsAs expected(("x", AlwaysEdge))
      succOf("x") should contain theSameElementsAs expected(("1", AlwaysEdge))
      succOf("1") should contain theSameElementsAs expected(("x < 1", AlwaysEdge))
      succOf("x < 1") should contain theSameElementsAs expected(("y", TrueEdge), ("RET", FalseEdge))
      succOf("y") should contain theSameElementsAs expected(("2", AlwaysEdge))
      succOf("2") should contain theSameElementsAs expected(("y = 2", AlwaysEdge))
      succOf("y = 2") should contain theSameElementsAs expected(("x", AlwaysEdge))
      succOf("x") should contain theSameElementsAs expected(("1", AlwaysEdge))
    }

    "be correct for plain while loop with break" in {
      implicit val cpg: Cpg = code("while (x < 1) { break; y; }")
      succOf(":program") should contain theSameElementsAs expected(("x", AlwaysEdge))
      succOf("x") should contain theSameElementsAs expected(("1", AlwaysEdge))
      succOf("1") should contain theSameElementsAs expected(("x < 1", AlwaysEdge))
      succOf("x < 1") should contain theSameElementsAs expected(("break;", TrueEdge), ("RET", FalseEdge))
      succOf("break;") should contain theSameElementsAs expected(("RET", AlwaysEdge))
      succOf("y") should contain theSameElementsAs expected(("x", AlwaysEdge))
    }

    "be correct for plain while loop with continue" in {
      implicit val cpg: Cpg = code("while (x < 1) { continue; y; }")
      succOf(":program") should contain theSameElementsAs expected(("x", AlwaysEdge))
      succOf("x") should contain theSameElementsAs expected(("1", AlwaysEdge))
      succOf("1") should contain theSameElementsAs expected(("x < 1", AlwaysEdge))
      succOf("x < 1") should contain theSameElementsAs expected(("continue;", TrueEdge), ("RET", FalseEdge))
      succOf("continue;") should contain theSameElementsAs expected(("x", AlwaysEdge))
      succOf("y") should contain theSameElementsAs expected(("x", AlwaysEdge))
    }

    "be correct for nested while loop" in {
      implicit val cpg: Cpg = code("while (x) {while(y) {z;}}")
      succOf(":program") should contain theSameElementsAs expected(("x", AlwaysEdge))
      succOf("x") should contain theSameElementsAs expected(("y", TrueEdge), ("RET", FalseEdge))
      succOf("y") should contain theSameElementsAs expected(("z", TrueEdge), ("x", FalseEdge))
    }

    "be correct for nested while loop with break" in {
      implicit val cpg: Cpg = code("while (x) { while(y) { break; z;} a;} b;")
      succOf(":program") should contain theSameElementsAs expected(("x", AlwaysEdge))
      succOf("x") should contain theSameElementsAs expected(("y", TrueEdge), ("b", FalseEdge))
      succOf("y") should contain theSameElementsAs expected(("break;", TrueEdge), ("a", FalseEdge))
      succOf("a") should contain theSameElementsAs expected(("x", AlwaysEdge))
      succOf("b") should contain theSameElementsAs expected(("RET", AlwaysEdge))
    }

    "be correct for another nested while loop with break" in {
      implicit val cpg: Cpg = code("while (x) { while(y) { break; z;} a; break; b; } c;")
      succOf(":program") should contain theSameElementsAs expected(("x", AlwaysEdge))
      succOf("x") should contain theSameElementsAs expected(("y", TrueEdge), ("c", FalseEdge))
      succOf("y") should contain theSameElementsAs expected(("break;", TrueEdge), ("a", FalseEdge))
      succOf("break;") should contain theSameElementsAs expected(("a", AlwaysEdge))
      succOf("a") should contain theSameElementsAs expected(("break;", 1, AlwaysEdge))
      succOf("break;", 1) should contain theSameElementsAs expected(("c", AlwaysEdge))
      succOf("c") should contain theSameElementsAs expected(("RET", AlwaysEdge))
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
      succOf(":program") should contain theSameElementsAs expected(("x", AlwaysEdge))
      succOf("x") should contain theSameElementsAs expected(("y", TrueEdge), ("RET", FalseEdge))
      succOf("y") should contain theSameElementsAs expected(("break;", TrueEdge), ("z", FalseEdge))
      succOf("break;") should contain theSameElementsAs expected(("RET", AlwaysEdge))
      succOf("break;", 1) should contain theSameElementsAs expected(("x", AlwaysEdge))
      succOf("z") should contain theSameElementsAs expected(("break;", 1, TrueEdge), ("x", FalseEdge))
    }

    // DO-WHILE Loops
    "be correct for plain do-while loop" in {
      implicit val cpg: Cpg = code("do { y = 2; } while (x < 1);")
      succOf(":program") should contain theSameElementsAs expected(("y", AlwaysEdge))
      succOf("y") should contain theSameElementsAs expected(("2", AlwaysEdge))
      succOf("2") should contain theSameElementsAs expected(("y = 2", AlwaysEdge))
      succOf("y = 2") should contain theSameElementsAs expected(("x", AlwaysEdge))
      succOf("x") should contain theSameElementsAs expected(("1", AlwaysEdge))
      succOf("1") should contain theSameElementsAs expected(("x < 1", AlwaysEdge))
      succOf("x < 1") should contain theSameElementsAs expected(("y", TrueEdge), ("RET", FalseEdge))
    }

    "be correct for plain do-while loop with break" in {
      implicit val cpg: Cpg = code("do { break; y; } while (x < 1);")
      succOf(":program") should contain theSameElementsAs expected(("break;", AlwaysEdge))
      succOf("break;") should contain theSameElementsAs expected(("RET", AlwaysEdge))
      succOf("y") should contain theSameElementsAs expected(("x", AlwaysEdge))
      succOf("x") should contain theSameElementsAs expected(("1", AlwaysEdge))
      succOf("1") should contain theSameElementsAs expected(("x < 1", AlwaysEdge))
      succOf("x < 1") should contain theSameElementsAs expected(("break;", TrueEdge), ("RET", FalseEdge))
    }

    "be correct for plain do-while loop with continue" in {
      implicit val cpg: Cpg = code("do { continue; y; } while (x < 1);")
      succOf(":program") should contain theSameElementsAs expected(("continue;", AlwaysEdge))
      succOf("continue;") should contain theSameElementsAs expected(("x", AlwaysEdge))
      succOf("y") should contain theSameElementsAs expected(("x", AlwaysEdge))
      succOf("x") should contain theSameElementsAs expected(("1", AlwaysEdge))
      succOf("1") should contain theSameElementsAs expected(("x < 1", AlwaysEdge))
      succOf("x < 1") should contain theSameElementsAs expected(("continue;", TrueEdge), ("RET", FalseEdge))
    }

    "be correct for nested do-while loop with continue" in {
      implicit val cpg: Cpg = code("do { do { x; } while (y); } while (z);")
      succOf(":program") should contain theSameElementsAs expected(("x", AlwaysEdge))
      succOf("x") should contain theSameElementsAs expected(("y", AlwaysEdge))
      succOf("y") should contain theSameElementsAs expected(("x", TrueEdge), ("z", FalseEdge))
      succOf("z") should contain theSameElementsAs expected(("x", TrueEdge), ("RET", FalseEdge))
    }

    "be correct for nested while/do-while loops with break" in {
      implicit val cpg: Cpg = code("while (x) { do { while(y) { break; a; } z; } while (x < 1); } c;")
      succOf(":program") should contain theSameElementsAs expected(("x", AlwaysEdge))
      succOf("x") should contain theSameElementsAs expected(("y", TrueEdge), ("c", FalseEdge))
      succOf("y") should contain theSameElementsAs expected(("break;", TrueEdge), ("z", FalseEdge))
      succOf("break;") should contain theSameElementsAs expected(("z", AlwaysEdge))
      succOf("z") should contain theSameElementsAs expected(("x", 1, AlwaysEdge))
      succOf("x", 1) should contain theSameElementsAs expected(("1", AlwaysEdge))
      succOf("1") should contain theSameElementsAs expected(("x < 1", AlwaysEdge))
      succOf("x < 1") should contain theSameElementsAs expected(("y", TrueEdge), ("x", FalseEdge))
      succOf("c") should contain theSameElementsAs expected(("RET", AlwaysEdge))
    }

    "be correct for nested while/do-while loops with break and continue" in {
      implicit val cpg: Cpg = code("while(x) { do { break; } while (y) } o;")
      succOf(":program") should contain theSameElementsAs expected(("x", AlwaysEdge))
      succOf("x") should contain theSameElementsAs expected(("break;", TrueEdge), ("o", FalseEdge))
      succOf("break;") should contain theSameElementsAs expected(("x", AlwaysEdge))
      succOf("o") should contain theSameElementsAs expected(("RET", AlwaysEdge))
    }

    "be correct for two nested while loop with inner break" in {
      implicit val cpg: Cpg = code("while(y) { while(z) { break; x; } }")
      succOf(":program") should contain theSameElementsAs expected(("y", AlwaysEdge))
      succOf("y") should contain theSameElementsAs expected(("z", TrueEdge), ("RET", FalseEdge))
      succOf("z") should contain theSameElementsAs expected(("break;", TrueEdge), ("y", FalseEdge))
      succOf("break;") should contain theSameElementsAs expected(("y", AlwaysEdge))
    }

    // FOR Loops
    "be correct for plain for-loop" in {
      implicit val cpg: Cpg = code("for (x = 0; y < 1; z += 2) { a = 3; }")
      succOf(":program") should contain theSameElementsAs expected(("x", AlwaysEdge))
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

    "be correct for plain for-loop with break" in {
      implicit val cpg: Cpg = code("for (x = 0; y < 1; z += 2) { break; a = 3; }")
      succOf(":program") should contain theSameElementsAs expected(("x", AlwaysEdge))
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

    "be correct for plain for-loop with continue" in {
      implicit val cpg: Cpg = code("for (x = 0; y < 1; z += 2) { continue; a = 3; }")
      succOf(":program") should contain theSameElementsAs expected(("x", AlwaysEdge))
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
      succOf(":program") should contain theSameElementsAs expected(("x", AlwaysEdge))
      succOf("x") should contain theSameElementsAs expected(("y", AlwaysEdge))
      succOf("y") should contain theSameElementsAs expected(("a", TrueEdge), ("RET", FalseEdge))
      succOf("z") should contain theSameElementsAs expected(("y", AlwaysEdge))
      succOf("a") should contain theSameElementsAs expected(("b", AlwaysEdge))
      succOf("b") should contain theSameElementsAs expected(("u", TrueEdge), ("z", FalseEdge))
      succOf("c") should contain theSameElementsAs expected(("b", AlwaysEdge))
      succOf("u") should contain theSameElementsAs expected(("c", AlwaysEdge))
    }

    "be correct for for-loop with empty condition" in {
      implicit val cpg: Cpg = code("for (;;) { a = 1; }")
      succOf(":program") should contain theSameElementsAs expected(("true", AlwaysEdge))
      succOf("true") should contain theSameElementsAs expected(("a", TrueEdge), ("RET", FalseEdge))
      succOf("a") should contain theSameElementsAs expected(("1", AlwaysEdge))
      succOf("1") should contain theSameElementsAs expected(("a = 1", AlwaysEdge))
      succOf("a = 1") should contain theSameElementsAs expected(("true", AlwaysEdge))
    }

    "be correct for for-loop with empty condition and break" in {
      implicit val cpg: Cpg = code("for (;;) { break; }")
      succOf(":program") should contain theSameElementsAs expected(("true", AlwaysEdge))
      succOf("true") should contain theSameElementsAs expected(("break;", TrueEdge), ("RET", FalseEdge))
      succOf("break;") should contain theSameElementsAs expected(("RET", AlwaysEdge))
    }

    "be correct for for-loop with empty condition and continue" in {
      implicit val cpg: Cpg = code("for (;;) { continue; }")

      succOf(":program") should contain theSameElementsAs expected(("true", AlwaysEdge))
      succOf("true") should contain theSameElementsAs expected(("continue;", TrueEdge), ("RET", FalseEdge))
      succOf("continue;") should contain theSameElementsAs expected(("true", AlwaysEdge))
    }

    "be correct with empty condition with nested empty for-loop" in {
      implicit val cpg: Cpg = code("for (;;) { for (;;) { x; } }")
      succOf(":program") should contain theSameElementsAs expected(("true", AlwaysEdge))
      succOf("true") should contain theSameElementsAs expected(("true", 1, TrueEdge), ("RET", FalseEdge))
      succOf("true", 1) should contain theSameElementsAs expected(("x", TrueEdge), ("true", 0, FalseEdge))
      succOf("x") should contain theSameElementsAs expected(("true", 1, AlwaysEdge))
    }

    "be correct for for-loop with empty block" in {
      implicit val cpg: Cpg = code("for (;;) ;")
      succOf(":program") should contain theSameElementsAs expected(("true", AlwaysEdge))
      succOf("true") should contain theSameElementsAs expected(("true", TrueEdge), ("RET", FalseEdge))
    }

    "be correct for simple if statement" in {
      implicit val cpg: Cpg = code("if (x) { y; }")
      succOf(":program") should contain theSameElementsAs expected(("x", AlwaysEdge))
      succOf("x") should contain theSameElementsAs expected(("y", TrueEdge), ("RET", FalseEdge))
      succOf("y") should contain theSameElementsAs expected(("RET", AlwaysEdge))
    }

    "be correct for simple if statement with else block" in {
      implicit val cpg: Cpg = code("if (x) { y; } else { z; }")
      succOf(":program") should contain theSameElementsAs expected(("x", AlwaysEdge))
      succOf("x") should contain theSameElementsAs expected(("y", TrueEdge), ("z", FalseEdge))
      succOf("y") should contain theSameElementsAs expected(("RET", AlwaysEdge))
      succOf("z") should contain theSameElementsAs expected(("RET", AlwaysEdge))
    }

    "be correct for nested if statement" in {
      implicit val cpg: Cpg = code("if (x) { if (y) { z; } }")
      succOf(":program") should contain theSameElementsAs expected(("x", AlwaysEdge))
      succOf("x") should contain theSameElementsAs expected(("y", TrueEdge), ("RET", FalseEdge))
      succOf("y") should contain theSameElementsAs expected(("z", TrueEdge), ("RET", FalseEdge))
      succOf("z") should contain theSameElementsAs expected(("RET", AlwaysEdge))
    }

    "be correct for nested if statement with else-if chains" in {
      implicit val cpg: Cpg = code("if (a) { b; } else if (c) { d;} else { e; }")
      succOf(":program") should contain theSameElementsAs expected(("a", AlwaysEdge))
      succOf("a") should contain theSameElementsAs expected(("b", TrueEdge), ("c", FalseEdge))
      succOf("b") should contain theSameElementsAs expected(("RET", AlwaysEdge))
      succOf("c") should contain theSameElementsAs expected(("d", TrueEdge), ("e", FalseEdge))
      succOf("d") should contain theSameElementsAs expected(("RET", AlwaysEdge))
      succOf("e") should contain theSameElementsAs expected(("RET", AlwaysEdge))
    }

    "be correct for switch-case with single case" in {
      implicit val cpg: Cpg = code("switch (x) { case 1: y;}")
      succOf(":program") should contain theSameElementsAs expected(("x", AlwaysEdge))
      succOf("x") should contain theSameElementsAs expected(("case 1:", CaseEdge), ("RET", CaseEdge))
      succOf("case 1:") should contain theSameElementsAs expected(("1", AlwaysEdge))
      succOf("1") should contain theSameElementsAs expected(("y", AlwaysEdge))
      succOf("y") should contain theSameElementsAs expected(("RET", AlwaysEdge))
    }

    "be correct for switch-case with multiple cases" in {
      implicit val cpg: Cpg = code("switch (x) { case 1: y; case 2: z;}")
      succOf(":program") should contain theSameElementsAs expected(("x", AlwaysEdge))
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

    "be correct for switch-case with multiple cases on the same spot" in {
      implicit val cpg: Cpg = code("switch (x) { case 1: case 2: y; }")
      succOf(":program") should contain theSameElementsAs expected(("x", AlwaysEdge))
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

    "be correct for switch-case with default case" in {
      implicit val cpg: Cpg = code("switch (x) { default: y; }")
      succOf(":program") should contain theSameElementsAs expected(("x", AlwaysEdge))
      succOf("x") should contain theSameElementsAs expected(("default:", CaseEdge))
      succOf("default:") should contain theSameElementsAs expected(("y", AlwaysEdge))
      succOf("y") should contain theSameElementsAs expected(("RET", AlwaysEdge))
    }

    "be correct for switch-case with multiple cases and default combined" in {
      implicit val cpg: Cpg = code("switch (x) { case 1: y; break; default: z;}")
      succOf(":program") should contain theSameElementsAs expected(("x", AlwaysEdge))
      succOf("x") should contain theSameElementsAs expected(("case 1:", CaseEdge), ("default:", CaseEdge))
      succOf("case 1:") should contain theSameElementsAs expected(("1", AlwaysEdge))
      succOf("1") should contain theSameElementsAs expected(("y", AlwaysEdge))
      succOf("y") should contain theSameElementsAs expected(("break;", AlwaysEdge))
      succOf("break;") should contain theSameElementsAs expected(("RET", AlwaysEdge))
      succOf("default:") should contain theSameElementsAs expected(("z", AlwaysEdge))
      succOf("z") should contain theSameElementsAs expected(("RET", AlwaysEdge))
    }

    "be correct for constructor call with new" in {
      implicit val cpg: Cpg = code("""var x = new MyClass(arg1, arg2)""")
      succOf(":program") should contain theSameElementsAs expected(("x", AlwaysEdge))
      succOf("x") should contain theSameElementsAs expected(("_tmp_0", AlwaysEdge))
      succOf("_tmp_0") should contain theSameElementsAs expected((".alloc", AlwaysEdge))
      succOf(".alloc") should contain theSameElementsAs expected(("_tmp_0 = .alloc", AlwaysEdge))
      succOf("_tmp_0 = .alloc") should contain theSameElementsAs expected(("MyClass", AlwaysEdge))
      succOf("MyClass") should contain theSameElementsAs expected(("_tmp_0", 1, AlwaysEdge))
      succOf("_tmp_0", 1) should contain theSameElementsAs expected(("arg1", AlwaysEdge))
      succOf("arg1") should contain theSameElementsAs expected(("arg2", AlwaysEdge))
      succOf("arg2") should contain theSameElementsAs expected(("new MyClass(arg1, arg2)", AlwaysEdge))
      succOf("new MyClass(arg1, arg2)", NodeTypes.CALL) should contain theSameElementsAs expected(
        ("_tmp_0", 2, AlwaysEdge)
      )
      succOf("_tmp_0", 2) should contain theSameElementsAs expected(("new MyClass(arg1, arg2)", AlwaysEdge))
      succOf("new MyClass(arg1, arg2)") should contain theSameElementsAs expected(
        ("var x = new MyClass(arg1, arg2)", AlwaysEdge)
      )
      succOf("var x = new MyClass(arg1, arg2)") should contain theSameElementsAs expected(("RET", AlwaysEdge))
    }
  }

  private def testForInOrOf()(implicit cpg: Cpg): Unit = {
    succOf(":program") should contain theSameElementsAs expected(("_iterator_0", AlwaysEdge))
    succOf("_iterator_0") should contain theSameElementsAs expected(("arr", AlwaysEdge))
    succOf("arr") should contain theSameElementsAs expected(("<operator>.iterator(arr)", AlwaysEdge))
    succOf("<operator>.iterator(arr)") should contain theSameElementsAs expected(
      ("_iterator_0 = <operator>.iterator(arr)", AlwaysEdge)
    )
    succOf("_iterator_0 = <operator>.iterator(arr)") should contain theSameElementsAs expected(
      ("_result_0", AlwaysEdge)
    )
    succOf("_result_0") should contain theSameElementsAs expected(("i", AlwaysEdge))
    succOf("i") should contain theSameElementsAs expected(("_result_0", 1, AlwaysEdge))
    succOf("_result_0", 1) should contain theSameElementsAs expected(("_iterator_0", 1, AlwaysEdge))
    succOf("_iterator_0", 1) should contain theSameElementsAs expected(("next", AlwaysEdge))
    succOf("next") should contain theSameElementsAs expected(("_iterator_0.next", AlwaysEdge))
    succOf("_iterator_0.next") should contain theSameElementsAs expected(("_iterator_0", 2, AlwaysEdge))
    succOf("_iterator_0", 2) should contain theSameElementsAs expected(("_iterator_0.next()", AlwaysEdge))
    succOf("_iterator_0.next()") should contain theSameElementsAs expected(
      ("(_result_0 = _iterator_0.next())", AlwaysEdge)
    )
    succOf("(_result_0 = _iterator_0.next())") should contain theSameElementsAs expected(("done", AlwaysEdge))
    succOf("done") should contain theSameElementsAs expected(("(_result_0 = _iterator_0.next()).done", AlwaysEdge))
    succOf("(_result_0 = _iterator_0.next()).done") should contain theSameElementsAs expected(
      ("!(_result_0 = _iterator_0.next()).done", AlwaysEdge)
    )

    import io.shiftleft.semanticcpg.language._
    val code = cpg.method.ast.isBlock.code("for \\(var i.*foo.*}").code.head
    succOf("!(_result_0 = _iterator_0.next()).done") should contain theSameElementsAs expected(
      ("i", 1, TrueEdge),
      (code, FalseEdge)
    )
    succOf(code) should contain theSameElementsAs expected(("RET", AlwaysEdge))

    succOf("i", 1) should contain theSameElementsAs expected(("_result_0", 2, AlwaysEdge))
    succOf("_result_0", 2) should contain theSameElementsAs expected(("value", AlwaysEdge))
    succOf("value") should contain theSameElementsAs expected(("_result_0.value", AlwaysEdge))
    succOf("_result_0.value") should contain theSameElementsAs expected(("i = _result_0.value", AlwaysEdge))
    succOf("i = _result_0.value") should contain theSameElementsAs expected(("foo", AlwaysEdge))
    succOf("foo") should contain theSameElementsAs expected(("this", 1, AlwaysEdge))
    succOf("this", 1) should contain theSameElementsAs expected(("i", 2, AlwaysEdge))
    succOf("i", 2) should contain theSameElementsAs expected(("foo(i)", AlwaysEdge))
    val code2 = "{ foo(i) }"
    succOf("foo(i)") should contain theSameElementsAs expected((code2, AlwaysEdge))
    succOf(code2) should contain theSameElementsAs expected(("_result_0", 1, AlwaysEdge))
  }

}
