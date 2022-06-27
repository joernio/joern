package io.joern.jssrc2cpg.passes

import better.files.File
import io.joern.jssrc2cpg.testfixtures.JsSrc2CpgFrontend
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated._
import io.shiftleft.semanticcpg.language._
import io.joern.x2cpg.passes.controlflow.CfgCreationPass
import io.joern.x2cpg.passes.controlflow.cfgcreation.Cfg._
import overflowdb.traversal._
import overflowdb._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.jdk.CollectionConverters._

class CfgCreationPassTest extends AnyWordSpec with Matchers {

  "CFG generation for simple fragments" should {
    "have correct structure for block expression" in {
      new CfgFixture("let x = (class Foo {}, bar())") {
        succOf(":program") shouldBe expected(("x", AlwaysEdge))
        succOf("x") shouldBe expected(("class Foo", AlwaysEdge))
        succOf("class Foo") shouldBe expected(("bar", AlwaysEdge))
        succOf("bar") shouldBe expected(("this", AlwaysEdge))
        succOf("this", NodeTypes.IDENTIFIER) shouldBe expected(("bar()", AlwaysEdge))
        succOf("bar()") shouldBe expected(("x = (class Foo {}, bar())", AlwaysEdge))
        succOf("x = (class Foo {}, bar())") shouldBe expected(("RET", AlwaysEdge))
      }
    }

    "have correct structure for empty array literal" in {
      new CfgFixture("var x = []") {
        succOf(":program") shouldBe expected(("x", AlwaysEdge))
        succOf("x") shouldBe expected(("__ecma.Array.factory()", AlwaysEdge))
        succOf("__ecma.Array.factory()") shouldBe expected(("x = []", AlwaysEdge))
      }
    }

    "have correct structure for array literal with values" in {
      new CfgFixture("var x = [1, 2]") {
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
    }

    "have correct structure for untagged runtime node in call" in {
      new CfgFixture(s"foo(`Hello $${world}!`)") {
        succOf(":program") shouldBe expected(("foo", AlwaysEdge))
        succOf("foo") shouldBe expected(("this", AlwaysEdge))
        succOf("this", NodeTypes.IDENTIFIER) shouldBe expected(("\"Hello \"", AlwaysEdge))
        succOf("\"Hello \"") shouldBe expected(("world", AlwaysEdge))
        succOf("world") shouldBe expected(("\"!\"", AlwaysEdge))
        succOf("\"!\"") shouldBe expected(("__Runtime.TO_STRING(\"Hello \", world, \"!\")", AlwaysEdge))
        succOf("__Runtime.TO_STRING(\"Hello \", world, \"!\")") shouldBe expected(
          ("foo(__Runtime.TO_STRING(\"Hello \", world, \"!\"))", AlwaysEdge)
        )
        succOf("foo(__Runtime.TO_STRING(\"Hello \", world, \"!\"))") shouldBe expected(("RET", AlwaysEdge))
      }
    }

    "have correct structure for untagged runtime node" in {
      new CfgFixture(s"`$${x + 1}`") {
        succOf(":program") shouldBe expected(("\"\"", AlwaysEdge))
        succOf("\"\"") shouldBe expected(("x", AlwaysEdge))
        succOf("x") shouldBe expected(("1", AlwaysEdge))
        succOf("1") shouldBe expected(("x + 1", AlwaysEdge))
        succOf("x + 1") shouldBe expected(("\"\"", 1, AlwaysEdge))
        succOf("\"\"", 1) shouldBe expected(("__Runtime.TO_STRING(\"\", x + 1, \"\")", AlwaysEdge))
        succOf("__Runtime.TO_STRING(\"\", x + 1, \"\")") shouldBe expected(("RET", AlwaysEdge))
      }
    }

    "have correct structure for tagged runtime node" in {
      new CfgFixture(s"String.raw`../$${42}\\..`") {
        succOf(":program") shouldBe expected(("\"../\"", AlwaysEdge))
        succOf("\"../\"") shouldBe expected(("42", AlwaysEdge))
        succOf("42") shouldBe expected(("\"\\..\"", AlwaysEdge))
        succOf("\"\\..\"") shouldBe expected(("__Runtime.TO_STRING(\"../\", 42, \"\\..\")", AlwaysEdge))
        succOf("__Runtime.TO_STRING(\"../\", 42, \"\\..\")") shouldBe expected(
          ("String.raw(__Runtime.TO_STRING(\"../\", 42, \"\\..\"))", AlwaysEdge)
        )
        succOf("String.raw(__Runtime.TO_STRING(\"../\", 42, \"\\..\"))") shouldBe expected(("RET", AlwaysEdge))
      }
    }

    "be correct for try" in {
      new CfgFixture("""
          |try {
          | open()
          |} catch(err) {
          | handle()
          |} finally {
          | close()
          |}
          |""".stripMargin) {
        succOf(":program") shouldBe expected(("open", AlwaysEdge))
        succOf("open") shouldBe expected(("this", AlwaysEdge))
        succOf("this", NodeTypes.IDENTIFIER) shouldBe expected(("open()", AlwaysEdge))
        succOf("open()") shouldBe expected(("handle", AlwaysEdge), ("close", AlwaysEdge))
        succOf("handle()") shouldBe expected(("close", AlwaysEdge))
        succOf("close()") shouldBe expected(("RET", AlwaysEdge))
      }
    }

    "be correct for try with multiple CFG exit nodes in try block" in {
      new CfgFixture("""
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
          |""".stripMargin) {
        succOf(":program") shouldBe expected(("true", AlwaysEdge))
        succOf("true") shouldBe expected(("doA", TrueEdge), ("doB", FalseEdge))
        succOf("doA()") shouldBe expected(("handle", AlwaysEdge), ("close", AlwaysEdge))
        succOf("doB()") shouldBe expected(("handle", AlwaysEdge), ("close", AlwaysEdge))
        succOf("handle()") shouldBe expected(("close", AlwaysEdge))
        succOf("close()") shouldBe expected(("RET", AlwaysEdge))
      }
    }

    "be correct for 1 object with simple values" in {
      new CfgFixture("""
        |var x = {
        | key1: "value",
        | key2: 2
        |}
        |""".stripMargin) {
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
    }

    "be correct for member access used in an assignment (chained)" in {
      new CfgFixture("a.b = c.z;") {
        succOf(":program") shouldBe expected(("a", AlwaysEdge))
        succOf("a") shouldBe expected(("b", AlwaysEdge))
        succOf("b") shouldBe expected(("a.b", AlwaysEdge))
        succOf("a.b") shouldBe expected(("c", AlwaysEdge))
        succOf("c") shouldBe expected(("z", AlwaysEdge))
        succOf("z") shouldBe expected(("c.z", AlwaysEdge))
        succOf("c.z") shouldBe expected(("a.b = c.z", AlwaysEdge))
        succOf("a.b = c.z") shouldBe expected(("RET", AlwaysEdge))
      }
    }

    "be correct for decl statement with assignment" in {
      new CfgFixture("var x = 1;") {
        succOf(":program") shouldBe expected(("x", AlwaysEdge))
        succOf("x") shouldBe expected(("1", AlwaysEdge))
        succOf("1") shouldBe expected(("x = 1", AlwaysEdge))
        succOf("x = 1") shouldBe expected(("RET", AlwaysEdge))
      }
    }

    "be correct for nested expression" in {
      new CfgFixture("x = y + 1;") {
        succOf(":program") shouldBe expected(("x", AlwaysEdge))
        succOf("x") shouldBe expected(("y", AlwaysEdge))
        succOf("y") shouldBe expected(("1", AlwaysEdge))
        succOf("1") shouldBe expected(("y + 1", AlwaysEdge))
        succOf("y + 1") shouldBe expected(("x = y + 1", AlwaysEdge))
        succOf("x = y + 1") shouldBe expected(("RET", AlwaysEdge))
      }
    }

    "be correct for return statement" in {
      new CfgFixture("function foo(x) { return x; }") {
        succOf("foo", NodeTypes.METHOD) shouldBe expected(("x", AlwaysEdge))
        succOf("x", NodeTypes.IDENTIFIER) shouldBe expected(("return x", AlwaysEdge))
        succOf("return x") shouldBe expected(("RET", AlwaysEdge))
      }
    }

    "be correct for consecutive return statements" in {
      new CfgFixture("function foo(x, y) { return x; return y; }") {
        succOf("foo", NodeTypes.METHOD) shouldBe expected(("x", AlwaysEdge))
        succOf("x", NodeTypes.IDENTIFIER) shouldBe expected(("return x", AlwaysEdge))
        succOf("y", NodeTypes.IDENTIFIER) shouldBe expected(("return y", AlwaysEdge))
        succOf("return x") shouldBe expected(("RET", AlwaysEdge))
        succOf("return y") shouldBe expected(("RET", AlwaysEdge))
      }
    }

    "be correct for outer program function which declares foo function object" in {
      new CfgFixture("function foo(x, y) { return; }") {
        succOf(":program") shouldBe expected(("foo", 1, AlwaysEdge))
        succOf("foo", NodeTypes.IDENTIFIER) shouldBe expected(("foo", 2, AlwaysEdge))
        succOf("foo", NodeTypes.METHOD_REF) shouldBe expected(
          ("function foo = function foo(x, y) { return; }", AlwaysEdge)
        )
        succOf("function foo = function foo(x, y) { return; }") shouldBe expected(("RET", AlwaysEdge))
      }
    }

    "be correct for void return statement" in {
      new CfgFixture("function foo() { return; }") {
        succOf("foo", NodeTypes.METHOD) shouldBe expected(("return", AlwaysEdge))
        succOf("return") shouldBe expected(("RET", AlwaysEdge))
      }
    }

    "be correct for call expression" in {
      new CfgFixture("foo(a + 1, b);") {
        succOf(":program") shouldBe expected(("foo", AlwaysEdge))
        succOf("foo") shouldBe expected(("this", AlwaysEdge))
        succOf("this", NodeTypes.IDENTIFIER) shouldBe expected(("a", AlwaysEdge))
        succOf("a") shouldBe expected(("1", AlwaysEdge))
        succOf("1") shouldBe expected(("a + 1", AlwaysEdge))
        succOf("a + 1") shouldBe expected(("b", AlwaysEdge))
        succOf("b") shouldBe expected(("foo(a + 1, b)", AlwaysEdge))
        succOf("foo(a + 1, b)") shouldBe expected(("RET", AlwaysEdge))
      }
    }

    "be correct for chained calls" ignore {
      // TODO the current style of writing this tests in unmaintainable.
      new CfgFixture("x.foo(y).bar(z)") {}
    }

    "be correct for unary expression '++'" in {
      new CfgFixture("x++") {
        succOf(":program") shouldBe expected(("x", AlwaysEdge))
        succOf("x") shouldBe expected(("x++", AlwaysEdge))
        succOf("x++") shouldBe expected(("RET", AlwaysEdge))
      }
    }

    "be correct for conditional expression" in {
      new CfgFixture("x ? y : z;") {
        succOf(":program") shouldBe expected(("x", AlwaysEdge))
        succOf("x") shouldBe expected(("y", TrueEdge), ("z", FalseEdge))
        succOf("y") shouldBe expected(("x ? y : z", AlwaysEdge))
        succOf("z") shouldBe expected(("x ? y : z", AlwaysEdge))
        succOf("x ? y : z") shouldBe expected(("RET", AlwaysEdge))
      }
    }

    // WHILE Loops
    "be correct for plain while loop" in {
      new CfgFixture("while (x < 1) { y = 2; }") {
        succOf(":program") shouldBe expected(("x", AlwaysEdge))
        succOf("x") shouldBe expected(("1", AlwaysEdge))
        succOf("1") shouldBe expected(("x < 1", AlwaysEdge))
        succOf("x < 1") shouldBe expected(("y", TrueEdge), ("RET", FalseEdge))
        succOf("y") shouldBe expected(("2", AlwaysEdge))
        succOf("2") shouldBe expected(("y = 2", AlwaysEdge))
        succOf("y = 2") shouldBe expected(("x", AlwaysEdge))
        succOf("x") shouldBe expected(("1", AlwaysEdge))
      }
    }

    "be correct for plain while loop with break" in {
      new CfgFixture("while (x < 1) { break; y; }") {
        succOf(":program") shouldBe expected(("x", AlwaysEdge))
        succOf("x") shouldBe expected(("1", AlwaysEdge))
        succOf("1") shouldBe expected(("x < 1", AlwaysEdge))
        succOf("x < 1") shouldBe expected(("break;", TrueEdge), ("RET", FalseEdge))
        succOf("break;") shouldBe expected(("RET", AlwaysEdge))
        succOf("y") shouldBe expected(("x", AlwaysEdge))
      }
    }

    "be correct for plain while loop with continue" in {
      new CfgFixture("while (x < 1) { continue; y; }") {
        succOf(":program") shouldBe expected(("x", AlwaysEdge))
        succOf("x") shouldBe expected(("1", AlwaysEdge))
        succOf("1") shouldBe expected(("x < 1", AlwaysEdge))
        succOf("x < 1") shouldBe expected(("continue;", TrueEdge), ("RET", FalseEdge))
        succOf("continue;") shouldBe expected(("x", AlwaysEdge))
        succOf("y") shouldBe expected(("x", AlwaysEdge))
      }
    }

    "be correct for nested while loop" in {
      new CfgFixture("while (x) {while(y) {z;}}") {
        succOf(":program") shouldBe expected(("x", AlwaysEdge))
        succOf("x") shouldBe expected(("y", TrueEdge), ("RET", FalseEdge))
        succOf("y") shouldBe expected(("z", TrueEdge), ("x", FalseEdge))
      }
    }

    "be correct for nested while loop with break" in {
      new CfgFixture("while (x) { while(y) { break; z;} a;} b;") {
        succOf(":program") shouldBe expected(("x", AlwaysEdge))
        succOf("x") shouldBe expected(("y", TrueEdge), ("b", FalseEdge))
        succOf("y") shouldBe expected(("break;", TrueEdge), ("a", FalseEdge))
        succOf("a") shouldBe expected(("x", AlwaysEdge))
        succOf("b") shouldBe expected(("RET", AlwaysEdge))
      }
    }

    "be correct for another nested while loop with break" in {
      new CfgFixture("while (x) { while(y) { break; z;} a; break; b; } c;") {
        succOf(":program") shouldBe expected(("x", AlwaysEdge))
        succOf("x") shouldBe expected(("y", TrueEdge), ("c", FalseEdge))
        succOf("y") shouldBe expected(("break;", TrueEdge), ("a", FalseEdge))
        succOf("break;", 0) shouldBe expected(("a", AlwaysEdge))
        succOf("a") shouldBe expected(("break;", 1, AlwaysEdge))
        succOf("break;", 1) shouldBe expected(("c", AlwaysEdge))
        succOf("c") shouldBe expected(("RET", AlwaysEdge))
      }
    }

    "nested while loop with conditional break" in {
      new CfgFixture(s"""
           |while (x) {
           |  if (y) {
           |	   break;
           |	 }
           |	 while (z) {
           |    break;
           |  }
           |}
           |n
        """.stripMargin) {
        succOf(":program") shouldBe expected(("x", AlwaysEdge))
        succOf("x") shouldBe expected(("y", TrueEdge), ("n", FalseEdge))
        succOf("y") shouldBe expected(("break;", TrueEdge), ("z", FalseEdge))
        succOf("break;", 0) shouldBe expected(("n", AlwaysEdge))
        succOf("break;", 1) shouldBe expected(("x", AlwaysEdge))
        succOf("z") shouldBe expected(("break;", 1, TrueEdge), ("x", FalseEdge))
        succOf("n") shouldBe expected(("RET", AlwaysEdge))
      }
    }

    // DO-WHILE Loops
    "be correct for plain do-while loop" in {
      new CfgFixture("do { y = 2; } while (x < 1);") {
        succOf(":program") shouldBe expected(("y", AlwaysEdge))
        succOf("y") shouldBe expected(("2", AlwaysEdge))
        succOf("2") shouldBe expected(("y = 2", AlwaysEdge))
        succOf("y = 2") shouldBe expected(("x", AlwaysEdge))
        succOf("x") shouldBe expected(("1", AlwaysEdge))
        succOf("1") shouldBe expected(("x < 1", AlwaysEdge))
        succOf("x < 1") shouldBe expected(("y", TrueEdge), ("RET", FalseEdge))
      }
    }

    "be correct for plain do-while loop with break" in {
      new CfgFixture("do { break; y; } while (x < 1);") {
        succOf(":program") shouldBe expected(("break;", AlwaysEdge))
        succOf("break;") shouldBe expected(("RET", AlwaysEdge))
        succOf("y") shouldBe expected(("x", AlwaysEdge))
        succOf("x") shouldBe expected(("1", AlwaysEdge))
        succOf("1") shouldBe expected(("x < 1", AlwaysEdge))
        succOf("x < 1") shouldBe expected(("break;", TrueEdge), ("RET", FalseEdge))
      }
    }

    "be correct for plain do-while loop with continue" in {
      new CfgFixture("do { continue; y; } while (x < 1);") {
        succOf(":program") shouldBe expected(("continue;", AlwaysEdge))
        succOf("continue;") shouldBe expected(("x", AlwaysEdge))
        succOf("y") shouldBe expected(("x", AlwaysEdge))
        succOf("x") shouldBe expected(("1", AlwaysEdge))
        succOf("1") shouldBe expected(("x < 1", AlwaysEdge))
        succOf("x < 1") shouldBe expected(("continue;", TrueEdge), ("RET", FalseEdge))
      }
    }

    "be correct for nested do-while loop with continue" in {
      new CfgFixture("do { do { x; } while (y); } while (z);") {
        succOf(":program") shouldBe expected(("x", AlwaysEdge))
        succOf("x") shouldBe expected(("y", AlwaysEdge))
        succOf("y") shouldBe expected(("x", TrueEdge), ("z", FalseEdge))
        succOf("z") shouldBe expected(("x", TrueEdge), ("RET", FalseEdge))
      }
    }

    "be correct for nested while/do-while loops with break" in {
      new CfgFixture("while (x) { do { while(y) { break; a; } z; } while (x < 1); } c;") {
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
    }

    "be correct for nested while/do-while loops with break and continue" in {
      new CfgFixture("while(x) { do { break; } while (y) } o;") {
        succOf(":program") shouldBe expected(("x", AlwaysEdge))
        succOf("x") shouldBe expected(("break;", TrueEdge), ("o", FalseEdge))
        succOf("break;") shouldBe expected(("x", AlwaysEdge))
        succOf("o") shouldBe expected(("RET", AlwaysEdge))
      }
    }

    "be correct for two nested while loop with inner break" in {
      new CfgFixture("while(y) { while(z) { break; x; } }") {
        succOf(":program") shouldBe expected(("y", AlwaysEdge))
        succOf("y") shouldBe expected(("z", TrueEdge), ("RET", FalseEdge))
        succOf("z") shouldBe expected(("break;", TrueEdge), ("y", FalseEdge))
        succOf("break;") shouldBe expected(("y", AlwaysEdge))
      }
    }

    // FOR Loops
    "be correct for plain for-loop" in {
      new CfgFixture("for (x = 0; y < 1; z += 2) { a = 3; }") {
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
    }

    "be correct for plain for-loop with break" in {
      new CfgFixture("for (x = 0; y < 1; z += 2) { break; a = 3; }") {
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
    }

    "be correct for plain for-loop with continue" in {
      new CfgFixture("for (x = 0; y < 1; z += 2) { continue; a = 3; }") {
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
    }

    "be correct for for-loop with for-in" in {
      new CfgFixture("""
          |for (var i in arr) {
          |   foo(i)
          |}
        """.stripMargin) {
        testForInOrOf()
      }
    }

    "be correct for for-loop with for-of" in {
      new CfgFixture("""
          |for (var i of arr) {
          |   foo(i)
          |}
        """.stripMargin) {
        testForInOrOf()
      }
    }

    "be correct for nested for-loop" in {
      new CfgFixture("for (x; y; z) { for (a; b; c) { u; } }") {
        succOf(":program") shouldBe expected(("x", AlwaysEdge))
        succOf("x") shouldBe expected(("y", AlwaysEdge))
        succOf("y") shouldBe expected(("a", TrueEdge), ("RET", FalseEdge))
        succOf("z") shouldBe expected(("y", AlwaysEdge))
        succOf("a") shouldBe expected(("b", AlwaysEdge))
        succOf("b") shouldBe expected(("u", TrueEdge), ("z", FalseEdge))
        succOf("c") shouldBe expected(("b", AlwaysEdge))
        succOf("u") shouldBe expected(("c", AlwaysEdge))
      }
    }

    "be correct for for-loop with empty condition" in {
      new CfgFixture("for (;;) { a = 1; }") {
        succOf(":program") shouldBe expected(("true", AlwaysEdge))
        succOf("true") shouldBe expected(("a", TrueEdge), ("RET", FalseEdge))
        succOf("a") shouldBe expected(("1", AlwaysEdge))
        succOf("1") shouldBe expected(("a = 1", AlwaysEdge))
        succOf("a = 1") shouldBe expected(("true", AlwaysEdge))
      }
    }

    "be correct for for-loop with empty condition and break" in {
      new CfgFixture("for (;;) { break; }") {
        succOf(":program") shouldBe expected(("true", AlwaysEdge))
        succOf("true") shouldBe expected(("break;", TrueEdge), ("RET", FalseEdge))
        succOf("break;") shouldBe expected(("RET", AlwaysEdge))
      }
    }

    "be correct for for-loop with empty condition and continue" in {
      new CfgFixture("for (;;) { continue; }") {
        succOf(":program") shouldBe expected(("true", AlwaysEdge))
        succOf("true") shouldBe expected(("continue;", TrueEdge), ("RET", FalseEdge))
        succOf("continue;") shouldBe expected(("true", AlwaysEdge))
      }
    }

    "be correct with empty condition with nested empty for-loop" in {
      new CfgFixture("for (;;) { for (;;) { x; } }") {
        succOf(":program") shouldBe expected(("true", AlwaysEdge))
        succOf("true") shouldBe expected(("true", 1, TrueEdge), ("RET", FalseEdge))
        succOf("true", 1) shouldBe expected(("x", TrueEdge), ("true", 0, FalseEdge))
        succOf("x") shouldBe expected(("true", 1, AlwaysEdge))
      }
    }

    "be correct for for-loop with empty block" in {
      new CfgFixture("for (;;) ;") {
        succOf(":program") shouldBe expected(("true", AlwaysEdge))
        succOf("true") shouldBe expected(("true", TrueEdge), ("RET", FalseEdge))
      }
    }

    // IF Statement
    "be correct for simple if statement" in {
      new CfgFixture("if (x) { y; }") {
        succOf(":program") shouldBe expected(("x", AlwaysEdge))
        succOf("x") shouldBe expected(("y", TrueEdge), ("RET", FalseEdge))
        succOf("y") shouldBe expected(("RET", AlwaysEdge))
      }
    }

    "be correct for simple if statement with else block" in {
      new CfgFixture("if (x) { y; } else { z; }") {
        succOf(":program") shouldBe expected(("x", AlwaysEdge))
        succOf("x") shouldBe expected(("y", TrueEdge), ("z", FalseEdge))
        succOf("y") shouldBe expected(("RET", AlwaysEdge))
        succOf("z") shouldBe expected(("RET", AlwaysEdge))
      }
    }

    "be correct for nested if statement" in {
      new CfgFixture("if (x) { if (y) { z; } }") {
        succOf(":program") shouldBe expected(("x", AlwaysEdge))
        succOf("x") shouldBe expected(("y", TrueEdge), ("RET", FalseEdge))
        succOf("y") shouldBe expected(("z", TrueEdge), ("RET", FalseEdge))
        succOf("z") shouldBe expected(("RET", AlwaysEdge))
      }
    }

    "be correct for nested if statement with else-if chains" in {
      new CfgFixture("if (a) { b; } else if (c) { d;} else { e; }") {
        succOf(":program") shouldBe expected(("a", AlwaysEdge))
        succOf("a") shouldBe expected(("b", TrueEdge), ("c", FalseEdge))
        succOf("b") shouldBe expected(("RET", AlwaysEdge))
        succOf("c") shouldBe expected(("d", TrueEdge), ("e", FalseEdge))
        succOf("d") shouldBe expected(("RET", AlwaysEdge))
        succOf("e") shouldBe expected(("RET", AlwaysEdge))
      }
    }

    "be correct for switch-case with single case" in {
      new CfgFixture("switch (x) { case 1: y;}") {
        succOf(":program") shouldBe expected(("x", AlwaysEdge))
        succOf("x") shouldBe expected(("case 1:", CaseEdge), ("RET", CaseEdge))
        succOf("case 1:") shouldBe expected(("1", AlwaysEdge))
        succOf("1") shouldBe expected(("y", AlwaysEdge))
        succOf("y") shouldBe expected(("RET", AlwaysEdge))
      }
    }

    "be correct for switch-case with multiple cases" in {
      new CfgFixture("switch (x) { case 1: y; case 2: z;}") {
        succOf(":program") shouldBe expected(("x", AlwaysEdge))
        succOf("x") shouldBe expected(("case 1:", CaseEdge), ("case 2:", CaseEdge), ("RET", CaseEdge))
        succOf("case 1:") shouldBe expected(("1", AlwaysEdge))
        succOf("1") shouldBe expected(("y", AlwaysEdge))
        succOf("y") shouldBe expected(("case 2:", AlwaysEdge))
        succOf("case 2:") shouldBe expected(("2", AlwaysEdge))
        succOf("2") shouldBe expected(("z", AlwaysEdge))
        succOf("z") shouldBe expected(("RET", AlwaysEdge))
      }
    }

    "be correct for switch-case with multiple cases on the same spot" in {
      new CfgFixture("switch (x) { case 1: case 2: y; }") {
        succOf(":program") shouldBe expected(("x", AlwaysEdge))
        succOf("x") shouldBe expected(("case 1:", CaseEdge), ("case 2:", CaseEdge), ("RET", CaseEdge))
        succOf("case 1:") shouldBe expected(("1", AlwaysEdge))
        succOf("1") shouldBe expected(("case 2:", AlwaysEdge))
        succOf("case 2:") shouldBe expected(("2", AlwaysEdge))
        succOf("2") shouldBe expected(("y", AlwaysEdge))
        succOf("y") shouldBe expected(("RET", AlwaysEdge))
      }
    }

    "be correct for switch-case with default case" in {
      new CfgFixture("switch (x) { default: y; }") {
        succOf(":program") shouldBe expected(("x", AlwaysEdge))
        succOf("x") shouldBe expected(("default:", CaseEdge))
        succOf("default:") shouldBe expected(("y", AlwaysEdge))
        succOf("y") shouldBe expected(("RET", AlwaysEdge))
      }
    }

    "be correct for switch-case with multiple cases and default combined" in {
      new CfgFixture("switch (x) { case 1: y; break; default: z;}") {
        succOf(":program") shouldBe expected(("x", AlwaysEdge))
        succOf("x") shouldBe expected(("case 1:", CaseEdge), ("default:", CaseEdge))
        succOf("case 1:") shouldBe expected(("1", AlwaysEdge))
        succOf("1") shouldBe expected(("y", AlwaysEdge))
        succOf("y") shouldBe expected(("break;", AlwaysEdge))
        succOf("break;") shouldBe expected(("RET", AlwaysEdge))
        succOf("default:") shouldBe expected(("z", AlwaysEdge))
        succOf("z") shouldBe expected(("RET", AlwaysEdge))
      }
    }

    "be correct for constructor call with new" in {
      new CfgFixture("""var x = new MyClass(arg1, arg2)""") {
        succOf(":program") shouldBe expected(("x", AlwaysEdge))
        succOf("x") shouldBe expected(("_tmp_0", AlwaysEdge))
        succOf("_tmp_0") shouldBe expected((".alloc", AlwaysEdge))
        succOf(".alloc") shouldBe expected(("_tmp_0 = .alloc", AlwaysEdge))
        succOf("_tmp_0 = .alloc") shouldBe expected(("MyClass", AlwaysEdge))
        succOf("MyClass") shouldBe expected(("_tmp_0", 1, AlwaysEdge))
        succOf("_tmp_0", 1) shouldBe expected(("arg1", AlwaysEdge))
        succOf("arg1") shouldBe expected(("arg2", AlwaysEdge))
        succOf("arg2") shouldBe expected(("MyClass(arg1, arg2)", AlwaysEdge))
        succOf("MyClass(arg1, arg2)") shouldBe expected(("_tmp_0", 2, AlwaysEdge))
        succOf("_tmp_0", 2) shouldBe expected(("x = new MyClass(arg1, arg2)", AlwaysEdge))
        succOf("x = new MyClass(arg1, arg2)") shouldBe expected(("RET", AlwaysEdge))
      }
    }
  }

  "CFG generation for destructing assignment" should {
    "be correct for object destruction assignment with declaration" in {
      new CfgFixture("var {a, b} = x") {
        succOf(":program") shouldBe expected(("_tmp_0", AlwaysEdge))
        succOf("_tmp_0") shouldBe expected(("x", AlwaysEdge))
        succOf("x") shouldBe expected(("_tmp_0 = x", AlwaysEdge))

        succOf("_tmp_0 = x") shouldBe expected(("a", AlwaysEdge))
        succOf("a") shouldBe expected(("_tmp_0", 1, AlwaysEdge))
        succOf("_tmp_0", 1) shouldBe expected(("a", 1, AlwaysEdge))
        succOf("a", 1) shouldBe expected(("_tmp_0.a", AlwaysEdge))
        succOf("_tmp_0.a") shouldBe expected(("a = _tmp_0.a", AlwaysEdge))

        succOf("a = _tmp_0.a") shouldBe expected(("b", AlwaysEdge))
        succOf("b") shouldBe expected(("_tmp_0", 2, AlwaysEdge))
        succOf("_tmp_0", 2) shouldBe expected(("b", 1, AlwaysEdge))
        succOf("b", 1) shouldBe expected(("_tmp_0.b", AlwaysEdge))
        succOf("_tmp_0.b") shouldBe expected(("b = _tmp_0.b", AlwaysEdge))
        succOf("b = _tmp_0.b") shouldBe expected(("_tmp_0", 3, AlwaysEdge))
        succOf("_tmp_0", 3) shouldBe expected(("RET", AlwaysEdge))
      }
    }

    "be correct for object destruction assignment with declaration and ternary init" in {
      new CfgFixture("const { a, b } = test() ? foo() : bar();") {
        succOf(":program") shouldBe expected(("_tmp_0", AlwaysEdge))
        succOf("_tmp_0") shouldBe expected(("test", AlwaysEdge))
        succOf("test") shouldBe expected(("this", AlwaysEdge))
        succOf("this", NodeTypes.IDENTIFIER) shouldBe expected(("test()", AlwaysEdge))
        succOf("test()") shouldBe expected(("foo", TrueEdge), ("bar", FalseEdge))
        succOf("foo") shouldBe expected(("this", 1, AlwaysEdge))
        succOf("this", 2) shouldBe expected(("foo()", AlwaysEdge))
        succOf("bar()") shouldBe expected(("test() ? foo() : bar()", AlwaysEdge))
        succOf("foo()") shouldBe expected(("test() ? foo() : bar()", AlwaysEdge))
        succOf("test() ? foo() : bar()") shouldBe expected(("_tmp_0 = test() ? foo() : bar()", AlwaysEdge))
        succOf("_tmp_0 = test() ? foo() : bar()") shouldBe expected(("a", AlwaysEdge))
        succOf("a") shouldBe expected(("_tmp_0", 1, AlwaysEdge))
        succOf("_tmp_0", 1) shouldBe expected(("a", 1, AlwaysEdge))
        succOf("a", 1) shouldBe expected(("_tmp_0.a", AlwaysEdge))
        succOf("_tmp_0.a") shouldBe expected(("a = _tmp_0.a", AlwaysEdge))
        succOf("a = _tmp_0.a") shouldBe expected(("b", AlwaysEdge))
        succOf("b") shouldBe expected(("_tmp_0", 2, AlwaysEdge))
        succOf("_tmp_0", 2) shouldBe expected(("b", 1, AlwaysEdge))
        succOf("b", 1) shouldBe expected(("_tmp_0.b", AlwaysEdge))
        succOf("_tmp_0.b") shouldBe expected(("b = _tmp_0.b", AlwaysEdge))
        succOf("b = _tmp_0.b") shouldBe expected(("_tmp_0", 3, AlwaysEdge))
        succOf("_tmp_0", 3) shouldBe expected(("RET", AlwaysEdge))
      }
    }

    "be correct for object destruction assignment with reassignment" in {
      new CfgFixture("var {a: n, b: m} = x") {
        succOf(":program") shouldBe expected(("_tmp_0", AlwaysEdge))
        succOf("_tmp_0") shouldBe expected(("x", AlwaysEdge))
        succOf("x") shouldBe expected(("_tmp_0 = x", AlwaysEdge))

        succOf("_tmp_0 = x") shouldBe expected(("n", AlwaysEdge))
        succOf("n") shouldBe expected(("_tmp_0", 1, AlwaysEdge))
        succOf("_tmp_0", 1) shouldBe expected(("a", AlwaysEdge))
        succOf("a") shouldBe expected(("_tmp_0.a", AlwaysEdge))
        succOf("_tmp_0.a") shouldBe expected(("n = _tmp_0.a", AlwaysEdge))

        succOf("n = _tmp_0.a") shouldBe expected(("m", AlwaysEdge))
        succOf("m") shouldBe expected(("_tmp_0", 2, AlwaysEdge))
        succOf("_tmp_0", 2) shouldBe expected(("b", AlwaysEdge))
        succOf("b") shouldBe expected(("_tmp_0.b", AlwaysEdge))
        succOf("_tmp_0.b") shouldBe expected(("m = _tmp_0.b", AlwaysEdge))
        succOf("m = _tmp_0.b") shouldBe expected(("_tmp_0", 3, AlwaysEdge))
        succOf("_tmp_0", 3) shouldBe expected(("RET", AlwaysEdge))
      }
    }

    "be correct for object destruction assignment with reassignment and defaults" in {
      new CfgFixture("var {a: n = 1, b: m = 2} = x") {
        succOf(":program") shouldBe expected(("_tmp_0", AlwaysEdge))
        succOf("_tmp_0") shouldBe expected(("x", AlwaysEdge))
        succOf("x") shouldBe expected(("_tmp_0 = x", AlwaysEdge))
        succOf("_tmp_0 = x") shouldBe expected(("n", AlwaysEdge))

        // test statement
        succOf("n") shouldBe expected(("_tmp_0", 1, AlwaysEdge))
        succOf("_tmp_0", 1) shouldBe expected(("a", AlwaysEdge))
        succOf("a") shouldBe expected(("_tmp_0.a", AlwaysEdge))
        succOf("_tmp_0.a") shouldBe expected(("void 0", AlwaysEdge))
        succOf("void 0") shouldBe expected(("_tmp_0.a === void 0", AlwaysEdge))

        // true, false cases
        succOf("_tmp_0.a === void 0") shouldBe expected(("1", TrueEdge), ("_tmp_0", 2, FalseEdge))
        succOf("_tmp_0", 2) shouldBe expected(("a", 1, AlwaysEdge))
        succOf("a", 1) shouldBe expected(("_tmp_0.a", 1, AlwaysEdge))
        succOf("_tmp_0.a", 1) shouldBe expected(("_tmp_0.a === void 0 ? 1 : _tmp_0.a", AlwaysEdge))
        succOf("1") shouldBe expected(("_tmp_0.a === void 0 ? 1 : _tmp_0.a", AlwaysEdge))
        succOf("_tmp_0.a === void 0 ? 1 : _tmp_0.a") shouldBe
          expected(("n = _tmp_0.a === void 0 ? 1 : _tmp_0.a", AlwaysEdge))
        succOf("n = _tmp_0.a === void 0 ? 1 : _tmp_0.a") shouldBe
          expected(("m", AlwaysEdge))

        // test statement
        succOf("m") shouldBe expected(("_tmp_0", 3, AlwaysEdge))
        succOf("_tmp_0", 3) shouldBe expected(("b", AlwaysEdge))
        succOf("b") shouldBe expected(("_tmp_0.b", AlwaysEdge))
        succOf("_tmp_0.b") shouldBe expected(("void 0", 1, AlwaysEdge))
        succOf("void 0", 1) shouldBe expected(("_tmp_0.b === void 0", AlwaysEdge))

        // true, false cases
        succOf("_tmp_0.b === void 0") shouldBe expected(("2", TrueEdge), ("_tmp_0", 4, FalseEdge))
        succOf("_tmp_0", 4) shouldBe expected(("b", 1, AlwaysEdge))
        succOf("b", 1) shouldBe expected(("_tmp_0.b", 1, AlwaysEdge))
        succOf("_tmp_0.b", 1) shouldBe expected(("_tmp_0.b === void 0 ? 2 : _tmp_0.b", AlwaysEdge))
        succOf("2") shouldBe expected(("_tmp_0.b === void 0 ? 2 : _tmp_0.b", AlwaysEdge))
        succOf("_tmp_0.b === void 0 ? 2 : _tmp_0.b") shouldBe
          expected(("m = _tmp_0.b === void 0 ? 2 : _tmp_0.b", AlwaysEdge))
        succOf("m = _tmp_0.b === void 0 ? 2 : _tmp_0.b") shouldBe
          expected(("_tmp_0", 5, AlwaysEdge))
        succOf("_tmp_0", 5) shouldBe expected(("RET", AlwaysEdge))
      }
    }

    "be correct for object destruction assignment with rest" in {
      new CfgFixture("var {a, ...rest} = x") {
        succOf(":program") shouldBe expected(("_tmp_0", AlwaysEdge))
        succOf("_tmp_0") shouldBe expected(("x", AlwaysEdge))
        succOf("x") shouldBe expected(("_tmp_0 = x", AlwaysEdge))

        succOf("_tmp_0 = x") shouldBe expected(("a", AlwaysEdge))
        succOf("a") shouldBe expected(("_tmp_0", 1, AlwaysEdge))
        succOf("_tmp_0", 1) shouldBe expected(("a", 1, AlwaysEdge))
        succOf("a", 1) shouldBe expected(("_tmp_0.a", AlwaysEdge))
        succOf("_tmp_0.a") shouldBe expected(("a = _tmp_0.a", AlwaysEdge))

        succOf("a = _tmp_0.a") shouldBe expected(("...rest", AlwaysEdge))
        succOf("...rest") shouldBe expected(("_tmp_0", 2, AlwaysEdge))
        succOf("_tmp_0", 2) shouldBe expected(("RET", AlwaysEdge))
      }
    }

    "be correct for object destruction assignment with computed property name" ignore {
      new CfgFixture("var {[propName]: n} = x") {}
    }

    "be correct for nested object destruction assignment with defaults as parameter" in {
      new CfgFixture("""
          |function userId({id = {}, b} = {}) {
          |  return id
          |}
          |""".stripMargin) {
        succOf("userId", NodeTypes.METHOD) shouldBe expected(("_tmp_1", AlwaysEdge))
        succOf("_tmp_1") shouldBe expected(("param1_0", AlwaysEdge))
        succOf("param1_0") shouldBe expected(("void 0", AlwaysEdge))
        succOf("void 0") shouldBe expected(("param1_0 === void 0", AlwaysEdge))
        succOf("param1_0 === void 0") shouldBe expected(
          ("_tmp_0", TrueEdge), // holds {}
          ("param1_0", 1, FalseEdge)
        )
        succOf("param1_0", 1) shouldBe expected(("param1_0 === void 0 ? {} : param1_0", AlwaysEdge))
        succOf("_tmp_0") shouldBe expected(("param1_0 === void 0 ? {} : param1_0", AlwaysEdge))
        succOf("param1_0 === void 0 ? {} : param1_0") shouldBe expected(
          ("_tmp_1 = param1_0 === void 0 ? {} : param1_0", AlwaysEdge)
        )
        succOf("_tmp_1 = param1_0 === void 0 ? {} : param1_0") shouldBe expected(("id", AlwaysEdge))
        succOf("id") shouldBe expected(("_tmp_1", 1, AlwaysEdge))
        succOf("_tmp_1", 1) shouldBe expected(("id", 1, AlwaysEdge))
        succOf("id", 1) shouldBe expected(("_tmp_1.id", AlwaysEdge))
        succOf("_tmp_1.id") shouldBe expected(("void 0", 1, AlwaysEdge))
        succOf("void 0", 1) shouldBe expected(("_tmp_1.id === void 0", AlwaysEdge))
        succOf("_tmp_1.id === void 0") shouldBe expected(
          ("_tmp_2", TrueEdge), // holds {}
          ("_tmp_1", 2, FalseEdge)
        )
        succOf("_tmp_2") shouldBe expected(("_tmp_1.id === void 0 ? {} : _tmp_1.id", AlwaysEdge))
        succOf("_tmp_1", 2) shouldBe expected(("id", 2, AlwaysEdge))

        succOf("_tmp_1.id === void 0 ? {} : _tmp_1.id") shouldBe expected(
          ("id = _tmp_1.id === void 0 ? {} : _tmp_1.id", AlwaysEdge)
        )
        succOf("id", 2) shouldBe expected(("_tmp_1.id", 1, AlwaysEdge))

        succOf("id = _tmp_1.id === void 0 ? {} : _tmp_1.id") shouldBe expected(("b", AlwaysEdge))

        succOf("b") shouldBe expected(("_tmp_1", 3, AlwaysEdge))
        succOf("_tmp_1", 3) shouldBe expected(("b", 1, AlwaysEdge))
        succOf("b", 1) shouldBe expected(("_tmp_1.b", AlwaysEdge))
        succOf("_tmp_1.b") shouldBe expected(("b = _tmp_1.b", AlwaysEdge))
        succOf("b = _tmp_1.b") shouldBe expected(("_tmp_1", 4, AlwaysEdge))
        succOf("_tmp_1", 4) shouldBe expected(("id", 3, AlwaysEdge))
      }
    }

    "be correct for object destruction assignment as parameter" in {
      new CfgFixture("""
         |function userId({id}) {
         |  return id
         |}
         |""".stripMargin) {
        succOf("userId", NodeTypes.METHOD) shouldBe expected(("id", AlwaysEdge))
        succOf("id") shouldBe expected(("param1_0", AlwaysEdge))
        succOf("param1_0") shouldBe expected(("id", 1, AlwaysEdge))
        succOf("id", 1) shouldBe expected(("param1_0.id", AlwaysEdge))
        succOf("param1_0.id") shouldBe expected(("id = param1_0.id", AlwaysEdge))
        succOf("id = param1_0.id") shouldBe expected(("id", 2, AlwaysEdge))
        succOf("id", 2) shouldBe expected(("return id", AlwaysEdge))
        succOf("return id") shouldBe expected(("RET", AlwaysEdge))
      }
    }

    "be correct for array destruction assignment with declaration" in {
      new CfgFixture("var [a, b] = x") {
        succOf(":program") shouldBe expected(("_tmp_0", AlwaysEdge))
        succOf("_tmp_0") shouldBe expected(("x", AlwaysEdge))
        succOf("x") shouldBe expected(("_tmp_0 = x", AlwaysEdge))

        succOf("_tmp_0 = x") shouldBe expected(("a", AlwaysEdge))

        succOf("a") shouldBe expected(("_tmp_0", 1, AlwaysEdge))
        succOf("_tmp_0", 1) shouldBe expected(("0", AlwaysEdge))
        succOf("0") shouldBe expected(("_tmp_0[0]", AlwaysEdge))
        succOf("_tmp_0[0]") shouldBe expected(("a = _tmp_0[0]", AlwaysEdge))

        succOf("a = _tmp_0[0]") shouldBe expected(("b", AlwaysEdge))
        succOf("b") shouldBe expected(("_tmp_0", 2, AlwaysEdge))
        succOf("_tmp_0", 2) shouldBe expected(("1", AlwaysEdge))
        succOf("1") shouldBe expected(("_tmp_0[1]", AlwaysEdge))
        succOf("_tmp_0[1]") shouldBe expected(("b = _tmp_0[1]", AlwaysEdge))
        succOf("b = _tmp_0[1]") shouldBe expected(("_tmp_0", 3, AlwaysEdge))
        succOf("_tmp_0", 3) shouldBe expected(("RET", AlwaysEdge))
      }
    }

    "be correct for array destruction assignment without declaration" in {
      new CfgFixture("[a, b] = x") {
        succOf(":program") shouldBe expected(("_tmp_0", AlwaysEdge))
        succOf("_tmp_0") shouldBe expected(("x", AlwaysEdge))
        succOf("x") shouldBe expected(("_tmp_0 = x", AlwaysEdge))

        succOf("_tmp_0 = x") shouldBe expected(("a", AlwaysEdge))

        succOf("a") shouldBe expected(("_tmp_0", 1, AlwaysEdge))
        succOf("_tmp_0", 1) shouldBe expected(("0", AlwaysEdge))
        succOf("0") shouldBe expected(("_tmp_0[0]", AlwaysEdge))
        succOf("_tmp_0[0]") shouldBe expected(("a = _tmp_0[0]", AlwaysEdge))

        succOf("a = _tmp_0[0]") shouldBe expected(("b", AlwaysEdge))
        succOf("b") shouldBe expected(("_tmp_0", 2, AlwaysEdge))
        succOf("_tmp_0", 2) shouldBe expected(("1", AlwaysEdge))
        succOf("1") shouldBe expected(("_tmp_0[1]", AlwaysEdge))
        succOf("_tmp_0[1]") shouldBe expected(("b = _tmp_0[1]", AlwaysEdge))
        succOf("b = _tmp_0[1]") shouldBe expected(("_tmp_0", 3, AlwaysEdge))
        succOf("_tmp_0", 3) shouldBe expected(("RET", AlwaysEdge))
      }
    }

    "be correct for array destruction assignment with defaults" in {
      new CfgFixture("var [a = 1, b = 2] = x") {
        succOf(":program") shouldBe expected(("_tmp_0", AlwaysEdge))
        succOf("_tmp_0") shouldBe expected(("x", AlwaysEdge))
        succOf("x") shouldBe expected(("_tmp_0 = x", AlwaysEdge))

        succOf("_tmp_0 = x") shouldBe expected(("a", AlwaysEdge))

        // test statement
        succOf("a") shouldBe expected(("_tmp_0", 1, AlwaysEdge))
        succOf("_tmp_0", 1) shouldBe expected(("0", AlwaysEdge))
        succOf("0") shouldBe expected(("_tmp_0[0]", AlwaysEdge))
        succOf("_tmp_0[0]") shouldBe expected(("void 0", AlwaysEdge))
        succOf("void 0") shouldBe expected(("_tmp_0[0] === void 0", AlwaysEdge))

        // true, false cases
        succOf("_tmp_0[0] === void 0") shouldBe expected(("1", TrueEdge), ("_tmp_0", 2, FalseEdge))
        succOf("_tmp_0", 2) shouldBe expected(("0", 1, AlwaysEdge))
        succOf("0", 1) shouldBe expected(("_tmp_0[0]", 1, AlwaysEdge))
        succOf("_tmp_0[0]", 1) shouldBe expected(("_tmp_0[0] === void 0 ? 1 : _tmp_0[0]", AlwaysEdge))
        succOf("_tmp_0[0] === void 0 ? 1 : _tmp_0[0]") shouldBe expected(
          ("a = _tmp_0[0] === void 0 ? 1 : _tmp_0[0]", AlwaysEdge)
        )
        succOf("a = _tmp_0[0] === void 0 ? 1 : _tmp_0[0]") shouldBe expected(("b", AlwaysEdge))

        // test statement
        succOf("b") shouldBe expected(("_tmp_0", 3, AlwaysEdge))
        succOf("_tmp_0", 3) shouldBe expected(("1", 1, AlwaysEdge))
        succOf("1", 1) shouldBe expected(("_tmp_0[1]", AlwaysEdge))
        succOf("_tmp_0[1]") shouldBe expected(("void 0", 1, AlwaysEdge))
        succOf("void 0", 1) shouldBe expected(("_tmp_0[1] === void 0", AlwaysEdge))

        // true, false cases
        succOf("_tmp_0[1] === void 0") shouldBe expected(("2", TrueEdge), ("_tmp_0", 4, FalseEdge))
        succOf("_tmp_0", 4) shouldBe expected(("1", 2, AlwaysEdge))
        succOf("1", 2) shouldBe expected(("_tmp_0[1]", 1, AlwaysEdge))
        succOf("_tmp_0[1]", 1) shouldBe expected(("_tmp_0[1] === void 0 ? 2 : _tmp_0[1]", AlwaysEdge))
        succOf("_tmp_0[1] === void 0 ? 2 : _tmp_0[1]") shouldBe expected(
          ("b = _tmp_0[1] === void 0 ? 2 : _tmp_0[1]", AlwaysEdge)
        )
        succOf("b = _tmp_0[1] === void 0 ? 2 : _tmp_0[1]") shouldBe expected(("_tmp_0", 5, AlwaysEdge))

        succOf("_tmp_0", 5) shouldBe expected(("RET", AlwaysEdge))
      }
    }

    "be correct for array destruction assignment with ignores" in {
      new CfgFixture("var [a, , b] = x") {
        succOf(":program") shouldBe expected(("_tmp_0", AlwaysEdge))
        succOf("_tmp_0") shouldBe expected(("x", AlwaysEdge))
        succOf("x") shouldBe expected(("_tmp_0 = x", AlwaysEdge))

        succOf("_tmp_0 = x") shouldBe expected(("a", AlwaysEdge))

        succOf("a") shouldBe expected(("_tmp_0", 1, AlwaysEdge))
        succOf("_tmp_0", 1) shouldBe expected(("0", AlwaysEdge))
        succOf("0") shouldBe expected(("_tmp_0[0]", AlwaysEdge))
        succOf("_tmp_0[0]") shouldBe expected(("a = _tmp_0[0]", AlwaysEdge))

        succOf("a = _tmp_0[0]") shouldBe expected(("b", AlwaysEdge))
        succOf("b") shouldBe expected(("_tmp_0", 2, AlwaysEdge))
        succOf("_tmp_0", 2) shouldBe expected(("2", AlwaysEdge))
        succOf("2") shouldBe expected(("_tmp_0[2]", AlwaysEdge))
        succOf("_tmp_0[2]") shouldBe expected(("b = _tmp_0[2]", AlwaysEdge))
        succOf("b = _tmp_0[2]") shouldBe expected(("_tmp_0", 3, AlwaysEdge))
        succOf("_tmp_0", 3) shouldBe expected(("RET", AlwaysEdge))
      }
    }

    "be correct for array destruction assignment with rest" ignore {
      new CfgFixture("var [a, ...rest] = x") {}
    }

    "be correct for array destruction assignment as parameter" in {
      new CfgFixture("""
          |function userId([id]) {
          |  return id
          |}
          |""".stripMargin) {
        succOf("userId", NodeTypes.METHOD) shouldBe expected(("id", AlwaysEdge))
        succOf("id") shouldBe expected(("param1_0", AlwaysEdge))
        succOf("param1_0") shouldBe expected(("id", 1, AlwaysEdge))
        succOf("id", 1) shouldBe expected(("param1_0.id", AlwaysEdge))
        succOf("param1_0.id") shouldBe expected(("id = param1_0.id", AlwaysEdge))
        succOf("id = param1_0.id") shouldBe expected(("id", 2, AlwaysEdge))
        succOf("id", 2) shouldBe expected(("return id", AlwaysEdge))
        succOf("return id") shouldBe expected(("RET", AlwaysEdge))
      }

    }
    // TODO nested cases as needed

    "CFG generation for spread arguments" should {
      "have correct structure for method spread argument" ignore {
        new CfgFixture("foo(...args))") {}
      }
    }

  }

  "CFG generation for await/async" should {
    "be correct for await/async" in {
      new CfgFixture("async function x(foo) { await foo() }") {
        succOf("x", NodeTypes.METHOD) shouldBe expected(("foo", AlwaysEdge))
        succOf("foo", NodeTypes.IDENTIFIER) shouldBe expected(("this", AlwaysEdge))
        succOf("this", NodeTypes.IDENTIFIER) shouldBe expected(("foo()", AlwaysEdge))
        succOf("foo()") shouldBe expected(("await foo()", AlwaysEdge))
        succOf("await foo()") shouldBe expected(("RET", AlwaysEdge))
      }
    }
  }

  "CFG generation for constructor" should {
    "be correct for simple new" in {
      new CfgFixture("new MyClass()") {
        succOf(":program") shouldBe expected(("_tmp_0", AlwaysEdge))
        succOf("_tmp_0") shouldBe expected((".alloc", AlwaysEdge))
        succOf(".alloc") shouldBe expected(("_tmp_0 = .alloc", AlwaysEdge))
        succOf("_tmp_0 = .alloc") shouldBe expected(("MyClass", AlwaysEdge))
        succOf("MyClass") shouldBe expected(("_tmp_0", 1, AlwaysEdge))
        succOf("_tmp_0", 1) shouldBe expected(("MyClass()", AlwaysEdge))
        succOf("MyClass()") shouldBe expected(("_tmp_0", 2, AlwaysEdge))
        succOf("_tmp_0", 2) shouldBe expected(("RET", AlwaysEdge))
      }
    }

    "be correct for simple new with arguments" in {
      new CfgFixture("new MyClass(arg1, arg2)") {
        succOf(":program") shouldBe expected(("_tmp_0", AlwaysEdge))
        succOf("_tmp_0") shouldBe expected((".alloc", AlwaysEdge))
        succOf(".alloc") shouldBe expected(("_tmp_0 = .alloc", AlwaysEdge))
        succOf("_tmp_0 = .alloc") shouldBe expected(("MyClass", AlwaysEdge))
        succOf("MyClass") shouldBe expected(("_tmp_0", 1, AlwaysEdge))
        succOf("_tmp_0", 1) shouldBe expected(("arg1", AlwaysEdge))
        succOf("arg1") shouldBe expected(("arg2", AlwaysEdge))
        succOf("arg2") shouldBe expected(("MyClass(arg1, arg2)", AlwaysEdge))
        succOf("MyClass(arg1, arg2)") shouldBe expected(("_tmp_0", 2, AlwaysEdge))
        succOf("_tmp_0", 2) shouldBe expected(("RET", AlwaysEdge))
      }
    }

    "be correct for new with access path" in {
      new CfgFixture("new foo.bar.MyClass()") {
        succOf(":program") shouldBe expected(("_tmp_0", AlwaysEdge))
        succOf("_tmp_0") shouldBe expected((".alloc", AlwaysEdge))
        succOf(".alloc") shouldBe expected(("_tmp_0 = .alloc", AlwaysEdge))
        succOf("_tmp_0 = .alloc") shouldBe expected(("foo", AlwaysEdge))
        succOf("foo") shouldBe expected(("bar", AlwaysEdge))
        succOf("bar") shouldBe expected(("foo.bar", AlwaysEdge))
        succOf("foo.bar") shouldBe expected(("MyClass", AlwaysEdge))
        succOf("MyClass") shouldBe expected(("foo.bar.MyClass", AlwaysEdge))
        succOf("foo.bar.MyClass") shouldBe expected(("_tmp_0", 1, AlwaysEdge))
        succOf("_tmp_0", 1) shouldBe expected(("foo.bar.MyClass()", AlwaysEdge))
        succOf("foo.bar.MyClass()") shouldBe expected(("_tmp_0", 2, AlwaysEdge))
        succOf("_tmp_0", 2) shouldBe expected(("RET", AlwaysEdge))
      }
    }

    "be structure for throw new exceptions" in {
      new CfgFixture("function foo() { throw new Foo() }") {
        succOf("foo", NodeTypes.METHOD) shouldBe expected(("_tmp_0", AlwaysEdge))
        succOf("_tmp_0") shouldBe expected((".alloc", AlwaysEdge))
        succOf(".alloc") shouldBe expected(("_tmp_0 = .alloc", AlwaysEdge))
        succOf("_tmp_0 = .alloc") shouldBe expected(("Foo", AlwaysEdge))
        succOf("Foo") shouldBe expected(("_tmp_0", 1, AlwaysEdge))
        succOf("_tmp_0", 1) shouldBe expected(("Foo()", AlwaysEdge))
        succOf("Foo()") shouldBe expected(("_tmp_0", 2, AlwaysEdge))
        succOf("_tmp_0", 2) shouldBe expected(("throw new Foo()", AlwaysEdge))
        succOf("throw new Foo()") shouldBe expected(("RET", AlwaysEdge))
      }
    }
  }

  "CFG generation for classes" should {
    "be correct for methods in class type decls" in {
      new CfgFixture("""
          |class ClassA {
          |  foo() {
          |    bar()
          |  }
          |}
          |""".stripMargin) {
        succOf("foo", NodeTypes.METHOD) shouldBe expected(("bar", AlwaysEdge))
        succOf("bar") shouldBe expected(("this", AlwaysEdge))
        succOf("this", NodeTypes.IDENTIFIER) shouldBe expected(("bar()", AlwaysEdge))
        succOf("bar()") shouldBe expected(("RET", 2, AlwaysEdge))
      }
    }

    "be correct for methods in class type decls with assignment" in {
      new CfgFixture("""
            |var a = class ClassA {
            |  foo() {
            |    bar()
            |  }
            |}
            |""".stripMargin) {
        succOf(":program") shouldBe expected(("a", AlwaysEdge))
        // call to constructor of ClassA
        succOf("a") shouldBe expected(("class ClassA", AlwaysEdge))
      }
    }

    "be correct for outer method of anonymous class declaration" in {
      new CfgFixture("var a = class {}") {
        succOf(":program") shouldBe expected(("a", AlwaysEdge))
        succOf("a") shouldBe expected(("class _anon_cdecl", AlwaysEdge))
        succOf("class _anon_cdecl") shouldBe expected(("a = class {}", AlwaysEdge))
        succOf("a = class {}") shouldBe expected(("RET", AlwaysEdge))
      }
    }
  }

  "CFG generation for instanceof/delete" should {
    "be correct for instanceof" in {
      new CfgFixture("x instanceof Foo") {
        succOf(":program") shouldBe expected(("x", AlwaysEdge))
        succOf("x") shouldBe expected(("Foo", AlwaysEdge))
        succOf("Foo") shouldBe expected(("x instanceof Foo", AlwaysEdge))
        succOf("x instanceof Foo", NodeTypes.CALL) shouldBe expected(("RET", AlwaysEdge))
      }
    }

    "be correct for delete" in {
      new CfgFixture("delete foo.x") {
        succOf(":program") shouldBe expected(("foo", AlwaysEdge))
        succOf("foo") shouldBe expected(("x", AlwaysEdge))
        succOf("x") shouldBe expected(("foo.x", AlwaysEdge))
        succOf("foo.x") shouldBe expected(("delete foo.x", AlwaysEdge))
        succOf("delete foo.x", NodeTypes.CALL) shouldBe expected(("RET", AlwaysEdge))
      }
    }
  }

  "CFG generation for default parameters" should {
    "be correct for method parameter with default" in {
      new CfgFixture("function foo(a = 1) { }") {
        succOf("foo", NodeTypes.METHOD) shouldBe expected(("a", AlwaysEdge))
        succOf("a", NodeTypes.IDENTIFIER) shouldBe expected(("a", 2, AlwaysEdge))
        succOf("a", 2) shouldBe expected(("void 0", AlwaysEdge))
        succOf("void 0") shouldBe expected(("a === void 0", AlwaysEdge))
        succOf("a === void 0") shouldBe expected(("1", TrueEdge), ("a", 2, FalseEdge))
        succOf("1") shouldBe expected(("a === void 0 ? 1 : a", AlwaysEdge))
        succOf("a", 3) shouldBe expected(("a === void 0 ? 1 : a", AlwaysEdge))
        succOf("a === void 0 ? 1 : a") shouldBe expected(("a = a === void 0 ? 1 : a", AlwaysEdge))
        succOf("a = a === void 0 ? 1 : a") shouldBe expected(("RET", AlwaysEdge))
      }
    }

    "be correct for multiple method parameters with default" in {
      new CfgFixture("function foo(a = 1, b = 2) { }") {
        succOf("foo", NodeTypes.METHOD) shouldBe expected(("a", AlwaysEdge))
        succOf("a", NodeTypes.IDENTIFIER) shouldBe expected(("a", 1, AlwaysEdge))
        succOf("a", 2) shouldBe expected(("void 0", AlwaysEdge))
        succOf("void 0") shouldBe expected(("a === void 0", AlwaysEdge))
        succOf("a === void 0") shouldBe expected(("1", TrueEdge), ("a", 2, FalseEdge))
        succOf("1") shouldBe expected(("a === void 0 ? 1 : a", AlwaysEdge))
        succOf("a", 3) shouldBe expected(("a === void 0 ? 1 : a", AlwaysEdge))
        succOf("a === void 0 ? 1 : a") shouldBe expected(("a = a === void 0 ? 1 : a", AlwaysEdge))
        succOf("a = a === void 0 ? 1 : a") shouldBe expected(("b", AlwaysEdge))

        succOf("b", NodeTypes.IDENTIFIER) shouldBe expected(("b", 1, AlwaysEdge))
        succOf("b", 2) shouldBe expected(("void 0", 1, AlwaysEdge))
        succOf("void 0", 1) shouldBe expected(("b === void 0", AlwaysEdge))
        succOf("b === void 0") shouldBe expected(("2", TrueEdge), ("b", 2, FalseEdge))
        succOf("2") shouldBe expected(("b === void 0 ? 2 : b", AlwaysEdge))
        succOf("b", 3) shouldBe expected(("b === void 0 ? 2 : b", AlwaysEdge))
        succOf("b === void 0 ? 2 : b") shouldBe expected(("b = b === void 0 ? 2 : b", AlwaysEdge))
        succOf("b = b === void 0 ? 2 : b") shouldBe expected(("RET", AlwaysEdge))
      }
    }

    "be correct for method mixed parameters with default" in {
      new CfgFixture("function foo(a, b = 1) { }") {
        succOf("foo", NodeTypes.METHOD) shouldBe expected(("b", AlwaysEdge))
        succOf("b", 1) shouldBe expected(("b", 2, AlwaysEdge))
        succOf("b", 2) shouldBe expected(("void 0", AlwaysEdge))
        succOf("void 0") shouldBe expected(("b === void 0", AlwaysEdge))
        succOf("b === void 0") shouldBe expected(("1", TrueEdge), ("b", 3, FalseEdge))
        succOf("1") shouldBe expected(("b === void 0 ? 1 : b", AlwaysEdge))
        succOf("b", 3) shouldBe expected(("b === void 0 ? 1 : b", AlwaysEdge))
        succOf("b === void 0 ? 1 : b") shouldBe expected(("b = b === void 0 ? 1 : b", AlwaysEdge))
        succOf("b = b === void 0 ? 1 : b") shouldBe expected(("RET", AlwaysEdge))
      }
    }

    "be correct for multiple method mixed parameters with default" in {
      new CfgFixture("function foo(x, a = 1, b = 2) { }") {
        succOf("foo", NodeTypes.METHOD) shouldBe expected(("a", AlwaysEdge))
        succOf("a", 1) shouldBe expected(("a", 2, AlwaysEdge))
        succOf("a", 2) shouldBe expected(("void 0", AlwaysEdge))
        succOf("void 0") shouldBe expected(("a === void 0", AlwaysEdge))
        succOf("a === void 0") shouldBe expected(("1", TrueEdge), ("a", 3, FalseEdge))
        succOf("1") shouldBe expected(("a === void 0 ? 1 : a", AlwaysEdge))
        succOf("a", 3) shouldBe expected(("a === void 0 ? 1 : a", AlwaysEdge))
        succOf("a === void 0 ? 1 : a") shouldBe expected(("a = a === void 0 ? 1 : a", AlwaysEdge))
        succOf("a = a === void 0 ? 1 : a") shouldBe expected(("b", AlwaysEdge))

        succOf("b", 1) shouldBe expected(("b", 2, AlwaysEdge))
        succOf("b", 2) shouldBe expected(("void 0", 1, AlwaysEdge))
        succOf("void 0", 1) shouldBe expected(("b === void 0", AlwaysEdge))
        succOf("b === void 0") shouldBe expected(("2", TrueEdge), ("b", 3, FalseEdge))
        succOf("2") shouldBe expected(("b === void 0 ? 2 : b", AlwaysEdge))
        succOf("b", 3) shouldBe expected(("b === void 0 ? 2 : b", AlwaysEdge))
        succOf("b === void 0 ? 2 : b") shouldBe expected(("b = b === void 0 ? 2 : b", AlwaysEdge))
        succOf("b = b === void 0 ? 2 : b") shouldBe expected(("RET", AlwaysEdge))
      }
    }
  }

  "CFG generation for global builtins" should {
    "be correct for JSON.parse" in {
      new CfgFixture("""JSON.parse("foo");""") {
        succOf(":program") shouldBe expected((""""foo"""", AlwaysEdge))
        succOf(""""foo"""") shouldBe expected(("""JSON.parse("foo")""", AlwaysEdge))
        succOf("""JSON.parse("foo")""") shouldBe expected(("RET", AlwaysEdge))
      }
    }

    "have correct structure for JSON.stringify" in {
      new CfgFixture("""JSON.stringify(foo);""") {
        succOf(":program") shouldBe expected(("foo", AlwaysEdge))
        succOf("foo") shouldBe expected(("JSON.stringify(foo)", AlwaysEdge))
        succOf("JSON.stringify(foo)") shouldBe expected(("RET", AlwaysEdge))
      }
    }
  }

  private class CfgFixture(code: String) {

    private var cpg: Cpg = Cpg.emptyCpg

    File.usingTemporaryDirectory("jssrc2cpgCfgTest") { workspace =>
      val file = workspace / "code.js"
      file.write(code)
      file.deleteOnExit()
      cpg = new JsSrc2CpgFrontend().execute(workspace.toJava)
      new CfgCreationPass(cpg).createAndApply()
    }

    private def matchCode(node: Node, code: String): Boolean = {
      node.label() match {
        // Method does not has a CODE property
        case NodeTypes.METHOD =>
          node.property(PropertyNames.NAME) == code
        case _ =>
          node.propertiesMap.asScala.exists { case (propertyKind, value) =>
            propertyKind == PropertyNames.CODE && value == code
          }
      }
    }

    // index is zero based and describes which node to take if multiple node match the code string.
    case class ExpectationInfo(code: String, index: Int, cfgEdgeKind: CfgEdgeType)

    implicit def toExpectationInfoShort(pair: (String, CfgEdgeType)): ExpectationInfo = {
      ExpectationInfo(pair._1, 0, pair._2)
    }

    implicit def toExpectationInfoFull(pair: (String, Int, CfgEdgeType)): ExpectationInfo = {
      ExpectationInfo(pair._1, pair._2, pair._3)
    }

    def expected(pairs: ExpectationInfo*): Set[String] = {
      pairs.map { case ExpectationInfo(code, index, _) =>
        val node = cpg.method.ast.isCfgNode.toVector
          .collect {
            case node if matchCode(node, code) => node
          }
          .lift(index)
          .getOrElse(fail(s"No node found for code = '$code' and index '$index'!"))
        node.property(PropertyNames.CODE).toString
      }.toSet
    }

    def succOf(code: String): Set[String] = {
      succOf(code, 0)
    }

    // index is zero based and describes which node to take if multiple node match the code string.
    def succOf(code: String, index: Int): Set[String] = {
      val nodes = cpg.method.ast.isCfgNode.toSeq
        .collect {
          case node if matchCode(node, code) => node
        }
      val node = nodes
        .lift(index)
        .getOrElse(fail(s"No node found for code = '$code' and index '$index'!"))
      val successors =
        node._cfgOut.map(_.property(PropertyNames.CODE).toString)
      successors.toSetImmutable
    }

    def succOf(code: String, nodeType: String): Set[String] = {
      val nodes = cpg.method.ast.isCfgNode
        .label(nodeType)
        .toVector
        .collectFirst {
          case node if matchCode(node, code) => node
        }
      val node = nodes
        .getOrElse(fail(s"No node found for code = '$code' and type '$nodeType'!"))
      val successors =
        node._cfgOut.map(_.property(PropertyNames.CODE).toString)
      successors.toSetImmutable
    }

    def testForInOrOf(): Unit = {
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
}
