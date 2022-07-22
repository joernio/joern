package io.joern.jssrc2cpg.passes.cfg

import io.joern.x2cpg.passes.controlflow.cfgcreation.Cfg.AlwaysEdge
import io.joern.x2cpg.passes.controlflow.cfgcreation.Cfg.FalseEdge
import io.joern.x2cpg.passes.controlflow.cfgcreation.Cfg.TrueEdge
import io.shiftleft.codepropertygraph.generated.NodeTypes

class MixedCfgCreationPassTest extends AbstractCfgPassTest {

  "CFG generation for destructing assignment" should {
    "be correct for object destruction assignment with declaration" in CfgFixture("var {a, b} = x") { implicit cpg =>
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

    "be correct for object destruction assignment with declaration and ternary init" in CfgFixture(
      "const { a, b } = test() ? foo() : bar();"
    ) { implicit cpg =>
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

    "be correct for object destruction assignment with reassignment" in CfgFixture("var {a: n, b: m} = x") {
      implicit cpg =>
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

    "be correct for object destruction assignment with reassignment and defaults" in CfgFixture(
      "var {a: n = 1, b: m = 2} = x"
    ) { implicit cpg =>
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

    "be correct for object destruction assignment with rest" in CfgFixture("var {a, ...rest} = x") { implicit cpg =>
      succOf(":program") shouldBe expected(("_tmp_0", AlwaysEdge))
      succOf("_tmp_0") shouldBe expected(("x", AlwaysEdge))
      succOf("x") shouldBe expected(("_tmp_0 = x", AlwaysEdge))

      succOf("_tmp_0 = x") shouldBe expected(("a", AlwaysEdge))
      succOf("a") shouldBe expected(("_tmp_0", 1, AlwaysEdge))
      succOf("_tmp_0", 1) shouldBe expected(("a", 1, AlwaysEdge))
      succOf("a", 1) shouldBe expected(("_tmp_0.a", AlwaysEdge))
      succOf("_tmp_0.a") shouldBe expected(("a = _tmp_0.a", AlwaysEdge))

      succOf("a = _tmp_0.a") shouldBe expected(("rest", AlwaysEdge))
      succOf("rest") shouldBe expected(("_tmp_0", 2, AlwaysEdge))
      succOf("_tmp_0", 2) shouldBe expected(("rest", 1, AlwaysEdge))
      succOf("rest", 1) shouldBe expected(("_tmp_0.rest", AlwaysEdge))
      succOf("_tmp_0.rest") shouldBe expected(("rest = _tmp_0.rest", AlwaysEdge))
      succOf("rest = _tmp_0.rest") shouldBe expected(("_tmp_0", 3, AlwaysEdge))

      succOf("_tmp_0", 3) shouldBe expected(("RET", AlwaysEdge))
    }

    "be correct for object destruction assignment with computed property name" ignore CfgFixture(
      "var {[propName]: n} = x"
    ) { _ => }

    "be correct for nested object destruction assignment with defaults as parameter" in CfgFixture("""
       |function userId({id = {}, b} = {}) {
       |  return id
       |}
       |""".stripMargin) { implicit cpg =>
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

    "be correct for object destruction assignment as parameter" in CfgFixture("""
        |function userId({id}) {
        |  return id
        |}
        |""".stripMargin) { implicit cpg =>
      succOf("userId", NodeTypes.METHOD) shouldBe expected(("id", AlwaysEdge))
      succOf("id") shouldBe expected(("param1_0", AlwaysEdge))
      succOf("param1_0") shouldBe expected(("id", 1, AlwaysEdge))
      succOf("id", 1) shouldBe expected(("param1_0.id", AlwaysEdge))
      succOf("param1_0.id") shouldBe expected(("id = param1_0.id", AlwaysEdge))
      succOf("id = param1_0.id") shouldBe expected(("id", 2, AlwaysEdge))
      succOf("id", 2) shouldBe expected(("return id", AlwaysEdge))
      succOf("return id") shouldBe expected(("RET", AlwaysEdge))
    }

    "be correct for array destruction assignment with declaration" in CfgFixture("var [a, b] = x") { implicit cpg =>
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

    "be correct for array destruction assignment without declaration" in CfgFixture("[a, b] = x") { implicit cpg =>
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

    "be correct for array destruction assignment with defaults" in CfgFixture("var [a = 1, b = 2] = x") {
      implicit cpg =>
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

    "be correct for array destruction assignment with ignores" in CfgFixture("var [a, , b] = x") { implicit cpg =>
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

    "be correct for array destruction assignment with rest" in CfgFixture("var [a, ...rest] = x") { implicit cpg =>
      succOf(":program") shouldBe expected(("_tmp_0", AlwaysEdge))
      succOf("_tmp_0") shouldBe expected(("x", AlwaysEdge))
      succOf("x") shouldBe expected(("_tmp_0 = x", AlwaysEdge))

      succOf("_tmp_0 = x") shouldBe expected(("a", AlwaysEdge))

      succOf("a") shouldBe expected(("_tmp_0", 1, AlwaysEdge))
      succOf("_tmp_0", 1) shouldBe expected(("0", AlwaysEdge))
      succOf("0") shouldBe expected(("_tmp_0[0]", AlwaysEdge))
      succOf("_tmp_0[0]") shouldBe expected(("a = _tmp_0[0]", AlwaysEdge))

      succOf("a = _tmp_0[0]") shouldBe expected(("rest", AlwaysEdge))
      succOf("rest") shouldBe expected(("_tmp_0", 2, AlwaysEdge))
      succOf("_tmp_0", 2) shouldBe expected(("1", AlwaysEdge))
      succOf("1") shouldBe expected(("_tmp_0[1]", AlwaysEdge))
      succOf("_tmp_0[1]") shouldBe expected(("rest = _tmp_0[1]", AlwaysEdge))
      succOf("rest = _tmp_0[1]") shouldBe expected(("_tmp_0", 3, AlwaysEdge))
      succOf("_tmp_0", 3) shouldBe expected(("RET", AlwaysEdge))
    }

    "be correct for array destruction assignment as parameter" in CfgFixture("""
       |function userId([id]) {
       |  return id
       |}
       |""".stripMargin) { implicit cpg =>
      succOf("userId", NodeTypes.METHOD) shouldBe expected(("id", AlwaysEdge))
      succOf("id") shouldBe expected(("param1_0", AlwaysEdge))
      succOf("param1_0") shouldBe expected(("id", 1, AlwaysEdge))
      succOf("id", 1) shouldBe expected(("param1_0.id", AlwaysEdge))
      succOf("param1_0.id") shouldBe expected(("id = param1_0.id", AlwaysEdge))
      succOf("id = param1_0.id") shouldBe expected(("id", 2, AlwaysEdge))
      succOf("id", 2) shouldBe expected(("return id", AlwaysEdge))
      succOf("return id") shouldBe expected(("RET", AlwaysEdge))
    }

    "CFG generation for spread arguments" should {
      "have correct structure for method spread argument" in CfgFixture("foo(...args)") { implicit cpg =>
        succOf(":program") shouldBe expected(("foo", AlwaysEdge))
        succOf("foo") shouldBe expected(("this", AlwaysEdge))
        succOf("this", NodeTypes.IDENTIFIER) shouldBe expected(("...args", AlwaysEdge))
        succOf("...args") shouldBe expected(("foo(...args)", AlwaysEdge))
        succOf("foo(...args)") shouldBe expected(("RET", AlwaysEdge))
      }
    }

  }

  "CFG generation for await/async" should {
    "be correct for await/async" in CfgFixture("async function x(foo) { await foo() }") { implicit cpg =>
      succOf("x", NodeTypes.METHOD) shouldBe expected(("foo", AlwaysEdge))
      succOf("foo", NodeTypes.IDENTIFIER) shouldBe expected(("this", AlwaysEdge))
      succOf("this", NodeTypes.IDENTIFIER) shouldBe expected(("foo()", AlwaysEdge))
      succOf("foo()") shouldBe expected(("await foo()", AlwaysEdge))
      succOf("await foo()") shouldBe expected(("RET", AlwaysEdge))
    }
  }

  "CFG generation for instanceof/delete" should {
    "be correct for instanceof" in CfgFixture("x instanceof Foo") { implicit cpg =>
      succOf(":program") shouldBe expected(("x", AlwaysEdge))
      succOf("x") shouldBe expected(("Foo", AlwaysEdge))
      succOf("Foo") shouldBe expected(("x instanceof Foo", AlwaysEdge))
      succOf("x instanceof Foo", NodeTypes.CALL) shouldBe expected(("RET", AlwaysEdge))
    }

    "be correct for delete" in CfgFixture("delete foo.x") { implicit cpg =>
      succOf(":program") shouldBe expected(("foo", AlwaysEdge))
      succOf("foo") shouldBe expected(("x", AlwaysEdge))
      succOf("x") shouldBe expected(("foo.x", AlwaysEdge))
      succOf("foo.x") shouldBe expected(("delete foo.x", AlwaysEdge))
      succOf("delete foo.x", NodeTypes.CALL) shouldBe expected(("RET", AlwaysEdge))
    }
  }

  "CFG generation for default parameters" should {
    "be correct for method parameter with default" in CfgFixture("function foo(a = 1) { }") { implicit cpg =>
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

    "be correct for multiple method parameters with default" in CfgFixture("function foo(a = 1, b = 2) { }") {
      implicit cpg =>
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

    "be correct for method mixed parameters with default" in CfgFixture("function foo(a, b = 1) { }") { implicit cpg =>
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

    "be correct for multiple method mixed parameters with default" in CfgFixture("function foo(x, a = 1, b = 2) { }") {
      implicit cpg =>
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
