package io.joern.jssrc2cpg.passes.cfg

import io.joern.x2cpg.passes.controlflow.cfgcreation.Cfg.AlwaysEdge
import io.shiftleft.codepropertygraph.generated.NodeTypes

class JsClassesCfgCreationPassTest extends AbstractCfgPassTest {

  "CFG generation for constructor" should {
    "be correct for simple new" in CfgFixture("new MyClass()") { implicit cpg =>
      succOf(":program") shouldBe expected(("_tmp_0", AlwaysEdge))
      succOf("_tmp_0") shouldBe expected((".alloc", AlwaysEdge))
      succOf(".alloc") shouldBe expected(("_tmp_0 = .alloc", AlwaysEdge))
      succOf("_tmp_0 = .alloc") shouldBe expected(("MyClass", AlwaysEdge))
      succOf("MyClass") shouldBe expected(("_tmp_0", 1, AlwaysEdge))
      succOf("_tmp_0", 1) shouldBe expected(("MyClass()", AlwaysEdge))
      succOf("MyClass()") shouldBe expected(("_tmp_0", 2, AlwaysEdge))
      succOf("_tmp_0", 2) shouldBe expected(("RET", AlwaysEdge))
    }

    "be correct for simple new with arguments" in CfgFixture("new MyClass(arg1, arg2)") { implicit cpg =>
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

    "be correct for new with access path" in CfgFixture("new foo.bar.MyClass()") { implicit cpg =>
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

    "be structure for throw new exceptions" in CfgFixture("function foo() { throw new Foo() }") { implicit cpg =>
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

  "CFG generation for classes" should {
    "be correct for methods in class type decls" in CfgFixture("""
       |class ClassA {
       |  foo() {
       |    bar()
       |  }
       |}
       |""".stripMargin) { implicit cpg =>
      succOf("foo", NodeTypes.METHOD) shouldBe expected(("bar", AlwaysEdge))
      succOf("bar") shouldBe expected(("this", AlwaysEdge))
      succOf("this", NodeTypes.IDENTIFIER) shouldBe expected(("bar()", AlwaysEdge))
      succOf("bar()") shouldBe expected(("RET", 2, AlwaysEdge))
    }

    "be correct for methods in class type decls with assignment" in CfgFixture("""
       |var a = class ClassA {
       |  foo() {
       |    bar()
       |  }
       |}
       |""".stripMargin) { implicit cpg =>
      succOf(":program") shouldBe expected(("a", AlwaysEdge))
      // call to constructor of ClassA
      succOf("a") shouldBe expected(("class ClassA", AlwaysEdge))
    }

    "be correct for outer method of anonymous class declaration" in CfgFixture("var a = class {}") { implicit cpg =>
      succOf(":program") shouldBe expected(("a", AlwaysEdge))
      succOf("a") shouldBe expected(("class _anon_cdecl", AlwaysEdge))
      succOf("class _anon_cdecl") shouldBe expected(("a = class {}", AlwaysEdge))
      succOf("a = class {}") shouldBe expected(("RET", AlwaysEdge))
    }
  }

}
