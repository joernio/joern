package io.joern.jssrc2cpg.passes.cfg

import io.joern.jssrc2cpg.testfixtures.JsSrcCfgTestCpg
import io.joern.x2cpg.passes.controlflow.cfgcreation.Cfg.AlwaysEdge
import io.joern.x2cpg.testfixtures.CfgTestFixture
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.NodeTypes

class JsClassesCfgCreationPassTests extends CfgTestFixture(() => new JsSrcCfgTestCpg()) {

  "CFG generation for constructor" should {
    "be correct for simple new" in {
      implicit val cpg: Cpg = code("new MyClass()")
      succOf(":program") shouldBe expected(("_tmp_0", AlwaysEdge))
      succOf("_tmp_0") shouldBe expected((".alloc", AlwaysEdge))
      succOf(".alloc") shouldBe expected(("_tmp_0 = .alloc", AlwaysEdge))
      succOf("_tmp_0 = .alloc") shouldBe expected(("MyClass", AlwaysEdge))
      succOf("MyClass") shouldBe expected(("_tmp_0", 1, AlwaysEdge))
      succOf("_tmp_0", 1) shouldBe expected(("new MyClass()", AlwaysEdge))
      succOf("new MyClass()", NodeTypes.CALL) shouldBe expected(("_tmp_0", 2, AlwaysEdge))
      succOf("_tmp_0", 2) shouldBe expected(("new MyClass()", AlwaysEdge))
      succOf("new MyClass()") shouldBe expected(("RET", AlwaysEdge))
    }

    "be correct for simple new with arguments" in {
      implicit val cpg: Cpg = code("new MyClass(arg1, arg2)")
      succOf(":program") shouldBe expected(("_tmp_0", AlwaysEdge))
      succOf("_tmp_0") shouldBe expected((".alloc", AlwaysEdge))
      succOf(".alloc") shouldBe expected(("_tmp_0 = .alloc", AlwaysEdge))
      succOf("_tmp_0 = .alloc") shouldBe expected(("MyClass", AlwaysEdge))
      succOf("MyClass") shouldBe expected(("_tmp_0", 1, AlwaysEdge))
      succOf("_tmp_0", 1) shouldBe expected(("arg1", AlwaysEdge))
      succOf("arg1") shouldBe expected(("arg2", AlwaysEdge))
      succOf("arg2") shouldBe expected(("new MyClass(arg1, arg2)", AlwaysEdge))
      succOf("new MyClass(arg1, arg2)", NodeTypes.CALL) shouldBe expected(("_tmp_0", 2, AlwaysEdge))
      succOf("_tmp_0", 2) shouldBe expected(("new MyClass(arg1, arg2)", AlwaysEdge))
      succOf("new MyClass(arg1, arg2)") shouldBe expected(("RET", AlwaysEdge))
    }

    "be correct for new with access path" in {
      implicit val cpg: Cpg = code("new foo.bar.MyClass()")
      succOf(":program") shouldBe expected(("_tmp_0", AlwaysEdge))
      succOf("_tmp_0") shouldBe expected((".alloc", AlwaysEdge))
      succOf(".alloc") shouldBe expected(("_tmp_0 = .alloc", AlwaysEdge))
      succOf("_tmp_0 = .alloc") shouldBe expected(("foo", AlwaysEdge))
      succOf("foo") shouldBe expected(("bar", AlwaysEdge))
      succOf("bar") shouldBe expected(("foo.bar", AlwaysEdge))
      succOf("foo.bar") shouldBe expected(("MyClass", AlwaysEdge))
      succOf("MyClass") shouldBe expected(("foo.bar.MyClass", AlwaysEdge))
      succOf("foo.bar.MyClass") shouldBe expected(("_tmp_0", 1, AlwaysEdge))
      succOf("_tmp_0", 1) shouldBe expected(("new foo.bar.MyClass()", AlwaysEdge))
      succOf("new foo.bar.MyClass()", NodeTypes.CALL) shouldBe expected(("_tmp_0", 2, AlwaysEdge))
      succOf("_tmp_0", 2) shouldBe expected(("new foo.bar.MyClass()", AlwaysEdge))
      succOf("new foo.bar.MyClass()") shouldBe expected(("RET", AlwaysEdge))
    }

    "be structure for throw new exceptions" in {
      implicit val cpg: Cpg = code("function foo() { throw new Foo() }")
      succOf("foo", NodeTypes.METHOD) shouldBe expected(("_tmp_0", AlwaysEdge))
      succOf("_tmp_0") shouldBe expected((".alloc", AlwaysEdge))
      succOf(".alloc") shouldBe expected(("_tmp_0 = .alloc", AlwaysEdge))
      succOf("_tmp_0 = .alloc") shouldBe expected(("Foo", AlwaysEdge))
      succOf("Foo") shouldBe expected(("_tmp_0", 1, AlwaysEdge))
      succOf("_tmp_0", 1) shouldBe expected(("new Foo()", AlwaysEdge))
      succOf("new Foo()", NodeTypes.CALL) shouldBe expected(("_tmp_0", 2, AlwaysEdge))
      succOf("_tmp_0", 2) shouldBe expected(("new Foo()", AlwaysEdge))
      succOf("new Foo()") shouldBe expected(("throw new Foo()", AlwaysEdge))
      succOf("throw new Foo()") shouldBe expected(("RET", AlwaysEdge))
    }
  }

  "CFG generation for classes" should {
    "be correct for methods in class type decls" in {
      implicit val cpg: Cpg = code("""
                                     |class ClassA {
                                     |  foo() {
                                     |    bar()
                                     |  }
                                     |}
                                     |""".stripMargin)
      succOf("foo", NodeTypes.METHOD) shouldBe expected(("bar", AlwaysEdge))
      succOf("bar") shouldBe expected(("this", AlwaysEdge))
      succOf("this", NodeTypes.IDENTIFIER) shouldBe expected(("bar()", AlwaysEdge))
      succOf("bar()") shouldBe expected(("RET", 2, AlwaysEdge))
    }

    "be correct for methods in class type decls with assignment" in {
      implicit val cpg: Cpg = code("""
                                     |var a = class ClassA {
                                     |  foo() {
                                     |    bar()
                                     |  }
                                     |}
                                     |""".stripMargin)
      succOf(":program") shouldBe expected(("a", AlwaysEdge))
      // call to constructor of ClassA
      succOf("a") shouldBe expected(("class ClassA", AlwaysEdge))
    }

    "be correct for outer method of anonymous class declaration" in {
      implicit val cpg: Cpg = code("var a = class {}")
      succOf(":program") shouldBe expected(("a", AlwaysEdge))
      succOf("a") shouldBe expected(("class <anon-class>0", AlwaysEdge))
      succOf("class <anon-class>0") shouldBe expected(("var a = class {}", AlwaysEdge))
      succOf("var a = class {}") shouldBe expected(("RET", AlwaysEdge))
    }
  }

}
