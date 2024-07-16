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
      succOf(":program") should contain theSameElementsAs expected(("_tmp_0", AlwaysEdge))
      succOf("_tmp_0") should contain theSameElementsAs expected((".alloc", AlwaysEdge))
      succOf(".alloc") should contain theSameElementsAs expected(("_tmp_0 = .alloc", AlwaysEdge))
      succOf("_tmp_0 = .alloc") should contain theSameElementsAs expected(("MyClass", AlwaysEdge))
      succOf("MyClass") should contain theSameElementsAs expected(("_tmp_0", 1, AlwaysEdge))
      succOf("_tmp_0", 1) should contain theSameElementsAs expected(("new MyClass()", AlwaysEdge))
      succOf("new MyClass()", NodeTypes.CALL) should contain theSameElementsAs expected(("_tmp_0", 2, AlwaysEdge))
      succOf("_tmp_0", 2) should contain theSameElementsAs expected(("new MyClass()", AlwaysEdge))
      succOf("new MyClass()") should contain theSameElementsAs expected(("RET", AlwaysEdge))
    }

    "be correct for simple new with arguments" in {
      implicit val cpg: Cpg = code("new MyClass(arg1, arg2)")
      succOf(":program") should contain theSameElementsAs expected(("_tmp_0", AlwaysEdge))
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
      succOf("new MyClass(arg1, arg2)") should contain theSameElementsAs expected(("RET", AlwaysEdge))
    }

    "be correct for new with access path" in {
      implicit val cpg: Cpg = code("new foo.bar.MyClass()")
      succOf(":program") should contain theSameElementsAs expected(("_tmp_0", AlwaysEdge))
      succOf("_tmp_0") should contain theSameElementsAs expected((".alloc", AlwaysEdge))
      succOf(".alloc") should contain theSameElementsAs expected(("_tmp_0 = .alloc", AlwaysEdge))
      succOf("_tmp_0 = .alloc") should contain theSameElementsAs expected(("foo", AlwaysEdge))
      succOf("foo") should contain theSameElementsAs expected(("bar", AlwaysEdge))
      succOf("bar") should contain theSameElementsAs expected(("foo.bar", AlwaysEdge))
      succOf("foo.bar") should contain theSameElementsAs expected(("MyClass", AlwaysEdge))
      succOf("MyClass") should contain theSameElementsAs expected(("foo.bar.MyClass", AlwaysEdge))
      succOf("foo.bar.MyClass") should contain theSameElementsAs expected(("_tmp_0", 1, AlwaysEdge))
      succOf("_tmp_0", 1) should contain theSameElementsAs expected(("new foo.bar.MyClass()", AlwaysEdge))
      succOf("new foo.bar.MyClass()", NodeTypes.CALL) should contain theSameElementsAs expected(
        ("_tmp_0", 2, AlwaysEdge)
      )
      succOf("_tmp_0", 2) should contain theSameElementsAs expected(("new foo.bar.MyClass()", AlwaysEdge))
      succOf("new foo.bar.MyClass()") should contain theSameElementsAs expected(("RET", AlwaysEdge))
    }

    "be structure for throw new exceptions" in {
      implicit val cpg: Cpg = code("function foo() { throw new Foo() }")
      succOf("foo", NodeTypes.METHOD) should contain theSameElementsAs expected(("_tmp_0", AlwaysEdge))
      succOf("_tmp_0") should contain theSameElementsAs expected((".alloc", AlwaysEdge))
      succOf(".alloc") should contain theSameElementsAs expected(("_tmp_0 = .alloc", AlwaysEdge))
      succOf("_tmp_0 = .alloc") should contain theSameElementsAs expected(("Foo", AlwaysEdge))
      succOf("Foo") should contain theSameElementsAs expected(("_tmp_0", 1, AlwaysEdge))
      succOf("_tmp_0", 1) should contain theSameElementsAs expected(("new Foo()", AlwaysEdge))
      succOf("new Foo()", NodeTypes.CALL) should contain theSameElementsAs expected(("_tmp_0", 2, AlwaysEdge))
      succOf("_tmp_0", 2) should contain theSameElementsAs expected(("new Foo()", AlwaysEdge))
      succOf("new Foo()") should contain theSameElementsAs expected(("throw new Foo()", AlwaysEdge))
      succOf("throw new Foo()") should contain theSameElementsAs expected(("RET", AlwaysEdge))
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
      succOf("foo", NodeTypes.METHOD) should contain theSameElementsAs expected(("bar", AlwaysEdge))
      succOf("bar") should contain theSameElementsAs expected(("this", AlwaysEdge))
      succOf("this", NodeTypes.IDENTIFIER) should contain theSameElementsAs expected(("bar()", AlwaysEdge))
      succOf("bar()") should contain theSameElementsAs expected(("RET", 2, AlwaysEdge))
    }

    "be correct for methods in class type decls with assignment" in {
      implicit val cpg: Cpg = code("""
                                     |var a = class ClassA {
                                     |  foo() {
                                     |    bar()
                                     |  }
                                     |}
                                     |""".stripMargin)
      succOf(":program") should contain theSameElementsAs expected(("a", AlwaysEdge))
      // call to constructor of ClassA
      succOf("a") should contain theSameElementsAs expected(("class ClassA", AlwaysEdge))
    }

    "be correct for outer method of anonymous class declaration" in {
      implicit val cpg: Cpg = code("var a = class {}")
      succOf(":program") should contain theSameElementsAs expected(("a", AlwaysEdge))
      succOf("a") should contain theSameElementsAs expected(("class <anon-class>0", AlwaysEdge))
      succOf("class <anon-class>0") should contain theSameElementsAs expected(("var a = class {}", AlwaysEdge))
      succOf("var a = class {}") should contain theSameElementsAs expected(("RET", AlwaysEdge))
    }
  }

}
