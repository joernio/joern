package io.joern.javasrc2cpg.querying

import io.joern.javasrc2cpg.testfixtures.JavaSrcCodeToCpgFixture
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators, nodes}
import io.shiftleft.codepropertygraph.generated.nodes.{Call, FieldIdentifier, Identifier, Literal}
import io.shiftleft.semanticcpg.language.NoResolve
import io.shiftleft.semanticcpg.language._

class CallTests extends JavaSrcCodeToCpgFixture {

  implicit val resolver: ICallResolver = NoResolve

  override val code: String =
    """
      |package test;
      | class Foo {
      |   int add(int x, int y) {
      |     return x + y;
      |   }
      |
      |   int main(int argc, char argv) {
      |     return add(argc, 3);
      |   }
      |
      |   int bar(int argc) {
      |     foo(argc);
      |   }
      | }
      |
      |class MyObject {
      |    public static String staticCall(String s) {
      |        return s;
      |    }
      |
      |    public String myMethod(String s) {
      |        return s;
      |    }
      |}
      |
      |public class Bar {
      |    MyObject obj = new MyObject();
      |
      |    public static void staticMethod() {}
      |
      |    public String foo(MyObject myObj) {
      |        return myObj.myMethod("Hello, world!");
      |    }
      |
      |    public void bar() {
      |        foo(obj);
      |    }
      |
      |    public void baz() {
      |        this.foo(obj);
      |    }
      |
      |    public void qux() {
      |        staticMethod();
      |    }
      |
      |    public void quux() {
      |      bar();
      |    }
      |}
      |""".stripMargin

  "should contain a call node for `add` with correct fields" in {
    val List(x) = cpg.call("add").l
    x.code shouldBe "this.add(argc, 3)"
    x.name shouldBe "add"
    x.order shouldBe 2
    x.methodFullName shouldBe "test.Foo.add:int(int,int)"
    x.signature shouldBe "int(int,int)"
    x.argumentIndex shouldBe 2
    x.lineNumber shouldBe Some(9)
  }

  "should allow traversing from call to arguments" in {
    cpg.call("add").argument.size shouldBe 3
    val List(arg0) = cpg.call("add").argument(0).l
    arg0.isInstanceOf[nodes.Identifier] shouldBe true
    arg0.asInstanceOf[nodes.Identifier].name shouldBe "this"
    arg0.code shouldBe "this"
    arg0.order shouldBe 0
    arg0.argumentIndex shouldBe 0

    val List(arg1) = cpg.call("add").argument(1).l
    arg1.isInstanceOf[nodes.Identifier] shouldBe true
    arg1.asInstanceOf[nodes.Identifier].name shouldBe "argc"
    arg1.code shouldBe "argc"
    arg1.order shouldBe 1
    arg1.argumentIndex shouldBe 1

    val List(arg2) = cpg.call("add").argument(2).l
    arg2.asInstanceOf[nodes.Literal].code shouldBe "3"
    arg2.isInstanceOf[nodes.Literal] shouldBe true
    arg2.code shouldBe "3"
    arg2.order shouldBe 2
    arg2.argumentIndex shouldBe 2
  }

  "should allow traversing from call to surrounding method" in {
    val List(x) = cpg.call("add").method.l
    x.name shouldBe "main"
  }

  "should allow traversing from call to callee method" in {
    val List(x) = cpg.call("add").callee.l
    x.name shouldBe "add"
  }

  "should allow traversing from argument to parameter" in {
    val List(x) = cpg.call("add").argument(1).parameter.l
    x.name shouldBe "x"
  }

  "should handle unresolved calls with appropriate defaults" in {
    val List(call: Call) = cpg.typeDecl.name("Foo").ast.isCall.name("foo").l
    call.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH.toString
    call.methodFullName shouldBe "<empty>"
    call.signature shouldBe ""
    call.code shouldBe "foo(argc)"
  }

  "should create a call node for call on explicit object" in {
    val call = cpg.typeDecl.name("Bar").method.name("foo").call.nameExact("myMethod").head

    call.code shouldBe "myObj.myMethod(\"Hello, world!\")"
    call.name shouldBe "myMethod"
    call.methodFullName shouldBe "test.MyObject.myMethod:java.lang.String(java.lang.String)"
    call.signature shouldBe "java.lang.String(java.lang.String)"
    call.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH

    val List(objName: Identifier, argument: Literal) = call.astChildren.l

    objName.order shouldBe 0
    objName.argumentIndex shouldBe 0
    objName.code shouldBe "myObj"
    objName.name shouldBe "myObj"

    argument.code shouldBe "\"Hello, world!\""
    argument.order shouldBe 1
    argument.argumentIndex shouldBe 1
  }

  "should create a call node for a call with an implicit `this`" in {
    val call = cpg.typeDecl.name("Bar").method.name("bar").call.nameExact("foo").head

    call.code shouldBe "this.foo(obj)"
    call.name shouldBe "foo"
    call.methodFullName shouldBe "test.Bar.foo:java.lang.String(test.MyObject)"
    call.signature shouldBe "java.lang.String(test.MyObject)"
    call.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH

    val List(identifier: Identifier, argument: Identifier) = call.argument.l
    identifier.order shouldBe 0
    identifier.argumentIndex shouldBe 0
    identifier.code shouldBe "this"
    identifier.name shouldBe "this"

    argument.order shouldBe 1
    argument.argumentIndex shouldBe 1
    argument.code shouldBe "obj"
    argument.name shouldBe "obj"
  }

  "should create a call node for a call with an explicit `this`" in {
    val call = cpg.typeDecl.name("Bar").method.name("baz").call.nameExact("foo").head

    call.code shouldBe "this.foo(obj)"
    call.name shouldBe "foo"
    call.methodFullName shouldBe "test.Bar.foo:java.lang.String(test.MyObject)"
    call.signature shouldBe "java.lang.String(test.MyObject)"
    call.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH

    val List(objName: Identifier, argument: Identifier) = call.astChildren.l

    objName.order shouldBe 0
    objName.argumentIndex shouldBe 0
    objName.name shouldBe "this"
    objName.code shouldBe "this"

    argument.code shouldBe "obj"
    argument.order shouldBe 1
    argument.argumentIndex shouldBe 1
  }

  "should create correct code field for static call" in {
    val call = cpg.typeDecl.name("Bar").method.name("qux").call.head
    call.name shouldBe "staticMethod"
    call.methodFullName shouldBe "test.Bar.staticMethod:void()"
    call.code shouldBe "staticMethod()"
  }

  "should create correct call signature for method call without args" in {
    val call = cpg.typeDecl.name("Bar").method.name("quux").call.head
    call.name shouldBe "bar"
    call.methodFullName shouldBe "test.Bar.bar:void()"
    call.signature shouldBe "void()"
  }
}
