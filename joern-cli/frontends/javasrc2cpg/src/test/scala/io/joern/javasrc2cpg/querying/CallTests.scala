package io.joern.javasrc2cpg.querying

import io.joern.javasrc2cpg.testfixtures.JavaSrcCode2CpgFixture
import io.joern.x2cpg.Defines
import io.shiftleft.codepropertygraph.generated.edges.Ref
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators, nodes}
import io.shiftleft.codepropertygraph.generated.nodes.{Call, FieldIdentifier, Identifier, Literal, MethodParameterIn}
import io.shiftleft.semanticcpg.language.NoResolve
import io.shiftleft.semanticcpg.language._
import overflowdb.traversal.jIteratortoTraversal
import overflowdb.traversal.toNodeTraversal

class NewCallTests extends JavaSrcCode2CpgFixture {
  "calls to imported methods" when {
    "they are static methods imported from java.lang.* should be resolved" in {
      val cpg = code("""
                       |class Test {
                       |  public void test() {
                       |    String.valueOf(true);
                       |  }
                       |}
                       |
                       |""".stripMargin)

      cpg.call.name("valueOf").methodFullName.l shouldBe List("java.lang.String.valueOf:java.lang.String(boolean)")
    }

    "they are instance methods imported from java.lang.* should be resolved" in {
      val cpg = code("""
                       |class Test {
                       |  public void test(String s) {
                         |  s.length();
                       |  }
                       |}
                       |
                       |""".stripMargin)

      cpg.call.name("length").methodFullName.l shouldBe List("java.lang.String.length:int()")
    }

    "they are calls to instance methods from java imports should be resolved" in {
      val cpg = code("""
                       |import java.util.Base64;
                       |
                       |class Test {
                       |  public void test(Base64.Decoder decoder, String src) {
                       |    decoder.decode(src);
                       |  }
                       |}
                       |
                       |""".stripMargin)
      cpg.call.name("decode").methodFullName.l shouldBe List("java.util.Base64$Decoder.decode:byte[](java.lang.String)")
    }

    "they are calls to static methods from java imports should be resolved" in {
      val cpg = code("""
                       |import java.util.Base64;
                       |
                       |class Foo {
                       |  void test() {
                       |    Base64.getDecoder();
                       |  }
                       |}
                       |""".stripMargin)

      cpg.call.name("getDecoder").methodFullName.l shouldBe List(
        "java.util.Base64.getDecoder:java.util.Base64$Decoder()"
      )
    }
  }

  "calls to static methods in other files should be resolved" in {
    val cpg = code("""
        |package foo;
        |
        |class Foo {
        |  public static String foo() {
        |    return "FOO";
        |  }
        |}
        |""".stripMargin)
      .moreCode("""
        |package bar;
        |
        |import foo.Foo;
        |
        |class Bar {
        |  void test() {
        |    Foo.foo();
        |  }
        |}
        |""".stripMargin)

    cpg.call.name("foo").methodFullName.l shouldBe List("foo.Foo.foo:java.lang.String()")
  }

  "calls with unresolved receivers should have the correct fullnames" in {
    val cpg = code("""
        |import a.*;
        |
        |class Test {
        |
        |  void test() {
        |    foo().bar();
        |  }
        |}
        |""".stripMargin)

    cpg.call.name("foo").typeFullName.l shouldBe List("ANY")
    cpg.call.name("foo").methodFullName.l shouldBe List("Test.foo:<unresolvedSignature>(0)")
    cpg.call.name("bar").methodFullName.l shouldBe List("<unresolvedNamespace>.bar:<unresolvedSignature>(0)")
  }
  "calls to imported nested classes should be resolved" in {
    lazy val cpg = code("""
      |import foo.Foo.Bar;
      |
      |class Test {
      |  void test() {
      |    Bar.bar();
      |  }
      |}""".stripMargin)
      .moreCode(
        """
      |package foo;
      |
      |public class Foo {
      |  public class Bar {
      |    void bar() {}
      |  }
      |}
      |""".stripMargin,
        fileName = "Foo.java"
      )

    cpg.call.name("bar").methodFullName.l shouldBe List("foo.Foo$Bar.bar:void()")
  }

  "constructor init method call" should {
    lazy val cpg = code("""
        |class Foo {
        |  Foo(long aaa) {
        |  }
        |  static void method() {
        |    Foo foo = new Foo(1);
        |  }
        |}
        |""".stripMargin)

    "have correct methodFullName and signature" in {
      val initCall = cpg.call.nameExact(io.joern.x2cpg.Defines.ConstructorMethodName).head
      initCall.signature shouldBe "void(long)"
      initCall.methodFullName shouldBe s"Foo.${io.joern.x2cpg.Defines.ConstructorMethodName}:void(long)"
    }

    "contain a call to `<init>` with one of the arguments containing a REF edge to the newly-defined local" in {
      cpg.call.nameExact("<init>").argument(0).isIdentifier.outE.collectAll[Ref].l should not be List()
    }
  }

  "calls to methods with varargs should be resolved correctly" in {
    val cpg = code("""
        |class Test {
        |  void foo(String... inputs) {
        |    System.out.println(inputs.length);
        |  }
        |
        |  void test() {
        |    foo("a", "b");
        |  }
        |}
      |""".stripMargin)

    cpg.call.name("foo").methodFullName.toList shouldBe List("Test.foo:void(java.lang.String[])")
  }

  "calls to static methods in other files with varargs should be resolved correctly" in {
    val cpg = code("""
        |class Test {
        |
        |  void test(String[] inputs) {
        |    Foo.foo("a", "b");
        |  }
        |}
        |""".stripMargin)
      .moreCode(
        """
        |class Foo {
        |  static void foo(String... inputs) {
        |    System.out.println(inputs.length);
        |  }
        |}
        |""".stripMargin,
        fileName = "Foo.java"
      )

    cpg.call.name("foo").methodFullName.toList shouldBe List("Foo.foo:void(java.lang.String[])")
  }

  "calls to static methods in different files should be resolved correctly" in {
    val cpg = code(
      """
        |public class Foo {
        |  public static Foo foo(String arg) {
        |    return new Foo();
        |  }
        |
        |  public static Foo foo(int x) {
        |    return new Foo();
        |  }
        |}
        |""".stripMargin,
      fileName = "Foo.java"
    ).moreCode("""
        |class Bar {
        |  public static void bar(String barArg) {
        |    Foo.foo(barArg);
        |  }
        |}
        |""".stripMargin)

    inside(cpg.method.name("bar").call.l) { case List(fooCall) =>
      fooCall.methodFullName shouldBe "Foo.foo:Foo(java.lang.String)"
    }
  }

  "calls to unresolved lambda parameters" should {
    val cpg = code("""
		 |class Foo {
		 |  public void isSuccess(ExecutorService executorService) {
		 |    var responses = executorService.invokeAll(flagCalls);
		 |    responses.stream().filter(r -> {
		 |      return r.get().getStatusCode() == 200;
		 |    });
		 |  }
		 |}""".stripMargin)

    "have the correct call name" in {
      cpg.call
        .name("get")
        .methodFullName
        .head shouldBe s"${Defines.UnresolvedNamespace}.get:${Defines.UnresolvedSignature}(0)"
    }
  }

  "calls to instance methods in same class" should {
    "have ref edges from implicit `this` for an explicit constructor invocation" in {
      val cpg = code("""
			 |class Foo {
			 |  public Foo() {
			 |    this(42);
			 |  }
			 |
			 |  public Foo(int x) {}
			 |}
			 |""".stripMargin)

      cpg.method
        .fullNameExact(s"Foo.${io.joern.x2cpg.Defines.ConstructorMethodName}:void()")
        .call
        .nameExact(io.joern.x2cpg.Defines.ConstructorMethodName)
        .argument(0)
        .l match {
        case List(thisNode: Identifier) =>
          thisNode._refOut.l match {
            case List(paramNode: MethodParameterIn) =>
              paramNode.name shouldBe "this"
              paramNode.method.fullName shouldBe s"Foo.${io.joern.x2cpg.Defines.ConstructorMethodName}:void()"

            case result => fail(s"Expected REF edge to method parameter but found $result")
          }

        case result => fail(s"Expected <init> call with `this` receiver but found $result")
      }
    }
    "have ref edges from implicit `this` to method parameter" in {
      val cpg = code("""
			 |class Foo {
			 |  public void test() {
			 |    foo(42);
			 |  }
			 |
			 |  public void foo(int x) {}
			 |}""".stripMargin)

      cpg.method.name("test").call.name("foo").argument(0).outE.collectAll[Ref].l match {
        case List(ref) =>
          ref.inNode match {
            case param: MethodParameterIn =>
              param.name shouldBe "this"
              param.index shouldBe 0
              param.method.fullName shouldBe "Foo.test:void()"

            case result => fail(s"Expected ref edge to method param but found $result")
          }

        case result => fail(s"Expected out ref edge but got $result")
      }
    }

    "have ref edges from explicit `this` to method parameter" in {
      val cpg = code("""
                      |class Foo {
                      |  public void test() {
                      |    this.foo(42);
                      |  }
                      |
                      |  public void foo(int x) {}
                      |}""".stripMargin)

      cpg.method.name("test").call.name("foo").argument(0).outE.collectAll[Ref].l match {
        case List(ref) =>
          ref.inNode match {
            case param: MethodParameterIn =>
              param.name shouldBe "this"
              param.index shouldBe 0
              param.method.fullName shouldBe "Foo.test:void()"

            case result => fail(s"Expected ref edge to method param but found $result")
          }

        case result => fail(s"Expected out ref edge but got $result")
      }
    }
  }

  "call to method in different class" should {
    lazy val cpg = code(
      """
        |class Base {
        |  void method(int aaa) {}
        |}
        |""".stripMargin,
      "Base.java"
    ).moreCode(
      """
        |class Derived extends Base {}
        |""".stripMargin,
      "Derived.java"
    ).moreCode("""
        |class User {
        |  static void user() {
        |    Derived derived = new Derived();
        |    derived.method(1);
        |  }
        |}
        |""".stripMargin)

    "have correct methodFullName" in {
      cpg.call.nameExact("method").methodFullName.head shouldBe "Derived.method:void(int)"
    }
  }

  "call to method in same class" should {
    lazy val cpg = code(
      """
        |class Base {
        |  void method(int aaa) {}
        |}
        |""".stripMargin,
      "Base.java"
    ).moreCode(
      """
        |class Derived extends Base {}
        |""".stripMargin,
      "Derived.java"
    ).moreCode("""
        |class MoreDerived extends Derived {
        |  void user() {
        |    method(1);
        |  }
        |}
        |""".stripMargin)

    "have correct methodFullName" in {
      cpg.call.nameExact("method").methodFullName.head shouldBe "MoreDerived.method:void(int)"
    }
  }

  "code fields" should {
    "be correct for chained calls starting at a constructor invocation" in {
      lazy val cpg = code("""
          |class Foo {
          |  private String value;
          |
          |  public String getValue() {
          |    return value;
          |  }
          |
          |  public static void test() {
          |    String s = new Foo().getValue();
          |  }
          |}
          |""".stripMargin)

      cpg.call.name("getValue").l match {
        case List(getValueCall) =>
          getValueCall.code shouldBe "new Foo().getValue()"

        case result => fail(s"Expected single getValue call but got $result")
      }
    }

    "be correct for constructor invocations" in {
      lazy val cpg = code("""
          |class Foo {
          |
          |  public static void test() {
          |    Foo f = new Foo();
          |  }
          |}
          |""".stripMargin)
      cpg.call.name(io.joern.x2cpg.Defines.ConstructorMethodName).l match {
        case List(initCall: Call) =>
          initCall.code shouldBe "new Foo()"

        case result => fail(s"Expected single init call but got $result")
      }
    }
  }

  "call to method with generic return type" should {
    lazy val cpg = code("""
        |class Foo {
        |  void method(java.util.function.Function<String, Integer> supplier) {
        |     supplier.apply("abc");
        |  }
        |}
        |""".stripMargin)

    "have correct substitute type as expression type" in {
      cpg.call.name("apply").evalType.head shouldBe "java.lang.Integer"
    }
    "have correct methodFullName to erased method signature" in {
      cpg.call.name("apply").methodFullName.head shouldBe
        "java.util.function.Function.apply:java.lang.Object(java.lang.Object)"
    }
  }

  "call to generic method of generic type" should {
    lazy val cpg = code("""
        |class Foo <T extends Number> {
        |  <S extends T> void foo(S i) {}
        |
        |  static void method() {
        |    Foo<Integer> obj = new Foo();
        |    obj.foo(1);
        |  }
        |}
        |""".stripMargin)

    "have correct methodFullName" in {
      cpg.call("foo").methodFullName.head shouldBe "Foo.foo:void(java.lang.Number)"
    }
  }

  "call to method with generic array parameter" should {
    lazy val cpg = code("""
        |class Foo <T> {
        |  void foo(T[] aaa) {}
        |
        |  static void method() {
        |    Foo<Integer> obj = new Foo();
        |    Integer[] array = new Integer[3];
        |    obj.foo(array);
        |  }
        |}
        |""".stripMargin)

    "should have correct methodFullName" in {
      cpg.call("foo").methodFullName.head shouldBe "Foo.foo:void(java.lang.Object[])"
    }
  }

  "call to super method" should {
    val cpg = code("""
        |class Foo {
        |  @Override
        |  public String toString() {
        |    return super.toString();
        |  }
        |}
        |""".stripMargin)

    "create a `super` receiver with fields correctly set" in {
      val superReceiver = cpg.call.name("toString").argument(0).collectAll[Identifier].head
      superReceiver.name shouldBe "this"
      superReceiver.code shouldBe "super"
      superReceiver.typeFullName shouldBe "java.lang.Object"
      superReceiver.order shouldBe 1
      superReceiver.argumentIndex shouldBe 0
      superReceiver.lineNumber shouldBe Some(5)
      superReceiver.columnNumber shouldBe Some(12)
    }
  }

  "call to method in derived class using external package" should {
    lazy val cpg = code("""
        |import org.hibernate.Query;
        |import org.hibernate.Session;
        |import org.hibernate.SessionFactory;
        |
        |class Base {
        |  Session getCurrentSession() {
        |		return this.sessionFactory.getCurrentSession();
        |	}
        |}
        |
        |class Derived extends Base{
        | void foo() {
        |		Query q = getCurrentSession().createQuery("FROM User");
        |		return;
        |	}
        |}
        |""".stripMargin)

    "have correct methodFullName" in {
      cpg.call.nameExact("createQuery").methodFullName.head.split(":").head shouldBe "org.hibernate.Session.createQuery"
      cpg.call
        .nameExact("getCurrentSession")
        .methodFullName
        .last shouldBe "Derived.getCurrentSession:org.hibernate.Session()"
    }
  }

  "call to external method in a builder-like pattern" should {
    val cpg = code("""
        |package example;
        |import org.Builder;
        |import org.Client;
        |
        |class Main {
        | static void main(String[] args) {
        |   Client foo = new Builder().foo().buildClient(); // 8
        |   new Builder().somethingElse().buildClient();    // 9
        | }
        |}
        |""".stripMargin)

    "have correct method full name for `buildClient` call on line 8" in {
      cpg.call("buildClient").lineNumber(8).l match {
        case List(buildClient) =>
          buildClient.methodFullName shouldBe "<unresolvedNamespace>.buildClient:<unresolvedSignature>(0)"
        case result => fail(s"Expected single buildClient call but got $result")
      }
    }

    "have correct method full name for `buildClient` call on line 9" in {
      cpg.call("buildClient").lineNumber(9).l match {
        case List(buildClient) =>
          buildClient.methodFullName shouldBe "<unresolvedNamespace>.buildClient:<unresolvedSignature>(0)"
        case result => fail(s"Expected single buildClient call but got $result")
      }
    }
  }
}

class CallTests extends JavaSrcCode2CpgFixture {

  lazy val cpg = code("""
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
      |""".stripMargin)

  "should contain a call node for `add` with correct fields" in {
    val List(x) = cpg.call("add").l
    x.code shouldBe "this.add(argc, 3)"
    x.name shouldBe "add"
    x.order shouldBe 1
    x.methodFullName shouldBe "test.Foo.add:int(int,int)"
    x.signature shouldBe "int(int,int)"
    x.argumentIndex shouldBe 1
    x.lineNumber shouldBe Some(9)
  }

  "should allow traversing from call to arguments" in {
    cpg.call("add").argument.size shouldBe 3
    val List(arg0) = cpg.call("add").argument(0).l
    arg0.isInstanceOf[nodes.Identifier] shouldBe true
    arg0.asInstanceOf[nodes.Identifier].name shouldBe "this"
    arg0.code shouldBe "this"
    arg0.order shouldBe 1
    arg0.argumentIndex shouldBe 0

    val List(arg1) = cpg.call("add").argument(1).l
    arg1.isInstanceOf[nodes.Identifier] shouldBe true
    arg1.asInstanceOf[nodes.Identifier].name shouldBe "argc"
    arg1.code shouldBe "argc"
    arg1.order shouldBe 2
    arg1.argumentIndex shouldBe 1

    val List(arg2) = cpg.call("add").argument(2).l
    arg2.asInstanceOf[nodes.Literal].code shouldBe "3"
    arg2.isInstanceOf[nodes.Literal] shouldBe true
    arg2.code shouldBe "3"
    arg2.order shouldBe 3
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
    call.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH
    call.methodFullName shouldBe s"test.Foo.foo:${Defines.UnresolvedSignature}(1)"
    call.signature shouldBe s"${Defines.UnresolvedSignature}(1)"
    call.code shouldBe "foo(argc)"
  }

  "should create a call node for call on explicit object" in {
    val call = cpg.typeDecl.name("Bar").method.name("foo").call.nameExact("myMethod").head

    call.code shouldBe "myObj.myMethod(\"Hello, world!\")"
    call.name shouldBe "myMethod"
    call.methodFullName shouldBe "test.MyObject.myMethod:java.lang.String(java.lang.String)"
    call.signature shouldBe "java.lang.String(java.lang.String)"
    call.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH

    val List(objName: Identifier, argument: Literal) = call.astChildren.l: @unchecked

    objName.order shouldBe 1
    objName.argumentIndex shouldBe 0
    objName.code shouldBe "myObj"
    objName.name shouldBe "myObj"

    argument.code shouldBe "\"Hello, world!\""
    argument.order shouldBe 2
    argument.argumentIndex shouldBe 1
  }

  "should create a call node for a call with an implicit `this`" in {
    val call = cpg.typeDecl.name("Bar").method.name("bar").call.nameExact("foo").head

    call.code shouldBe "this.foo(obj)"
    call.name shouldBe "foo"
    call.methodFullName shouldBe "test.Bar.foo:java.lang.String(test.MyObject)"
    call.signature shouldBe "java.lang.String(test.MyObject)"
    call.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH

    val List(identifier: Identifier, argument: Call) = call.argument.l: @unchecked
    identifier.order shouldBe 1
    identifier.argumentIndex shouldBe 0
    identifier.code shouldBe "this"
    identifier.name shouldBe "this"

    argument.name shouldBe Operators.fieldAccess
    argument.typeFullName shouldBe "test.MyObject"

    val List(ident: Identifier, fieldIdent: FieldIdentifier) = argument.argument.l: @unchecked
    ident.name shouldBe "this"
    fieldIdent.canonicalName shouldBe "obj"
  }

  "should create a call node for a call with an explicit `this`" in {
    val call = cpg.typeDecl.name("Bar").method.name("baz").call.nameExact("foo").head

    call.code shouldBe "this.foo(obj)"
    call.name shouldBe "foo"
    call.methodFullName shouldBe "test.Bar.foo:java.lang.String(test.MyObject)"
    call.signature shouldBe "java.lang.String(test.MyObject)"
    call.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH

    val List(objName: Identifier, argument: Call) = call.astChildren.l: @unchecked

    objName.order shouldBe 1
    objName.argumentIndex shouldBe 0
    objName.name shouldBe "this"
    objName.code shouldBe "this"

    argument.name shouldBe Operators.fieldAccess
    argument.typeFullName shouldBe "test.MyObject"
    argument.code shouldBe "this.obj"
    argument.order shouldBe 2
    argument.argumentIndex shouldBe 1

    val List(ident: Identifier, fieldIdent: FieldIdentifier) = argument.argument.l: @unchecked
    ident.name shouldBe "this"
    fieldIdent.canonicalName shouldBe "obj"
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

class CallTests2 extends JavaSrcCode2CpgFixture {
  lazy val cpg = code("""
      |class Foo {
      |    public static class Ops {
      |        public <T> T ident(T x) {
      |            return x;
      |        }
      |    }
      |    public Integer method(Integer aaa) {
      |        Ops ops = new Ops();
      |        Integer ret = ops.ident(aaa);
      |        return ret;
      |    }
      |}
      |""".stripMargin)

  "test methodFullName for call to generic function" in {
    cpg.call(".*ident.*").methodFullName.head shouldBe "Foo$Ops.ident:java.lang.Object(java.lang.Object)"
  }
}
