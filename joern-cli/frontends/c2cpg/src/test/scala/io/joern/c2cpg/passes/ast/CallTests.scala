package io.joern.c2cpg.passes.ast

import io.joern.c2cpg.astcreation.Defines
import io.joern.c2cpg.testfixtures.C2CpgSuite
import io.joern.x2cpg.Defines as X2CpgDefines
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.Call
import io.shiftleft.codepropertygraph.generated.nodes.Literal
import io.shiftleft.semanticcpg.language.*

class CallTests extends C2CpgSuite {

  implicit val resolver: NoResolve.type = NoResolve

  "CallTest 1" should {
    val cpg = code("""
        |int add(int x, int y) {
        |  return x + y;
        |}
        |int main(int argc, char **argv) {
        |  printf("%d\n", add((1+2), 3));
        |}""".stripMargin)

    "contain a call node for `add` with correct fields" in {
      val List(x) = cpg.call("add").l
      x.code shouldBe "add((1+2), 3)"
      x.name shouldBe "add"
      x.order shouldBe 2
      x.methodFullName shouldBe "add"
      x.argumentIndex shouldBe 2
      // TODO x.signature
      // x.typeFullName : deprecated
      x.lineNumber shouldBe Option(6)
      x.columnNumber shouldBe Option(18)
    }

    "allow traversing from call to arguments" in {
      cpg.call("add").argument.size shouldBe 2

      val List(arg1) = cpg.call("add").argument(1).l
      arg1.isInstanceOf[Call] shouldBe true
      arg1.asInstanceOf[Call].name shouldBe Operators.addition
      arg1.code shouldBe "1+2"
      arg1.order shouldBe 1
      arg1.argumentIndex shouldBe 1

      val List(arg2) = cpg.call("add").argument(2).l
      arg2.isInstanceOf[Literal] shouldBe true
      arg2.asInstanceOf[Literal].code shouldBe "3"
      arg2.code shouldBe "3"
      arg2.order shouldBe 2
      arg2.argumentIndex shouldBe 2
    }

    "allow traversing from call to surrounding method" in {
      val List(x) = cpg.call("add").method.l
      x.name shouldBe "main"
    }

    "allow traversing from call to callee method" in {
      val List(x) = cpg.call("add").callee.l
      x.name shouldBe "add"
    }

    "allow traversing from argument to parameter" in {
      val List(x) = cpg.call("add").argument(1).parameter.l
      x.name shouldBe "x"
    }
  }

  "CallTest 2" should {
    val cpg = code(
      """
        |using namespace std;
        |
        |class A{
        |  public:
        |    int a;
        |};
        |
        |class B{
        |  public:
        |    A* GetObj();
        |};
        |
        |A* B::GetObj() {
        |  return nullptr;
        |}
        |
        |class C{
        |  public:
        |    A* GetObject();
        |};
        |
        |A* C::GetObject() {
        |  B * b;
        |  return b->GetObj();
        |}
        |
        |bool Run(A *obj, C *c) {
        |  const A * a = c->GetObject();
        |  a->a;
        |  return true;
        |}
        |""".stripMargin,
      "code.cpp"
    )

    "have the correct callIn" in {
      val List(m) = cpg.method.nameNot("<global>").where(_.ast.isReturn.code(".*nullptr.*")).l
      val List(c) = cpg.call.codeExact("b->GetObj()").l
      c.callee.l should contain(m)
      val List(callIn) = m.callIn.l
      callIn.code shouldBe "b->GetObj()"
    }
  }

  "CallTest 3" should {
    val cpg = code(
      """
        |int square(int num) {
        |    return num * num;
        |}
        |void call_square() {
        |    ::square(10);
        |}
        |""".stripMargin,
      "test.cpp"
    )
    "have correct names for static methods / calls" in {
      cpg.method.name("square").fullName.head shouldBe "square:int(int)"
      cpg.method.name("call_square").call.methodFullName.head shouldBe "square:int(int)"
    }
  }

  "CallTest 4" should {
    val cpg = code(
      """
        |class A {
        |  public:
        |    static int square(int num) {
        |      return num * num;
        |    }
        |};
        |
        |void call_square() {
        |  A::square(10);
        |}
        |""".stripMargin,
      "test.cpp"
    )
    "have correct names for static methods / calls from classes" in {
      cpg.method.name("square").fullName.head shouldBe "A.square:int(int)"
      cpg.method.name("call_square").call.methodFullName.head shouldBe "A.square:int(int)"
    }
  }

  "CallTest 5" should {
    val cpg = code(
      """
        |class A {
        |  void a() {
        |    b();
        |  }
        |  void b() {}
        |};
        |""".stripMargin,
      "test.cpp"
    )
    "have correct type full names for calls" in {
      val List(bCall) = cpg.call.l
      bCall.methodFullName shouldBe "A.b:void()"
      val List(bMethod) = cpg.method.name("b").internal.l
      bMethod.fullName shouldBe "A.b:void()"
      bMethod.callIn.head shouldBe bCall
      bCall.callee.head shouldBe bMethod
    }
  }

  "CallTest 6" should {
    val cpg = code(
      """
        |class A {
        |  public:
        |    void foo1(){
        |      foo2();
        |    }
        |	static void foo2() {}
        |};
        |
        |int main() {
        |  A a;
        |  a.foo1();
        |}
        |""".stripMargin,
      "test.cpp"
    )
    "have correct type full names for calls" in {
      val List(foo2Call) = cpg.call("foo2").l
      foo2Call.methodFullName shouldBe "A.foo2:void()"
      val List(foo2Method) = cpg.method("foo2").l
      foo2Method.fullName shouldBe "A.foo2:void()"
    }
  }

  "Successfully typed calls" should {
    "have correct call for call on non virtual class method" in {
      val cpg = code(
        """
          |namespace NNN {
          |  class A {
          |    public:
          |      void foo(int a){}
          |  };
          |}
          |
          |void outer() {
          |  NNN::A a;
          |  a.foo(1);
          |}
          |""".stripMargin,
        "test.cpp"
      )

      val List(call) = cpg.call.nameExact("foo").l
      call.signature shouldBe "void(int)"
      call.methodFullName shouldBe "NNN.A.foo:void(int)"
      call.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      call.typeFullName shouldBe "void"

      val List(instArg, arg1) = call.argument.l
      instArg.code shouldBe "a"
      instArg.argumentIndex shouldBe 0
      arg1.code shouldBe "1"
      arg1.argumentIndex shouldBe 1

      call.receiver.isEmpty shouldBe true
    }

    "have correct call for call on virtual class method" in {
      val cpg = code(
        """
          |namespace NNN {
          |  class A {
          |    public:
          |      virtual void foo(int a){}
          |  };
          |}
          |
          |void outer() {
          |  NNN::A a;
          |  a.foo(1);
          |}
          |""".stripMargin,
        "test.cpp"
      )

      val List(call) = cpg.call.nameExact("foo").l
      call.signature shouldBe "void(int)"
      call.methodFullName shouldBe "NNN.A.foo:void(int)"
      call.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH
      call.typeFullName shouldBe "void"

      val List(instArg, arg1) = call.argument.l
      instArg.code shouldBe "a"
      instArg.argumentIndex shouldBe 0
      arg1.code shouldBe "1"
      arg1.argumentIndex shouldBe 1

      val List(receiver) = call.receiver.l
      receiver shouldBe instArg
    }

    "have correct call for call on stand alone method (CPP)" in {
      val cpg = code(
        """
          |namespace NNN {
          |  void foo(int a){}
          |}
          |
          |void outer() {
          |  NNN::foo(1);
          |}
          |""".stripMargin,
        "test.cpp"
      )

      val List(call) = cpg.call.nameExact("foo").l
      call.signature shouldBe "void(int)"
      call.methodFullName shouldBe "NNN.foo:void(int)"
      call.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      call.typeFullName shouldBe "void"

      val List(arg1) = call.argument.l
      arg1.code shouldBe "1"

      call.receiver.isEmpty shouldBe true
    }

    "have correct call for call on lambda function with explicit return type" in {
      val cpg = code(
        """
          |void outer() {
          |  [](int a) -> int { return a; }(1);
          |}
          |""".stripMargin,
        "test.cpp"
      )

      val List(call) = cpg.call.nameExact("<operator>()").l
      call.signature shouldBe "int(int)"
      call.methodFullName shouldBe "<operator>():int(int)"
      call.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH
      call.typeFullName shouldBe "int"

      val List(arg1) = call.argument.l
      arg1.code shouldBe "1"
      arg1.argumentIndex shouldBe 1

      val List(receiver) = call.receiver.l
      receiver.isMethodRef shouldBe true
      receiver.argumentIndex shouldBe -1
    }

    "have correct call for call on lambda function without explicit return type" in {
      val cpg = code(
        """
          |void outer() {
          |  [](int a) {}(1);
          |}
          |""".stripMargin,
        "test.cpp"
      )

      val List(call) = cpg.call.nameExact("<operator>()").l
      call.signature shouldBe "void(int)"
      call.methodFullName shouldBe "<operator>():void(int)"
      call.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH
      call.typeFullName shouldBe "void"

      val List(arg1) = call.argument.l
      arg1.code shouldBe "1"
      arg1.argumentIndex shouldBe 1

      val List(receiver) = call.receiver.l
      receiver.isMethodRef shouldBe true
      receiver.argumentIndex shouldBe -1
    }

    "have correct call for call on function pointer (CPP)" in {
      val cpg = code(
        """
          |class A {
          |  public:
          |    void (*foo)(int);
          |};
          |
          |void outer() {
          |  A a;
          |  a.foo(1);
          |}
          |""".stripMargin,
        "test.cpp"
      )

      val List(call) = cpg.call.nameExact(Defines.OperatorPointerCall).l
      call.signature shouldBe ""
      call.methodFullName shouldBe Defines.OperatorPointerCall
      call.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH
      call.typeFullName shouldBe "void"

      val List(arg1) = call.argument.l
      arg1.code shouldBe "1"
      arg1.argumentIndex shouldBe 1

      val List(receiver) = call.receiver.l
      receiver.code shouldBe "a.foo"
      receiver.argumentIndex shouldBe -1
    }

    "have correct call for call on callable object" in {
      val cpg = code(
        """
          |namespace NNN {
          |  class Callable {
          |    public:
          |      void operator()(int a){}
          |  };
          |}
          |class A {
          |  public:
          |    NNN::Callable foo;
          |};
          |
          |void outer() {
          |  A a;
          |  a.foo(1);
          |}
          |""".stripMargin,
        "test.cpp"
      )

      val List(call) = cpg.call.nameExact("<operator>()").l
      call.signature shouldBe "void(int)"
      call.methodFullName shouldBe "NNN.Callable.<operator>():void(int)"
      call.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      call.typeFullName shouldBe "void"

      val List(instArg, arg1) = call.argument.l
      instArg.code shouldBe "a.foo"
      instArg.argumentIndex shouldBe 0
      arg1.code shouldBe "1"
      arg1.argumentIndex shouldBe 1

      val List(receiver) = call.receiver.l
      receiver shouldBe instArg
    }

    "have correct call for call on function pointer (C)" in {
      val cpg = code(
        """
          |struct A {
          |  void (*foo)(int);
          |}
          |void outer() {
          |  struct A a;
          |  a.foo(1);
          |}
          |""".stripMargin,
        "test.c"
      )

      val List(call) = cpg.call.nameExact(Defines.OperatorPointerCall).l
      call.signature shouldBe ""
      call.methodFullName shouldBe Defines.OperatorPointerCall
      call.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH
      call.typeFullName shouldBe "void"

      val List(arg1) = call.argument.l
      arg1.code shouldBe "1"
      arg1.argumentIndex shouldBe 1

      val List(receiver) = call.receiver.l
      receiver.code shouldBe "a.foo"
      receiver.argumentIndex shouldBe -1
    }

    "have correct call for call on stand alone method (C)" in {
      val cpg = code(
        """
          |void foo(int) {}
          |void outer() {
          |  foo(1);
          |}
          |""".stripMargin,
        "test.c"
      )

      val List(call) = cpg.call.nameExact("foo").l
      call.signature shouldBe ""
      call.methodFullName shouldBe "foo"
      call.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      call.typeFullName shouldBe "void"

      val List(arg1) = call.argument.l
      arg1.code shouldBe "1"

      call.receiver.isEmpty shouldBe true
    }

    "have correct call for call on extern C function" in {
      val cpg = code(
        """
          |extern "C" {
          |  void foo(int);
          |}
          |
          |void outer() {
          |  foo(1);
          |}
          |""".stripMargin,
        "test.cpp"
      )

      val List(call) = cpg.call.nameExact("foo").l
      call.signature shouldBe ""
      call.methodFullName shouldBe "foo"
      call.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      call.typeFullName shouldBe "void"

      val List(arg1) = call.argument.l
      arg1.code shouldBe "1"
      arg1.argumentIndex shouldBe 1

      call.receiver.isEmpty shouldBe true
    }
  }

  "Not successfully typed calls" should {
    "have correct call for field reference style call (CPP)" in {
      val cpg = code(
        """
          |void outer() {
          |  Unknown a;
          |  a.foo(1);
          |}
          |""".stripMargin,
        "test.cpp"
      )

      val List(call) = cpg.call.nameExact("foo").l
      call.signature shouldBe X2CpgDefines.UnresolvedSignature
      call.methodFullName shouldBe s"${X2CpgDefines.UnresolvedNamespace}.foo:${X2CpgDefines.UnresolvedSignature}(1)"
      call.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      call.typeFullName shouldBe X2CpgDefines.Any

      val List(instArg, arg1) = call.argument.l
      instArg.code shouldBe "a"
      instArg.argumentIndex shouldBe 0
      arg1.code shouldBe "1"
      arg1.argumentIndex shouldBe 1

      val List(receiver) = call.receiver.l
      receiver shouldBe instArg
    }

    "have correct call for plain call (CPP)" in {
      val cpg = code(
        """
          |void outer() {
          |  foo(1);
          |}
          |""".stripMargin,
        "test.cpp"
      )

      val List(call) = cpg.call.nameExact("foo").l
      call.signature shouldBe X2CpgDefines.UnresolvedSignature
      call.methodFullName shouldBe s"${X2CpgDefines.UnresolvedNamespace}.foo:${X2CpgDefines.UnresolvedSignature}(1)"
      call.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      call.typeFullName shouldBe X2CpgDefines.Any

      val List(arg1) = call.argument.l
      arg1.code shouldBe "1"
      arg1.argumentIndex shouldBe 1

      call.receiver.isEmpty shouldBe true
    }

    "have correct call for call on arbitrary expression (CPP)" in {
      val cpg = code(
        """
          |void outer() {
          |  getX()(1);
          |}
          |""".stripMargin,
        "test.cpp"
      )

      val List(call) = cpg.call.nameExact("<operator>()").l
      call.signature shouldBe X2CpgDefines.UnresolvedSignature
      call.methodFullName shouldBe s"${X2CpgDefines.UnresolvedNamespace}.<operator>():${X2CpgDefines.UnresolvedSignature}(1)"
      call.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      call.typeFullName shouldBe X2CpgDefines.Any

      val List(instArg, arg1) = call.argument.l
      instArg.code shouldBe "getX()"
      instArg.argumentIndex shouldBe 0
      arg1.code shouldBe "1"
      arg1.argumentIndex shouldBe 1

      val List(receiver) = call.receiver.l
      receiver shouldBe instArg
    }

    "have correct call for field reference style call (C)" in {
      val cpg = code(
        """
          |void outer() {
          |  struct A a;
          |  a.foo(1);
          |}
          |""".stripMargin,
        "test.c"
      )

      val List(call) = cpg.call.nameExact(Defines.OperatorPointerCall).l
      call.signature shouldBe ""
      call.methodFullName shouldBe Defines.OperatorPointerCall
      call.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH
      call.typeFullName shouldBe X2CpgDefines.Any

      val List(arg1) = call.argument.l
      arg1.code shouldBe "1"
      arg1.argumentIndex shouldBe 1

      val List(receiver) = call.receiver.l
      receiver.code shouldBe "a.foo"
      receiver.argumentIndex shouldBe -1
    }

    "have correct call for plain call (C)" in {
      val cpg = code(
        """
          |void outer() {
          |  foo(1);
          |}
          |""".stripMargin,
        "test.c"
      )

      val List(call) = cpg.call.nameExact("foo").l
      call.signature shouldBe ""
      call.methodFullName shouldBe s"foo"
      call.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      call.typeFullName shouldBe X2CpgDefines.Any

      val List(arg1) = call.argument.l
      arg1.code shouldBe "1"
      arg1.argumentIndex shouldBe 1

      call.receiver.isEmpty shouldBe true
    }

    "have correct call for call on arbitrary expression (C)" in {
      val cpg = code(
        """
          |void outer() {
          |  getX()(1);
          |}
          |""".stripMargin,
        "test.c"
      )

      val List(call) = cpg.call.nameExact(Defines.OperatorPointerCall).l
      call.signature shouldBe ""
      call.methodFullName shouldBe Defines.OperatorPointerCall
      call.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH
      call.typeFullName shouldBe X2CpgDefines.Any

      val List(arg1) = call.argument.l
      arg1.code shouldBe "1"
      arg1.argumentIndex shouldBe 1

      val List(receiver) = call.receiver.l
      receiver.code shouldBe "getX()"
      receiver.argumentIndex shouldBe -1
    }
  }
}
