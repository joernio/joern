package io.joern.c2cpg.passes.ast

import io.joern.c2cpg.testfixtures.CCodeToCpgSuite
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes.Call
import io.shiftleft.codepropertygraph.generated.nodes.Literal
import io.shiftleft.semanticcpg.language.NoResolve
import io.shiftleft.semanticcpg.language._

class CallTests extends CCodeToCpgSuite {

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
      c.callee.head shouldBe m
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
      cpg.method.name("square").fullName.head shouldBe "square"
      cpg.method.name("call_square").call.methodFullName.head shouldBe "square"
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
      cpg.method.name("square").fullName.head shouldBe "A.square"
      cpg.method.name("call_square").call.methodFullName.head shouldBe "A.square"
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
      bCall.methodFullName shouldBe "A.b"
      val List(bMethod) = cpg.method.name("b").internal.l
      bMethod.fullName shouldBe "A.b"
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
      foo2Call.methodFullName shouldBe "A.foo2"
      val List(foo2Method) = cpg.method("foo2").l
      foo2Method.fullName shouldBe "A.foo2"
    }
  }

}
