package io.joern.c2cpg.passes.ast

import io.joern.c2cpg.astcreation.Defines
import io.joern.c2cpg.parser.FileDefaults
import io.joern.c2cpg.testfixtures.AstC2CpgSuite
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes.Call
import io.shiftleft.codepropertygraph.generated.nodes.Identifier
import io.shiftleft.semanticcpg.language.*

class MemberPointerCallTests extends AstC2CpgSuite(FileDefaults.CppExt) {

  "A pointer to member expression" should {

    val cpg = code("""
        |struct S {
        |  int f;
        |  int a(int x) { return x + 1; }
        |};
        |typedef int (S::*PMF)(int);
        |int viaArrow(S* s, PMF m, int x) { return (s->*m)(x); }
        |int viaDot(S& s, PMF m, int x)   { return (s.*m)(x); }
        |int plainArrow(S* s)             { return s->f; }
        |""".stripMargin)

    "be distinguishable from a plain field access and from each other" in {
      cpg.method.nameExact("viaArrow").ast.isCall.nameExact(Defines.OperatorMemberPointerArrow).code.l shouldBe
        List("s->*m")
      cpg.method.nameExact("viaDot").ast.isCall.nameExact(Defines.OperatorMemberPointerDot).code.l shouldBe
        List("s.*m")
      cpg.method.nameExact("plainArrow").ast.isCall.nameExact(Operators.indirectFieldAccess).code.l shouldBe
        List("s->f")
      cpg.call.nameExact(Operators.indirectFieldAccess).code.l shouldBe List("s->f")
    }

    "carry the member pointer as an ordinary identifier, not a field identifier" in {
      val List(access)   = cpg.call.nameExact(Defines.OperatorMemberPointerArrow).l
      val List(obj, pmf) = access.argument.l
      obj.code shouldBe "s"
      obj.argumentIndex shouldBe 1
      pmf.code shouldBe "m"
      pmf.argumentIndex shouldBe 2
      pmf.isInstanceOf[Identifier] shouldBe true
      pmf.isFieldIdentifier shouldBe false
    }
  }

  "A call through a pointer to member function" should {

    val cpg = code("""
        |struct S {
        |  int a(int x) { return x + 1; }
        |};
        |typedef int (S::*PMF)(int);
        |int viaArrow(S* s, PMF m, int x) { return (s->*m)(x); }
        |int viaDot(S& s, PMF m, int x)   { return (s.*m)(x); }
        |""".stripMargin)

    "be a dynamically dispatched pointer call with a receiver, not an operator() call" in {
      cpg.call.nameExact(Defines.OperatorCall).l shouldBe empty

      val List(arrowCall, dotCall) = cpg.call.nameExact(Defines.OperatorPointerCall).l
      arrowCall.code shouldBe "(s->*m)(x)"
      arrowCall.methodFullName shouldBe Defines.OperatorPointerCall
      arrowCall.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH
      arrowCall.typeFullName shouldBe "int"
      dotCall.code shouldBe "(s.*m)(x)"
      dotCall.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH
      dotCall.typeFullName shouldBe "int"
    }

    "let a query walk from the call to the member pointer it dispatches on" in {
      val List(arrowCall, dotCall) = cpg.call.nameExact(Defines.OperatorPointerCall).l

      val List(arrowReceiver) = arrowCall.receiver.l
      arrowReceiver.asInstanceOf[Call].name shouldBe
        Defines.OperatorMemberPointerArrow
      arrowReceiver.argumentIndex shouldBe -1

      val List(dotReceiver) = dotCall.receiver.l
      dotReceiver.asInstanceOf[Call].name shouldBe
        Defines.OperatorMemberPointerDot

      cpg.call
        .nameExact(Defines.OperatorPointerCall)
        .receiver
        .isCall
        .argument(2)
        .code
        .l shouldBe List("m", "m")
    }

    "be a pointer call in a template, where CDT resolves no type for the member pointer" in {
      val cpg = code("""
          |template <typename T, typename M, typename A>
          |int invoke(T* obj, M method, A arg) { return (obj->*method)(arg); }
          |""".stripMargin)

      cpg.call.nameExact(Defines.OperatorCall).l shouldBe empty

      val List(call) = cpg.call.nameExact(Defines.OperatorPointerCall).l
      call.code shouldBe "(obj->*method)(arg)"
      call.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH

      val List(receiver) = call.receiver.l
      receiver.asInstanceOf[Call].name shouldBe Defines.OperatorMemberPointerArrow
    }
  }

}
