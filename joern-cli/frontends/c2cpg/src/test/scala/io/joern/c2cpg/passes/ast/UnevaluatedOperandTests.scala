package io.joern.c2cpg.passes.ast

import io.joern.c2cpg.astcreation.Defines
import io.joern.c2cpg.parser.FileDefaults
import io.joern.c2cpg.testfixtures.C2CpgSuite
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.{Identifier, MethodRef, Unknown}
import io.shiftleft.semanticcpg.language.*

class UnevaluatedOperandTests extends C2CpgSuite(FileDefaults.CppExt) {

  implicit val resolver: NoResolve.type = NoResolve

  "An unevaluated operand" should {
    val cpg = code("""
        |int danger(int);
        |bool probe() { return noexcept(danger(1)); }
        |unsigned long weigh() { return sizeof(danger(2)); }
        |int real(int x) { return danger(x); }
        |""".stripMargin)

    "lower the operand of noexcept without a call" in {
      inside(cpg.call.nameExact(Defines.OperatorNoExcept).l) { case List(noExceptCall) =>
        noExceptCall.methodFullName shouldBe Defines.OperatorNoExcept
        noExceptCall.typeFullName shouldBe "bool"
        inside(noExceptCall.argument.l) { case List(operand: Unknown) =>
          operand.code shouldBe "danger(1)"
          operand.typeFullName shouldBe "int"
        }
      }
    }

    "lower the operand of sizeof without a call" in {
      inside(cpg.call.nameExact(Operators.sizeOf).l) { case List(sizeOfCall) =>
        sizeOfCall.methodFullName shouldBe Operators.sizeOf
        inside(sizeOfCall.argument.l) { case List(operand: Unknown) =>
          operand.code shouldBe "danger(2)"
          operand.typeFullName shouldBe "int"
        }
      }
    }

    "keep the evaluated call" in {
      inside(cpg.call.nameExact("danger").l) { case List(call) =>
        call.code shouldBe "danger(x)"
        call.methodFullName shouldBe "danger:int(int)"
        call.signature shouldBe "int(int)"
        call.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
        call.typeFullName shouldBe "int"
        call.method.fullName shouldBe "real:int(int)"
      }
    }

    "report the evaluated call as the only incoming call of danger" in {
      cpg.method.nameExact("danger").fullName.l shouldBe List("danger:int(int)")
      cpg.method.nameExact("danger").callIn.code.l shouldBe List("danger(x)")
    }
  }

  "The operand of __alignof__" should {
    val cpg = code("""
        |int danger(int);
        |unsigned long weigh() { return __alignof__(danger(1)); }
        |""".stripMargin)

    "be lowered without a call" in {
      cpg.call.code.l shouldBe List("__alignof__(danger(1))")
      inside(cpg.call.codeExact("__alignof__(danger(1))").argument.l) { case List(operand: Unknown) =>
        operand.code shouldBe "danger(1)"
        operand.typeFullName shouldBe "int"
      }
    }
  }

  "A member call inside an unevaluated operand" should {
    val cpg = code("""
        |struct Widget {
        |  int scale(int factor) { return factor; }
        |};
        |unsigned long weigh(Widget& w) { return sizeof(w.scale(1)); }
        |int real(Widget& w) { return w.scale(2); }
        |""".stripMargin)

    "lower the operand without a call" in {
      inside(cpg.call.nameExact(Operators.sizeOf).argument.l) { case List(operand: Unknown) =>
        operand.code shouldBe "w.scale(1)"
        operand.typeFullName shouldBe "int"
      }
    }

    "report the evaluated call as the only incoming call of the member" in {
      cpg.method.nameExact("scale").fullName.l shouldBe List("Widget.scale:int(int)")
      cpg.method.nameExact("scale").callIn.code.l shouldBe List("w.scale(2)")
    }
  }

  "The operand of typeid" should {
    val cpg = code("""
        |int danger(int);
        |void probe() { typeid(danger(3)); }
        |""".stripMargin)

    "stay a call, because [expr.typeid] leaves it unevaluated only conditionally" in {
      cpg.call.nameExact(Defines.OperatorTypeOf).code.l shouldBe List("typeid(danger(3))")
      cpg.call.nameExact("danger").code.l shouldBe List("danger(3)")
      cpg.method.nameExact("danger").callIn.code.l shouldBe List("danger(3)")
    }
  }

  "A lambda written inside an unevaluated operand" should {
    val cpg = code("""
        |int danger(int);
        |unsigned long weigh() { return sizeof([] { return danger(4); }); }
        |""".stripMargin)

    "keep the call in the lambda body" in {
      cpg.call.nameExact("danger").code.l shouldBe List("danger(4)")
      cpg.method.nameExact("danger").callIn.code.l shouldBe List("danger(4)")
    }

    "keep the operand as a method reference to the lambda" in {
      inside(cpg.call.nameExact(Operators.sizeOf).argument.l) { case List(operand: MethodRef) =>
        operand.referencedMethod.name shouldBe "<lambda>0"
      }
    }
  }

  "A call whose argument holds an unevaluated operand" should {
    val cpg = code("""
        |void* allocate(unsigned long);
        |void* probe(int n) { return allocate(sizeof(n)); }
        |""".stripMargin)

    "keep the enclosing call" in {
      inside(cpg.call.nameExact("allocate").l) { case List(call) =>
        call.code shouldBe "allocate(sizeof(n))"
        call.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      }
      cpg.method.nameExact("allocate").callIn.code.l shouldBe List("allocate(sizeof(n))")
    }

    "keep an identifier operand as an identifier" in {
      inside(cpg.call.nameExact(Operators.sizeOf).argument.l) { case List(operand: Identifier) =>
        operand.name shouldBe "n"
        operand.typeFullName shouldBe "int"
      }
    }
  }

  "An unevaluated operand whose type CDT cannot resolve" should {
    val cpg = code("""
        |unsigned long weigh() { return sizeof(missing(5)); }
        |""".stripMargin)

    "fall back to the unresolved type" in {
      inside(cpg.call.nameExact(Operators.sizeOf).argument.l) { case List(operand: Unknown) =>
        operand.code shouldBe "missing(5)"
        operand.typeFullName shouldBe Defines.Any
      }
    }
  }

  "An unevaluated operand in C" should {
    val cpg = code(
      """
        |int danger(int);
        |unsigned long weigh(void) { return sizeof(danger(2)); }
        |int real(int x) { return danger(x); }
        |""".stripMargin,
      "file.c"
    )

    "lower the operand without a call" in {
      inside(cpg.call.nameExact(Operators.sizeOf).argument.l) { case List(operand: Unknown) =>
        operand.code shouldBe "danger(2)"
        operand.typeFullName shouldBe "int"
      }
    }

    "keep the evaluated call" in {
      cpg.call.nameExact("danger").code.l shouldBe List("danger(x)")
    }
  }

}
