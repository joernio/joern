package io.joern.c2cpg.passes.ast

import io.joern.c2cpg.astcreation.Defines
import io.joern.c2cpg.testfixtures.AstC2CpgSuite
import io.shiftleft.codepropertygraph.generated.ModifierTypes
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.semanticcpg.language.*

class NoExceptTests extends AstC2CpgSuite(".cpp") {

  "The noexcept specifier" should {

    "be recorded as a modifier on function definitions" in {
      val cpg = code("""
         |int f_plain(int x) { return x + 1; }
         |int f_noexcept(int x) noexcept { return x + 2; }
         |int f_noexcept_true(int x) noexcept(true) { return x + 3; }
         |int f_noexcept_false(int x) noexcept(false) { return x + 4; }
         |int d_legacy(int x) throw() { return x + 5; }
         |int d_legacy_int(int x) throw(int) { return x + 6; }
         |""".stripMargin)
      def modifiers(name: String) = cpg.method.nameExact(name).modifier.modifierType.l
      modifiers("f_noexcept") shouldBe List(Defines.NoExceptModifier)
      modifiers("f_noexcept_true") shouldBe List(Defines.NoExceptModifier)
      modifiers("d_legacy") shouldBe List(Defines.NoExceptModifier)
      modifiers("f_plain") shouldBe empty
      modifiers("f_noexcept_false") shouldBe empty
      modifiers("d_legacy_int") shouldBe empty
    }

    "be recorded as a modifier on member function declarations next to static and const" in {
      val cpg = code("""
         |struct Q {
         |  static void q_static();
         |  void q_const() const;
         |  void q_noexcept() noexcept;
         |  void q_const_noexcept() const noexcept;
         |  void q_plain();
         |};
         |""".stripMargin)
      def modifiers(name: String) = cpg.method.nameExact(name).modifier.modifierType.l
      modifiers("q_static") shouldBe List(ModifierTypes.STATIC)
      modifiers("q_const") shouldBe empty
      modifiers("q_noexcept") shouldBe List(Defines.NoExceptModifier)
      modifiers("q_const_noexcept") shouldBe List(Defines.NoExceptModifier)
      modifiers("q_plain") shouldBe empty
      cpg.method.nameExact("q_const_noexcept").signature.l shouldBe List(s"void()${Defines.ConstSuffix}")
    }

    "be recorded for a macro-spelled specifier, which METHOD.CODE cannot show" in {
      val cpg = code("""
         |#define MY_NOEXCEPT noexcept
         |int g(int x) MY_NOEXCEPT { return x; }
         |""".stripMargin)
      cpg.method.nameExact("g").code.l shouldBe List("int g(int x) MY_NOEXCEPT { return x; }")
      cpg.method.nameExact("g").modifier.modifierType.l shouldBe List(Defines.NoExceptModifier)
    }

    "not be reported for a dependent condition that cannot be evaluated" in {
      val cpg = code("""
         |template <class T> int f_dep(T x) noexcept(noexcept(T(x))) { return 0; }
         |""".stripMargin)
      cpg.method.nameExact("f_dep").modifier.modifierType.l shouldBe empty
    }

    "leave C function declarations alone" in {
      val cpg = code("int f(int x) { return x; }", "file.c")
      cpg.method.nameExact("f").modifier.modifierType.l shouldBe empty
    }

  }

  "The noexcept operator" should {

    "be a named call of type bool" in {
      val cpg = code("""
         |int danger(int);
         |bool probe() { return noexcept(danger(1)); }
         |""".stripMargin)
      inside(cpg.call.nameExact(Defines.OperatorNoExcept).l) { case List(call) =>
        call.code shouldBe "noexcept(danger(1))"
        call.typeFullName shouldBe "bool"
        call.methodFullName shouldBe Defines.OperatorNoExcept
      }
      cpg.call.nameExact(Defines.OperatorUnknown).l shouldBe empty
    }

    "not change the type of the other unary operators" in {
      val cpg = code("""
         |void foo(int x) {
         |  int a = -x;
         |  int b = sizeof(x);
         |  int* c = &x;
         |}
         |""".stripMargin)
      cpg.call.nameExact(Operators.minus).typeFullName.l shouldBe List(Defines.Any)
      cpg.call.nameExact(Operators.sizeOf).typeFullName.l shouldBe List(Defines.Any)
      cpg.call.nameExact(Operators.addressOf).typeFullName.l shouldBe List(Defines.Any)
    }

  }

}
