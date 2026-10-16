package io.joern.c2cpg.passes.types

import io.joern.c2cpg.parser.FileDefaults
import io.joern.c2cpg.testfixtures.C2CpgSuite
import io.shiftleft.codepropertygraph.generated.{ModifierTypes, Operators}
import io.shiftleft.semanticcpg.language.*

class EnumReferenceTests extends C2CpgSuite {

  "Enumerators" should {
    "link initializer targets and value uses to their enum members" in {
      val cpg = code("""
        |enum mode { zero, first = 1, second = first + 1 };
        |int value(void) { return second; }
        |""".stripMargin)
      val first             = cpg.member.nameExact("first").head
      val second            = cpg.member.nameExact("second").head
      val firstInitializer  = cpg.assignment.target.isIdentifier.nameExact("first").head
      val secondInitializer = cpg.assignment.target.isIdentifier.nameExact("second").head

      first.astChildren.isModifier.modifierType.l shouldBe List(ModifierTypes.FINAL)
      second.astChildren.isModifier.modifierType.l shouldBe List(ModifierTypes.FINAL)
      firstInitializer.refsTo.l shouldBe List(first)
      secondInitializer.refsTo.l shouldBe List(second)
      cpg.call.nameExact(Operators.addition).argument(1).isIdentifier.refsTo.l shouldBe List(first)
      cpg.method.nameExact("value").ast.isIdentifier.nameExact("second").refsTo.l shouldBe List(second)
      cpg.local.nameExact("first").l shouldBe empty
      cpg.local.nameExact("second").l shouldBe empty
    }

    "preserve parameter and local shadowing" in {
      val cpg = code("""
        |enum { action = 1 };
        |int parameter(int action) { return action; }
        |int local(void) { int action = 2; return action; }
        |int enumeration(void) { return action; }
        |""".stripMargin)
      val member    = cpg.member.nameExact("action").head
      val parameter = cpg.method.nameExact("parameter").parameter.nameExact("action").head
      val local     = cpg.method.nameExact("local").local.nameExact("action").head

      cpg.method.nameExact("parameter").ast.isIdentifier.nameExact("action").refsTo.l shouldBe List(parameter)
      cpg.method.nameExact("local").ast.isIdentifier.nameExact("action").refsTo.l.distinct shouldBe List(local)
      cpg.method.nameExact("enumeration").ast.isIdentifier.nameExact("action").refsTo.l shouldBe List(member)
    }

    "distinguish enumerators shadowed by a nested enum" in {
      val cpg = code("""
        |enum { VALUE = 1 };
        |int value(void) {
        |  int result = VALUE;
        |  {
        |    enum { VALUE = 2 };
        |    result += VALUE;
        |  }
        |  return result + VALUE;
        |}
        |""".stripMargin)
      val outer = cpg.member.nameExact("VALUE").codeExact("VALUE = 1").head
      val inner = cpg.member.nameExact("VALUE").codeExact("VALUE = 2").head

      cpg.call.nameExact(Operators.assignmentPlus).argument(2).isIdentifier.refsTo.l shouldBe List(inner)
      cpg.method.nameExact("value").ast.isReturn.ast.isIdentifier.nameExact("VALUE").refsTo.l shouldBe List(outer)
    }
  }
}

class CppEnumReferenceTests extends C2CpgSuite(fileSuffix = FileDefaults.CppExt) {

  "Enumerators" should {
    "link unqualified C++ enum uses through their CDT declaration binding" in {
      val cpg = code("""
        |namespace colors {
        |  enum color { red = 1, blue = red + 1 };
        |  int value() { return blue; }
        |}
        |""".stripMargin)
      val red  = cpg.member.nameExact("red").head
      val blue = cpg.member.nameExact("blue").head

      cpg.call.nameExact(Operators.addition).argument(1).isIdentifier.refsTo.l shouldBe List(red)
      cpg.method.nameExact("value").ast.isIdentifier.nameExact("blue").refsTo.l shouldBe List(blue)
    }
  }
}
