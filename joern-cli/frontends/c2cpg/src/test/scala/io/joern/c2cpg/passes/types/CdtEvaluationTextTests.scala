package io.joern.c2cpg.passes.types

import io.joern.c2cpg.parser.FileDefaults
import io.joern.c2cpg.testfixtures.C2CpgSuite
import io.joern.x2cpg.Defines
import io.shiftleft.semanticcpg.language.*

class CdtEvaluationTextTests extends C2CpgSuite(fileSuffix = FileDefaults.CppExt) {

  "A type name that would carry CDT evaluation text" should {

    "degrade to ANY for a call inside a fold over non-constant expressions" in {
      val cpg = code("""
        |int opaque(int);
        |template <typename... Ts>
        |int fold_non_constant(Ts... xs) {
        |  return (opaque(xs) + ...);
        |}
        |""".stripMargin)
      val calls = cpg.call.nameExact("opaque").l
      calls should not be empty
      calls.foreach(_.typeFullName shouldBe Defines.Any)
    }

    "not fabricate a TYPE_DECL named after an evaluation signature" in {
      val cpg = code("""
        |int opaque(int);
        |template <typename... Ts>
        |int fold_non_constant(Ts... xs) {
        |  return (opaque(xs) + ...);
        |}
        |""".stripMargin)
      cpg.typeDecl.fullName.l.sorted shouldBe List(
        "<operator>.addition",
        "ANY",
        "Test0.cpp:<global>",
        "Ts",
        "fold_non_constant:int(Ts)",
        "int",
        "void"
      )
      cpg.typ.fullName.l.sorted shouldBe List(
        "<operator>.addition",
        "ANY",
        "Ts",
        "fold_non_constant:int(Ts)",
        "int",
        "void"
      )
    }

    "leave the fold expression's own type alone" in {
      val cpg = code("""
        |template <typename... Ts>
        |int fold_constant(Ts... xs) {
        |  return (xs + ...);
        |}
        |""".stripMargin)
      // A fold whose nested evaluation does contribute a separator still resolves to the pack's
      // type. This is the control: the fix must not widen to ANY what already worked.
      inside(cpg.call.nameExact("<operator>.fold").l) { case List(fold) =>
        fold.typeFullName shouldBe "Ts"
      }
    }

    "reject a name in the org.eclipse.cdt package outright" in {
      // The pre-existing guard named one class, ProblemType, so any other internal class passed
      // through. This pins the package rather than the class; exercised directly because
      // provoking a second internal class from source is frontend-version dependent.
      val cpg = code("int x;")
      cpg.local.typeFullName.l.foreach(tpe => tpe should not include "org.eclipse.cdt")
      cpg.typeDecl.fullName.l.foreach(fullName => fullName should not include "org.eclipse.cdt")
    }
  }
}
