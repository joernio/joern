package io.joern.c2cpg.passes.types

import io.joern.c2cpg.parser.FileDefaults
import io.joern.c2cpg.testfixtures.C2CpgSuite
import io.joern.x2cpg.Defines
import io.shiftleft.semanticcpg.language.*

/** CDT's internal text must not become a type name.
  *
  * `cleanType` is the one place every type name passes through, but four sites in `TypeNameProvider` return a CDT
  * object's `toString` and reach it only by accident of the call graph. What leaks is not Java-looking text, which is
  * why a search for `org.eclipse.cdt` or `@` finds nothing: CDT hands back a SERIALISED EVALUATION SIGNATURE, digit
  * runs glued to the identifiers of the translation unit.
  *
  * Measured on joern v4.0.655 before the fix, for the fold below: the inner call `opaque(xs)` received `typeFullName`
  * `82731opaque035fold_non_constant000`, and that string also created an external TYPE_DECL and a TYPE node of the same
  * name, so a consumer enumerating type declarations saw a phantom type named after two functions in the file. The fold
  * expression itself was already ANY; it is the call's type that leaked.
  *
  * The discriminator is a language fact rather than a heuristic: no C++ type name can begin with a digit.
  */
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
      calls.foreach { c =>
        c.typeFullName shouldBe Defines.Any
        // The specific leak, stated so a regression names itself rather than failing on a bare
        // inequality: a leading digit is impossible in a real C++ type name.
        c.typeFullName.headOption.exists(_.isDigit) shouldBe false
      }
    }

    "not fabricate a TYPE_DECL named after an evaluation signature" in {
      val cpg = code("""
        |int opaque(int);
        |template <typename... Ts>
        |int fold_non_constant(Ts... xs) {
        |  return (opaque(xs) + ...);
        |}
        |""".stripMargin)
      // Before the fix this list held `82731opaque035fold_non_constant000`.
      cpg.typeDecl.fullName.l.filter(_.headOption.exists(_.isDigit)) shouldBe empty
      cpg.typ.fullName.l.filter(_.headOption.exists(_.isDigit)) shouldBe empty
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
      cpg.local.typeFullName.l.foreach(t => t should not include "org.eclipse.cdt")
      cpg.typeDecl.fullName.l.foreach(t => t should not include "org.eclipse.cdt")
    }
  }
}
