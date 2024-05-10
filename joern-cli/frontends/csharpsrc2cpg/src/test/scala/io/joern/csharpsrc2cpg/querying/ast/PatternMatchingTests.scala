package io.joern.csharpsrc2cpg.querying.ast

import io.joern.csharpsrc2cpg.testfixtures.CSharpCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Identifier, Literal, TypeRef}
import io.shiftleft.codepropertygraph.generated.{ControlStructureTypes, NodeTypes, Operators}
import io.shiftleft.semanticcpg.language.*

class PatternMatchingTests extends CSharpCode2CpgFixture {

  "Pattern matching to extract the non-null value in an if-statement" should {
    val cpg = code(basicBoilerplate("""
        |int? maybe = 12;
        |
        |if (maybe is int number)
        |{
        |    Console.WriteLine($"The nullable int 'maybe' has the value {number}");
        |}
        |else
        |{
        |    Console.WriteLine("The nullable int 'maybe' doesn't hold a value");
        |}
        |""".stripMargin))

    "lower an assignment from `maybe` to `number` as the first statement of the if-body" in {
      inside(cpg.assignment.where(_.target.isIdentifier.name("number")).headOption) {
        case Some(assignment) =>
          assignment.order shouldBe 1
          assignment.inAst.exists(_.label == NodeTypes.CONTROL_STRUCTURE) shouldBe true

          inside(assignment.argument.l) {
            case (number: Identifier) :: (maybe: Identifier) :: Nil =>
              number.name shouldBe "number"
              number.typeFullName shouldBe "System.Int32"

              maybe.name shouldBe "maybe"
              maybe.typeFullName shouldBe "System.Int32"
            case xs => fail(s"Expected two identifier arguments, instead got [${xs.code.mkString(",")}]")
          }

        case None => fail("Expected an assignment `number = maybe`")
      }
    }

    "have an instanceOf-style check as the if-condition" in {
      inside(cpg.controlStructure.controlStructureType(ControlStructureTypes.IF).condition.headOption) {
        case Some(condition: Call) =>
          condition.name shouldBe Operators.instanceOf
          inside(condition.argument.l) {
            case (maybe: Identifier) :: (intType: TypeRef) :: Nil =>
              maybe.name shouldBe "maybe"
              maybe.typeFullName shouldBe "System.Int32"

              intType.typeFullName shouldBe "System.Int32"
            case xs =>
              fail(
                s"Expected an identifier and type ref argument to `instanceOf`, instead got [${xs.code.mkString(",")}]"
              )
          }
        case _ => fail("Expected an if-statement with a condition call")

      }
    }
  }

  "Pattern matching with null type check" should {
    val cpg = code(basicBoilerplate("""
      |int? maybe = 12;
      |
      |if (maybe is null)
      |{
      |    Console.WriteLine($"The nullable int 'maybe' has the value {number}");
      |}
      |""".stripMargin))

    "have equals check in if statement" in {
      inside(cpg.controlStructure.controlStructureType(ControlStructureTypes.IF).condition.headOption) {
        case Some(condition: Call) =>
          condition.name shouldBe Operators.equals

          inside(condition.argument.l) {
            case (maybe: Identifier) :: (nullType: Literal) :: Nil =>
              maybe.name shouldBe "maybe"
              maybe.typeFullName shouldBe "System.Int32"

              nullType.typeFullName shouldBe "null"
            case xs => fail(s"Expect identifier and literal, instead got [${xs.code.mkString(", ")}]")
          }
        case _ => fail("Expected an if-statement with condition call")
      }
    }
  }

}
