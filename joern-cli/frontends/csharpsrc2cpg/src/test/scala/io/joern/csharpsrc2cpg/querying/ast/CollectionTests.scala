package io.joern.csharpsrc2cpg.querying.ast

import io.joern.csharpsrc2cpg.testfixtures.CSharpCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.{ControlStructureTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.{Call, ControlStructure, Identifier, Literal}
import io.shiftleft.semanticcpg.language.*

class CollectionTests extends CSharpCode2CpgFixture {
  "Array AST" should {
    "be correct for static initialized array" in {
      val cpg = code(basicBoilerplate("""
                  |var a = {1, 2, 3};
                  |""".stripMargin))

      inside(cpg.call.name(Operators.arrayInitializer).l) {
        case initializer :: Nil =>
          initializer.typeFullName shouldBe "System.Int32[]"

          inside(initializer.argument.l) {
            case (var1: Literal) :: (var2: Literal) :: (var3: Literal) :: Nil =>
              var1.typeFullName shouldBe "System.Int32"
              var1.code shouldBe "1"

              var2.typeFullName shouldBe "System.Int32"
              var2.code shouldBe "2"

              var3.typeFullName shouldBe "System.Int32"
              var3.code shouldBe "3"
            case _ => fail("Only 3 variables expected for array")
          }
        case _ => fail("Only one initializer call expected")
      }
    }

    "be correct for static initialized 2D-array" in {
      val cpg = code(basicBoilerplate("""
                  |var a = { {1, 2, 3}, {4, 5, 6} };
                  |""".stripMargin))

      inside(cpg.call.name(Operators.assignment).l) { case assignment :: Nil =>
        inside(assignment.argument.l) {
          case (lhs: Identifier) :: (rhs: Call) :: Nil =>
            inside(rhs.argument.l) {
              case arr1 :: arr2 :: Nil =>
                println(arr1.code)
              case _ => fail("Expected 2 1D arrays")
            }
          case _ => fail("Only expected LHS and RHS")
        }
      }
    }
  }
}
