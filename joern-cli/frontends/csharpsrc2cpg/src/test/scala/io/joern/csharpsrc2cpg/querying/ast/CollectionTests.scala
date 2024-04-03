package io.joern.csharpsrc2cpg.querying.ast

import io.joern.csharpsrc2cpg.testfixtures.CSharpCode2CpgFixture
import io.joern.x2cpg.Defines
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Identifier, Literal}
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

    "<too-many-initializers> present" in {
      val cpg = code(basicBoilerplate(s"""
          |var a = {${("n" * 1002).mkString(",")}};
          |""".stripMargin))

      inside(cpg.literal.typeFullName(Defines.Any).l) {
        case tooManyInitializers :: Nil =>
          tooManyInitializers.code shouldBe "<too-many-initializers>"
          tooManyInitializers.typeFullName shouldBe Defines.Any
        case _ => fail("Too Many Initializers node expected")
      }
    }

    "be correct for static initialized 2D-array" in {
      val cpg = code(basicBoilerplate("""
                  |var a = { {1, 2, 3}, {4, 5, 6} };
                  |""".stripMargin))

      inside(cpg.call.name(Operators.assignment).l) { case assignment :: Nil =>
        inside(assignment.argument.l) {
          case (lhs: Identifier) :: (rhs: Call) :: Nil =>
            lhs.typeFullName shouldBe "System.Int32[][]"

            rhs.typeFullName shouldBe "System.Int32[][]"
            rhs.name shouldBe Operators.arrayInitializer

            inside(rhs.argument.isCall.l) {
              case arr1 :: arr2 :: Nil =>
                arr1.code shouldBe "{1, 2, 3}"
                arr1.typeFullName shouldBe "System.Int32[]"

                inside(arr1.argument.isLiteral.l) {
                  case elem1 :: elem2 :: elem3 :: Nil =>
                    elem1.typeFullName shouldBe "System.Int32"
                    elem1.code shouldBe "1"

                    elem2.typeFullName shouldBe "System.Int32"
                    elem2.code shouldBe "2"

                    elem3.typeFullName shouldBe "System.Int32"
                    elem3.code shouldBe "3"
                  case _ => fail("Only 3 elements in array expected")
                }

                arr2.code shouldBe "{4, 5, 6}"
                arr2.typeFullName shouldBe "System.Int32[]"
              case _ => fail("Expected 2 1D arrays")
            }
          case _ => fail("Only expected LHS and RHS")
        }
      }
    }

    "Collection static initialization" in {
      val cpg = code(basicBoilerplate("""
          | var list = [1, 2, 3];
          |""".stripMargin))

      inside(cpg.call.name(Operators.arrayInitializer).l) {
        case initializer :: Nil =>
          initializer.typeFullName shouldBe "System.List"
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

    "Collection static initialization on nested collection" in {
      val cpg = code(basicBoilerplate("""
          | var list = [[1, 2, 3], [4, 5, 6]];
          |""".stripMargin))

      inside(cpg.call.name(Operators.assignment).l) { case assignment :: Nil =>
        inside(assignment.argument.l) {
          case (lhs: Identifier) :: (rhs: Call) :: Nil =>
            lhs.typeFullName shouldBe "System.List"

            rhs.typeFullName shouldBe "System.List"
            rhs.name shouldBe Operators.arrayInitializer

            inside(rhs.argument.isCall.l) {
              case arr1 :: arr2 :: Nil =>
                arr1.code shouldBe "[1, 2, 3]"
                arr1.typeFullName shouldBe "System.List"

                inside(arr1.argument.isLiteral.l) {
                  case elem1 :: elem2 :: elem3 :: Nil =>
                    elem1.typeFullName shouldBe "System.Int32"
                    elem1.code shouldBe "1"

                    elem2.typeFullName shouldBe "System.Int32"
                    elem2.code shouldBe "2"

                    elem3.typeFullName shouldBe "System.Int32"
                    elem3.code shouldBe "3"
                  case _ => fail("Only 3 elements in list expected")
                }

                arr2.code shouldBe "[4, 5, 6]"
                arr2.typeFullName shouldBe "System.List"
              case _ => fail("Expected 2 1D Lists ")
            }
          case _ => fail("Only expected LHS and RHS")
        }
      }
    }

    "be correct for implicitly declared array" in {
      val cpg = code(basicBoilerplate("""
          |var foo = new[] {1, 2, 3};
          |""".stripMargin))

      inside(cpg.call.name(Operators.assignment).l) {
        case assignment :: Nil =>
          inside(assignment.argument.l) {
            case (lhs: Identifier) :: (rhs: Call) :: Nil =>
              lhs.typeFullName shouldBe "System.Int32[]"

              rhs.typeFullName shouldBe "System.Int32[]"
              rhs.name shouldBe Operators.arrayInitializer
              rhs.code shouldBe "{1, 2, 3}"

              inside(rhs.argument.isLiteral.l) {
                case elem1 :: elem2 :: elem3 :: Nil =>
                  elem1.typeFullName shouldBe "System.Int32"
                  elem1.code shouldBe "1"

                  elem2.typeFullName shouldBe "System.Int32"
                  elem2.code shouldBe "2"

                  elem3.typeFullName shouldBe "System.Int32"
                  elem3.code shouldBe "3"
                case _ => fail("Only 3 elements in list expected")
              }
            case _ => fail("Identifier on the LHS, and a call node on the RHS was expected.")
          }
        case _ => fail("One assignment was expected.")

      }
    }

    "Create index-access call" in {
      val cpg = code(basicBoilerplate("""
          |var foo = new[] {1, 2, 3};
          |foo[4] = 5;
          |""".stripMargin))

      inside(cpg.method.name("Main").l) {
        case mainMethod :: Nil =>
          inside(mainMethod.block.astChildren.isCall.l) {
            case arrInit :: assignmentCall :: Nil =>
              inside(assignmentCall.argument.l) {
                case (indexAccessCall: Call) :: (numLiteral: Literal) :: Nil =>
                  indexAccessCall.name shouldBe Operators.indexAccess

                  numLiteral.code shouldBe "5"

                  inside(indexAccessCall.argument.l) {
                    case (ident: Identifier) :: (index: Literal) :: Nil =>
                      ident.code shouldBe "foo"
                      ident.typeFullName shouldBe "System.Int32[]"

                      index.code shouldBe "4"
                    case xs => fail(s"Expected identifier and literal, got ${xs.code.mkString(", ")} instead")
                  }

                case xs => fail(s"Expected indexAccess and literal, got ${xs.code.mkString(", ")} instead")
              }
            case xs => fail(s"Expected 2 calls in main method, got ${xs.code.mkString(", ")} instead")
          }
        case xs => fail(s"Expected one Main method, got ${xs.name.mkString(", ")} instead")
      }
    }
  }

  "Dictionary AST" should {
    val cpg = code(basicBoilerplate("""
        |var dict = new Dictionary<string, string>();
        |dict["foo"] = "bar";
        |""".stripMargin))

    "Create index-access call" in {
      inside(cpg.method.name("Main").l) {
        case mainMethod :: Nil =>
          inside(mainMethod.block.astChildren.isCall.l) {
            case dictInit :: assignmentCall :: Nil =>
              inside(assignmentCall.argument.l) {
                case (indexAccessCall: Call) :: (barLiteral: Literal) :: Nil =>
                  indexAccessCall.name shouldBe Operators.indexAccess

                  barLiteral.code shouldBe "\"bar\""

                  inside(indexAccessCall.argument.l) {
                    case (ident: Identifier) :: (index: Literal) :: Nil =>
                      ident.code shouldBe "dict"
                      ident.typeFullName shouldBe "Dictionary"

                      index.code shouldBe "\"foo\""
                    case xs => fail(s"Expected identifier and literal, got ${xs.code.mkString(", ")} instead")
                  }

                case xs => fail(s"Expected indexAccess and literal, got ${xs.code.mkString(", ")} instead")
              }
            case xs => fail(s"Expected 2 calls in main method, got ${xs.code.mkString(", ")} instead")
          }
        case xs => fail(s"Expected one Main method, got ${xs.name.mkString(", ")} instead")
      }
    }
  }

}
