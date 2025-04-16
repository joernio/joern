package io.joern.rubysrc2cpg.querying

import io.joern.x2cpg.Defines
import io.joern.x2cpg.frontendspecific.rubysrc2cpg.Constants
import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.{ControlStructureTypes, DispatchTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.{Block, Call, FieldIdentifier, Identifier, Literal}
import io.shiftleft.semanticcpg.language.*

class ErbTests extends RubyCode2CpgFixture {
  "Complete ERB processing" should {
    val cpg = code(
      """
        |app_name: <%= ENV['APP_NAME'] %>
        |version: <%= ENV['APP_VERSION'] %>
        |
        |database:
        |  host: <%= ENV['DB_HOST'] %>
        |  port: <%= ENV['DB_PORT'] %>
        |
        |<% if ENV['USE_REDIS'] == 'true' %>
        |redis:
        |  host: <%= ENV['REDIS_HOST'] %>
        |  port: <%= ENV['REDIS_PORT'] %>
        |<% end %>
        |""".stripMargin,
      "test.erb"
    )

    "Contain a call to <operator>.conditional" in {
      inside(cpg.call.name(Operators.conditional).l) {
        case _ :: Nil =>
        // Do nothing - contains call to operator.conditional
        case _ => fail("AST Should contain one if statement")
      }
    }

    "Contain a call to fmtString in the true branch of the <operator>.conditional" in {
      inside(cpg.call.name(Operators.conditional).argument.l) {
        case (condition: Call) :: (trueBranch: Block) :: _ :: Nil =>
          condition.code shouldBe "ENV['USE_REDIS'] == 'true'"
          condition.methodFullName shouldBe Operators.equals

          inside(trueBranch.astChildren.isCall.l) {
            case fmtStringCall :: Nil =>
              fmtStringCall.methodFullName shouldBe Operators.formatString
              fmtStringCall.argument.l.size shouldBe 4
              val List(fmtVal1: Call, fmtVal2: Call, fmtVal3: Call, fmtVal4: Call) =
                fmtStringCall.argument.l: @unchecked

              fmtVal1.methodFullName shouldBe Operators.formattedValue
              fmtVal1.code shouldBe
                """redis:
                  |  host: """.stripMargin

              fmtVal2.methodFullName shouldBe Operators.formattedValue
              fmtVal2.code shouldBe "#{ENV['REDIS_HOST']}"

              fmtVal3.methodFullName shouldBe Operators.formattedValue
              fmtVal3.code shouldBe "  port: "

              fmtVal4.methodFullName shouldBe Operators.formattedValue
              fmtVal4.code shouldBe "#{ENV['REDIS_PORT']}"
            case xs => fail(s"Expected one call to fmtString, got [${xs.mkString(",")}]")
          }
        case xs => fail(s"Expected three arguments, got ${xs.size}: [${xs.mkString(",")}] instead")
      }
    }

    "Contains calls to formattedValue" in {
      cpg.call.name(Operators.formattedValue).l.size shouldBe 14
    }
  }

  "ERB With If-Elsif contains IF-Struct" in {
    val cpg = code(
      """
        |app_name: <%= ENV['APP_NAME'] %>
        |version: <%= ENV['APP_VERSION'] %>
        |
        |database:
        |  host: <%= ENV['DB_HOST'] %>
        |  port: <%= ENV['DB_PORT'] %>
        |
        |<% if ENV['USE_REDIS'] == 'true' %>
        |redis:
        |  host: <%= ENV['REDIS_HOST'] %>
        |  port: <%= ENV['REDIS_PORT'] %>
        |<% elsif ENV['USE_RABBITMQ'] == 'true' %>
        |rabbitmq:
        |  host: <%= ENV['RABBITMQ_HOST'] %>
        |  port: <%= ENV['RABBITMQ_PORT'] %>
        |<% end %>
        |""".stripMargin,
      "test.erb"
    )

    inside(cpg.controlStructure.controlStructureType(ControlStructureTypes.IF).l) {
      case ifStruct :: _ :: Nil =>
        inside(ifStruct.condition.l) {
          case (cond: Call) :: Nil =>
            cond.methodFullName shouldBe Operators.equals
            val List(lhs: Call, rhs: Literal) = cond.argument.l: @unchecked
            lhs.methodFullName shouldBe Operators.indexAccess
            rhs.code shouldBe "'true'"
          case xs => fail(s"Expected one condition, got [${xs.code.mkString(",")}]")
        }

        inside(ifStruct.whenTrue.isBlock.astChildren.isCall.l) {
          case fmtStringCall :: Nil =>
            fmtStringCall.methodFullName shouldBe Operators.formatString
            fmtStringCall.argument.l.size shouldBe 4
            val List(fmtVal1: Call, fmtVal2: Call, fmtVal3: Call, fmtVal4: Call) = fmtStringCall.argument.l: @unchecked

            fmtVal1.methodFullName shouldBe Operators.formattedValue
            fmtVal1.code shouldBe
              """redis:
                |  host: """.stripMargin

            fmtVal2.methodFullName shouldBe Operators.formattedValue
            fmtVal2.code shouldBe "#{ENV['REDIS_HOST']}"

            fmtVal3.methodFullName shouldBe Operators.formattedValue
            fmtVal3.code shouldBe """  port: """

            fmtVal4.methodFullName shouldBe Operators.formattedValue
            fmtVal4.code shouldBe "#{ENV['REDIS_PORT']}"

          case xs => fail(s"Expected one body for true branch, got [${xs.code.mkString(",")}]")
        }

        inside(ifStruct.whenFalse.isBlock.astChildren.isControlStructure.l) {
          case elsifStruct :: Nil =>
            inside(elsifStruct.condition.l) {
              case (cond: Call) :: Nil =>
                cond.methodFullName shouldBe Operators.equals
                val List(lhs: Call, rhs: Literal) = cond.argument.l: @unchecked
                lhs.methodFullName shouldBe Operators.indexAccess
                rhs.code shouldBe "'true'"
              case xs => fail(s"Expected one condition, got [${xs.code.mkString(",")}]")
            }

            inside(elsifStruct.whenTrue.isBlock.astChildren.isCall.l) {
              case fmtStringCall :: Nil =>
                fmtStringCall.methodFullName shouldBe Operators.formatString
                fmtStringCall.argument.l.size shouldBe 4
                val List(fmtVal1: Call, fmtVal2: Call, fmtVal3: Call, fmtVal4: Call) =
                  fmtStringCall.argument.l: @unchecked

                fmtVal1.methodFullName shouldBe Operators.formattedValue
                fmtVal1.code shouldBe
                  """rabbitmq:
                    |  host: """.stripMargin

                fmtVal2.methodFullName shouldBe Operators.formattedValue
                fmtVal2.code shouldBe "#{ENV['RABBITMQ_HOST']}"

                fmtVal3.methodFullName shouldBe Operators.formattedValue
                fmtVal3.code shouldBe "  port: "

                fmtVal4.methodFullName shouldBe Operators.formattedValue
                fmtVal4.code shouldBe "#{ENV['RABBITMQ_PORT']}"
              case xs => fail(s"Expected one body with call for true branch, got [${xs.code.mkString(",")}]")
            }
          case _ => fail(s"Expected one IF struct in false branch")
        }

      case xs => fail(s"Expected two IF Structures, got [${xs.code.mkString(",")}]")
    }
  }

  "Invalid ERB processing" in {
    val cpg = code(
      """
        |app_name: <%= ENV['APP_NAME'] %>
        |version: <%= ENV['APP_VERSION'] %>
        |
        |database:
        |  host: <%= ENV['DB_HOST'] %>
        |  port: <%= ENV['DB_PORT'] %>
        |
        |<% if ENV['USE_REDIS'] == 'true' %>
        |redis:
        |  host: <%= ENV['REDIS_HOST'] %>
        |  port: <%= ENV['REDIS_PORT'] %>
        |""".stripMargin,
      "test.erb"
    )

    inside(cpg.method.name(Constants.Main).body.astChildren.isCall.l) {
      case fmtString :: Nil =>
        fmtString.methodFullName shouldBe Operators.formatString
      case xs => fail(s"Expected one call to fmtString, got [${xs.code.mkString(",")}]")
    }
  }
}
