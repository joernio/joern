package io.joern.rubysrc2cpg.querying

import io.joern.rubysrc2cpg.passes.Defines.RubyOperators
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
        |version: <%== ENV['APP_VERSION'] %>
        |
        |database:
        |  host: <%= ENV['DB_HOST'] %>
        |  port: <%== ENV['DB_PORT'] %>
        |
        |<% if ENV['USE_REDIS'] == 'true' %>
        |redis:
        |  host: <%= ENV['REDIS_HOST'] %>
        |  port: <%== ENV['REDIS_PORT'] %>
        |<% end %>
        |""".stripMargin,
      "test.erb"
    )

    "Contain a RETURN node" in {
      inside(cpg.method.name(Constants.Main).methodReturn.toReturn.l) {
        case erbReturn :: Nil =>
          val List(formatStringReturn: Call) = erbReturn.astChildren.l: @unchecked
          formatStringReturn.methodFullName shouldBe Operators.formatString
        case xs => fail(s"Expected one method return, got ${xs.code.mkString("[", ",", "]")}")
      }
    }

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
              val List(fmtVal1: Call, templateOutEscapeCall: Call, fmtVal3: Call, templateOutRawCall: Call) =
                fmtStringCall.argument.l: @unchecked

              fmtVal1.methodFullName shouldBe Operators.formattedValue
              fmtVal1.code shouldBe
                """redis:
                  |  host: """.stripMargin

              templateOutEscapeCall.methodFullName shouldBe RubyOperators.templateOutEscape
              templateOutEscapeCall.code shouldBe "<%= ENV['REDIS_HOST'] %>"

              fmtVal3.methodFullName shouldBe Operators.formattedValue
              fmtVal3.code shouldBe "  port: "

              templateOutRawCall.methodFullName shouldBe RubyOperators.templateOutRaw
              templateOutRawCall.code shouldBe "<%== ENV['REDIS_PORT'] %>"
            case xs => fail(s"Expected one call to fmtString, got [${xs.mkString(",")}]")
          }
        case xs => fail(s"Expected three arguments, got ${xs.size}: [${xs.mkString(",")}] instead")
      }
    }

    "Contains calls to formattedValue" in {
      cpg.call.name(Operators.formattedValue).l.size shouldBe 8
    }

    "Contains calls to templateOutRaw" in {
      cpg.call.name(RubyOperators.templateOutRaw).l.size shouldBe 3
    }

    "Contains calls to templateOutEscape" in {
      cpg.call.name(RubyOperators.templateOutEscape).l
    }
  }

  "ERB With If-Elsif contains IF-Struct" in {
    val cpg = code(
      """
        |app_name: <%= ENV['APP_NAME'] %>
        |version: <%== ENV['APP_VERSION'] %>
        |
        |database:
        |  host: <%= ENV['DB_HOST'] %>
        |  port: <%== ENV['DB_PORT'] %>
        |
        |<% if ENV['USE_REDIS'] == 'true' %>
        |redis:
        |  host: <%= ENV['REDIS_HOST'] %>
        |  port: <%= ENV['REDIS_PORT'] %>
        |<% elsif ENV['USE_RABBITMQ'] == 'true' %>
        |rabbitmq:
        |  host: <%= ENV['RABBITMQ_HOST'] %>
        |  port: <%== ENV['RABBITMQ_PORT'] %>
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
            val List(fmtVal1: Call, templateOutEscapeOne: Call, fmtVal3: Call, templateOutEscapeTwo: Call) =
              fmtStringCall.argument.l: @unchecked

            fmtVal1.methodFullName shouldBe Operators.formattedValue
            fmtVal1.code shouldBe
              """redis:
                |  host: """.stripMargin

            templateOutEscapeOne.methodFullName shouldBe RubyOperators.templateOutEscape
            templateOutEscapeOne.code shouldBe "<%= ENV['REDIS_HOST'] %>"

            fmtVal3.methodFullName shouldBe Operators.formattedValue
            fmtVal3.code shouldBe """  port: """

            templateOutEscapeTwo.methodFullName shouldBe RubyOperators.templateOutEscape
            templateOutEscapeTwo.code shouldBe "<%= ENV['REDIS_PORT'] %>"

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
                val List(fmtVal1: Call, templateOutEscape: Call, fmtVal3: Call, templateOutRaw: Call) =
                  fmtStringCall.argument.l: @unchecked

                fmtVal1.methodFullName shouldBe Operators.formattedValue
                fmtVal1.code shouldBe
                  """rabbitmq:
                    |  host: """.stripMargin

                templateOutEscape.methodFullName shouldBe RubyOperators.templateOutEscape
                templateOutEscape.code shouldBe "<%= ENV['RABBITMQ_HOST'] %>"

                fmtVal3.methodFullName shouldBe Operators.formattedValue
                fmtVal3.code shouldBe "  port: "

                templateOutRaw.methodFullName shouldBe RubyOperators.templateOutRaw
                templateOutRaw.code shouldBe "<%== ENV['RABBITMQ_PORT'] %>"
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
