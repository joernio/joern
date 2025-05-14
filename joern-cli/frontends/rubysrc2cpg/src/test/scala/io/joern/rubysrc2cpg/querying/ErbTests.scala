package io.joern.rubysrc2cpg.querying

import io.joern.rubysrc2cpg.passes.Defines.RubyOperators
import io.joern.x2cpg.Defines
import io.joern.x2cpg.frontendspecific.rubysrc2cpg.Constants
import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.{ControlStructureTypes, DispatchTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.{Block, Call, FieldIdentifier, Identifier, Literal, TypeRef}
import io.shiftleft.semanticcpg.language.*

class ErbTests extends RubyCode2CpgFixture {
  "Basic ERB processing" should {
    val cpg = code(
      """
        |app_name: <%= ENV['APP_NAME'] %>
        |version: <%== ENV['APP_VERSION'] %>
        |
        |<% if ENV['USE_REDIS'] == 'true' %>
        |redis:
        |  host: <%= ENV['REDIS_HOST'] %>
        |<% end %>
        |""".stripMargin,
      "test.erb"
    )

    "Contain a RETURN node" in {
      inside(cpg.method.name(Constants.Main).methodReturn.toReturn.l) {
        case erbReturn :: Nil =>
          val List(buffer: Identifier) = erbReturn.astChildren.l: @unchecked
          buffer.code shouldBe "joern__buffer"
        case xs => fail(s"Expected one method return, got ${xs.code.mkString("[", ",", "]")}")
      }
    }

    "Contains 6 calls to append (<<)" in {
      cpg.call.name("<<").l.size shouldBe 6
    }

    "Condition for IF should be `equals`" in {
      inside(cpg.controlStructure.isIf.condition.l) {
        case (condition: Call) :: Nil =>
          condition.methodFullName shouldBe Operators.equals
        case xs => fail(s"Expected one condition for if, got ${xs.code.mkString("[", ",", "]")}")
      }
    }

    "True branch contains appends" in {
      inside(cpg.controlStructure.isIf.whenTrue.astChildren.isCall.l) {
        case appendCallOne :: appendCallTwo :: Nil =>
          appendCallOne.code shouldBe
            """joern__buffer << "redis:\nhost:"""".stripMargin

          appendCallTwo.code shouldBe "joern__buffer << <%= ENV['REDIS_HOST'] %>"

          inside(appendCallTwo.argument.l) {
            case argOne :: (argTwo: Call) :: Nil =>
              argOne.code shouldBe "joern__buffer"
              argTwo.code shouldBe "<%= ENV['REDIS_HOST'] %>"
              argTwo.methodFullName shouldBe RubyOperators.templateOutEscape
            case xs => fail(s"Expected two arguments for append call, got ${xs.code.mkString("[", ",", "]")}")
          }
        case xs => fail(s"Expected one true branch, got ${xs.code.mkString("[", ",", "]")}")
      }
    }

    "Contains calls to templateOutRaw" in {
      cpg.call.name(RubyOperators.templateOutRaw).l.size shouldBe 1
    }

    "Contains calls to templateOutEscape" in {
      cpg.call.name(RubyOperators.templateOutEscape).l.size shouldBe 2
    }
  }

  "ERB With If-Elsif lowers to IF-Struct" should {
    val cpg = code(
      """
        |database:
        |  host: <%= ENV['DB_HOST'] %>
        |
        |<% if ENV['USE_REDIS'] == 'true' %>
        |redis:
        |  host: <%= ENV['REDIS_HOST'] %>
        |<% elsif ENV['USE_RABBITMQ'] == 'true' %>
        |rabbitmq:
        |  port: <%== ENV['RABBITMQ_PORT'] %>
        |<% end %>
        |""".stripMargin,
      "test.erb"
    )

    "Contain IF ControlStruct" in {
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
            case appendCallStatic :: appendCallTemplate :: Nil =>
              appendCallStatic.argument.l.size shouldBe 2
              val List(callArgStaticOne, callArgStaticTwo) = appendCallStatic.argument.l: @unchecked
              callArgStaticOne.code shouldBe "joern__buffer"
              callArgStaticTwo.code shouldBe
                """redis:
                  |host:""".stripMargin

              appendCallTemplate.argument.l.size shouldBe 2
              val List(callArgOne, callArgTwo: Call) = appendCallTemplate.argument.l: @unchecked
              callArgOne.code shouldBe "joern__buffer"
              callArgTwo.methodFullName shouldBe RubyOperators.templateOutEscape
              callArgTwo.code shouldBe "<%= ENV['REDIS_HOST'] %>"

            case xs => fail(s"Expected two appends for true branch, got [${xs.code.mkString(",")}]")
          }
        case xs => fail(s"Expected one IF Structure, got [${xs.code.mkString(",")}]")
      }
    }

    "Contains ELSE-IF structure" in {
      inside(
        cpg.controlStructure
          .controlStructureType(ControlStructureTypes.IF)
          .whenFalse
          .isBlock
          .astChildren
          .isControlStructure
          .l
      ) {
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
            case appendCallStatic :: appendCallTemplate :: Nil =>
              appendCallStatic.argument.l.size shouldBe 2
              val List(callArgStaticOne, callArgStaticTwo) = appendCallStatic.argument.l: @unchecked
              callArgStaticOne.code shouldBe "joern__buffer"
              callArgStaticTwo.code shouldBe
                """rabbitmq:
                  |port:""".stripMargin

              appendCallTemplate.argument.l.size shouldBe 2
              val List(callArgOne, callArgTwo: Call) = appendCallTemplate.argument.l: @unchecked
              callArgOne.code shouldBe "joern__buffer"
              callArgTwo.methodFullName shouldBe RubyOperators.templateOutRaw
              callArgTwo.code shouldBe "<%== ENV['RABBITMQ_PORT'] %>"

            case xs => fail(s"Expected two appends for true branch, got [${xs.code.mkString(",")}]")
          }
        case xs => fail(s"Expected one ELSE-IF block, got ${xs.code.mkString("[", ",", "]")}")
      }
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

  // Test for lowered do-block
  "Do-block in <%= %> tags" should {
    /* The code gets lowered to:
     * ```
     *   joern__buffer = ""
     *   joern__buffer << form_with(url: some_url)
     *   rails_lambda_0 = lambda do |form|
     *     joern__inner_buffer << <%= form.text_field :name %>
           joern__inner_buffer
     *   end
     *   joern__buffer << rails_lambda_0.call(form)
     *   joern__buffer
     * ```
     *
     */
    val cpg = code(
      """
        |<%= form_with url: some_url do |form| %>
        | <%= form.text_field :name %>
        |<% end %>
        |""".stripMargin,
      "index.html.erb"
    )

    "Contains call to main function" in {
      inside(cpg.call.name("form_with").l) {
        case formCall :: Nil =>
          formCall.code shouldBe "form_with(url: some_url)"
        case xs => fail(s"Expected one call to `form_with`, got ${xs.code.mkString("[", ",", "]")}")
      }
    }

    "Lower to a lambda" in {
      inside(cpg.typeDecl.isLambda.l) {
        case lambda :: Nil =>
          lambda.fullName shouldBe "index.html.erb:<main>.<lambda>0"
        case xs => fail(s"Expected one lambda, got ${xs.code.mkString("[", ",", "]")}")
      }

      inside(cpg.method.isLambda.l) {
        case lambdaMethod :: Nil =>
          val List(_, lambdaParamForm) = lambdaMethod.parameter.l
          lambdaParamForm.code shouldBe "form"

          inside(lambdaMethod.body.astChildren.l) {
            case _ :: _ :: (appendCallTemplate: Call) :: _ :: Nil =>
              appendCallTemplate.code shouldBe "joern__inner_buffer << <%= form.text_field :name %>"
              val List(appendCallArgOne, appendCallArgTwo: Call) = appendCallTemplate.argument.l: @unchecked

              appendCallArgOne.code shouldBe "joern__inner_buffer"
              appendCallArgTwo.code shouldBe "<%= form.text_field :name %>"
              appendCallArgTwo.methodFullName shouldBe RubyOperators.templateOutEscape

              val List(templateOutEscapeArg) = appendCallArgTwo.argument.l
              templateOutEscapeArg.code shouldBe "form.text_field :name"
            case xs => fail(s"Expected 3 children for body, got ${xs.code.mkString("[", ",", "]")}")
          }

          inside(lambdaMethod.methodReturn.toReturn.l) {
            case lambdaRet :: Nil =>
              val List(innerBuff) = lambdaRet.astChildren.l
              innerBuff.code shouldBe "joern__inner_buffer"
            case xs => fail(s"Expected one RETURN, got ${xs.code.mkString("[", ",", "]")}")
          }
        case xs => fail(s"Expected one lambda method, got ${xs.code.mkString("[", ",", "]")}")
      }
    }

    "Assign the lambda to a variable" in {
      inside(cpg.call.name(Operators.assignment).l) {
        case _ :: _ :: lambdaAssign :: Nil =>
          val List(lhs, rhs: TypeRef) = lambdaAssign.argument.l: @unchecked
          lhs.code shouldBe "rails_lambda_0"
          rhs.code shouldBe "<lambda>0&Proc"
        case xs => fail(s"Expected one assignment, got ${xs.code.mkString("[", ",", "]")}")
      }
    }

    "Contains a call to the lowered lambda" in {
      inside(cpg.call.name("call").l) {
        case lambdaCall :: Nil =>
          lambdaCall.code shouldBe "rails_lambda_0.call(form)"
        case xs => fail(s"Expected one call to `call`, got ${xs.code.mkString("[", ",", "]")}")
      }
    }
  }

  // Test for lowered conditional call
  "Conditional calls should get lowered to IF structs" in {
    /* ERB gets lowered to:
     * ```
     *  if a == "a"
     *   joern__buffer << <%= link_to(url: some_url) %>
     *  end
     * ```
     */
    val cpg = code(
      """
        |<%= link_to(url: some_url) if a == "a" %>
        |""".stripMargin,
      "index.html.erb"
    )

    inside(cpg.controlStructure.isIf.l) {
      case ifStruct :: Nil =>
        inside(ifStruct.condition.l) {
          case (condition: Call) :: Nil =>
            condition.methodFullName shouldBe Operators.equals
            condition.code shouldBe "a == \"a\""
            val List(lhs, rhs) = condition.argument.l
            lhs.code shouldBe "self.a"
            rhs.code shouldBe "\"a\""
          case xs => fail(s"Expected one condition, got ${xs.code.mkString("[", ",", "]")}")
        }

        inside(ifStruct.whenTrue.l) {
          case trueBranch :: Nil =>
            val List(appendCall: Call) = trueBranch.astChildren.isCall.l
            appendCall.code shouldBe "joern__buffer << <%= link_to(url: some_url) %>"
            val List(appendArgOne, appendArgTwo: Call) = appendCall.argument.l: @unchecked
            appendArgTwo.methodFullName shouldBe RubyOperators.templateOutEscape
            appendArgTwo.code shouldBe "<%= link_to(url: some_url) %>"
          case xs => fail(s"Expected one true branch, got ${xs.code.mkString("[", ",", "]")}")
        }
      case xs => fail(s"Expected one ifStruct, got ${xs.code.mkString("[", ",", "]")}")
    }
  }

  "ERB containing .each block" in {
    val cpg = code(
      """
        |<% some_var.each do |var| %>
        |  <%= var.out %>
        |<% end %>
        |""".stripMargin,
      "index.html.erb"
    )

    inside(cpg.method.isLambda.l) {
      case eachLambda :: Nil =>
        val List(_, varParam) = eachLambda.parameter.l
        varParam.code shouldBe "var"

        inside(eachLambda.methodReturn.toReturn.l) {
          case ret :: Nil =>
            ret.code shouldBe "joern__buffer << <%= var.out %>"
            inside(ret.astChildren.l) {
              case (appendCall: Call) :: Nil =>
                val List(callArgOne, callArgTwo: Call) = appendCall.argument.l: @unchecked
                callArgTwo.methodFullName shouldBe RubyOperators.templateOutEscape
              case xs => fail(s"Expected one call in the return, got ${xs.code.mkString("[", ",", "]")}")
            }
          case xs => fail(s"Expected one return, got ${xs.code.mkString("[", ",", "]")}")
        }
      case xs => fail(s"Expected one lambda, got ${xs.code.mkString("[", ",", "]")}")
    }
  }
}
