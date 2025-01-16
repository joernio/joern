package io.joern.rubysrc2cpg.querying

import io.joern.rubysrc2cpg.passes.Defines.{Initialize, Main, RubyOperators}
import io.joern.rubysrc2cpg.passes.GlobalTypes.builtinPrefix
import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.joern.x2cpg.Defines
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*
import io.joern.rubysrc2cpg.passes.Defines as RubyDefines

class DoBlockTests extends RubyCode2CpgFixture {

  "a basic unparameterized do block off a simple call" should {

    val cpg = code("""
        |def foo &block
        | puts block.call
        |end
        |
        |foo do
        | "world!"
        |end
        |
        |""".stripMargin)

    "create an anonymous method with associated type declaration and wrapper type" in {
      inside(cpg.method.isModule.l) {
        case program :: Nil =>
          inside(program.astChildren.collectAll[Method].l) {
            case foo :: closureMethod :: Nil =>
              foo.name shouldBe "foo"

              closureMethod.name shouldBe "<lambda>0"
              closureMethod.fullName shouldBe s"Test0.rb:$Main.<lambda>0"
            case xs => fail(s"Expected a two method nodes, instead got [${xs.code.mkString(", ")}]")
          }

          inside(program.astChildren.collectAll[TypeDecl].isLambda.l) {
            case closureType :: Nil =>
              closureType.name shouldBe "<lambda>0"
              closureType.fullName shouldBe s"Test0.rb:$Main.<lambda>0"
            case xs => fail(s"Expected a one closure type node, instead got [${xs.code.mkString(", ")}]")
          }

          inside(program.astChildren.collectAll[TypeDecl].name(".*Proc").l) {
            case closureType :: Nil =>
              val callMember = closureType.member.nameExact("call").head
              callMember.typeFullName shouldBe Defines.Any
              callMember.dynamicTypeHintFullName shouldBe Seq(s"Test0.rb:$Main.<lambda>0")
            case xs => fail(s"Expected a one closure type node, instead got [${xs.code.mkString(", ")}]")
          }
        case xs => fail(s"Expected a single program module, instead got [${xs.code.mkString(", ")}]")
      }
    }

    "create a method ref argument with populated type full name, which corresponds to the method type" in {
      val typeRefArg     = cpg.call("foo").argument(1).head.asInstanceOf[TypeRef]
      val lambdaTypeDecl = cpg.typeDecl("<lambda>0").head
      typeRefArg.typeFullName shouldBe s"${lambdaTypeDecl.fullName}&Proc"
    }

    "have no parameters in the closure declaration" in {
      inside(cpg.method("<lambda>0").parameter.indexGt(0).l) {
        case Nil => // pass
        case xs  => fail(s"Expected the closure to have no parameters, instead got [${xs.code.mkString(", ")}]")
      }
    }

    "have the return node under the closure (returning the literal)" in {
      inside(cpg.method("<lambda>0").block.astChildren.l) {
        case ret :: Nil =>
          ret.code shouldBe "\"world!\""
        case xs => fail(s"Expected the closure to have a single call, instead got [${xs.code.mkString(", ")}]")
      }
    }

  }

  "a basic parameterized do-block with braces" should {
    val cpg = code("""
        |my_array = [1, 2, 3]
        |my_array.each { |item|
        |    puts item
        |}
        |""".stripMargin)

    "create an anonymous method with associated type declaration" in {
      inside(cpg.method.isModule.l) {
        case program :: Nil =>
          inside(program.astChildren.collectAll[Method].l) {
            case closureMethod :: Nil =>
              closureMethod.name shouldBe "<lambda>0"
              closureMethod.fullName shouldBe s"Test0.rb:$Main.<lambda>0"
            case xs => fail(s"Expected a one method nodes, instead got [${xs.code.mkString(", ")}]")
          }

          inside(program.astChildren.collectAll[TypeDecl].isLambda.l) {
            case closureType :: Nil =>
              closureType.name shouldBe "<lambda>0"
              closureType.fullName shouldBe s"Test0.rb:$Main.<lambda>0"
            case xs => fail(s"Expected a one closure type node, instead got [${xs.code.mkString(", ")}]")
          }
        case xs => fail(s"Expected a single program module, instead got [${xs.code.mkString(", ")}]")
      }
    }

    "have the `item` parameter in the closure declaration" in {
      inside(cpg.method("<lambda>0").parameter.indexGt(0).l) {
        case itemParam :: Nil =>
          itemParam.name shouldBe "item"
        case xs => fail(s"Expected the closure to have a single parameter, instead got [${xs.code.mkString(", ")}]")
      }
    }

    "specify the closure reference as an argument to the member call with block" in {
      inside(cpg.call("each").argument.l) {
        case (myArray: Identifier) :: (lambdaRef: TypeRef) :: Nil =>
          myArray.argumentIndex shouldBe 0
          myArray.name shouldBe "my_array"
          myArray.code shouldBe "my_array"

          lambdaRef.argumentIndex shouldBe 1
          lambdaRef.typeFullName shouldBe s"Test0.rb:$Main.<lambda>0&Proc"
        case xs =>
          fail(s"Expected `each` call to have call and method ref arguments, instead got [${xs.code.mkString(", ")}]")
      }
    }

    "have the call under the closure" in {
      inside(cpg.method("<lambda>0").call.nameExact("puts").l) {
        case puts :: Nil =>
          puts.name shouldBe "puts"
          puts.code shouldBe "puts item"
        case xs => fail(s"Expected the closure to have a single call, instead got [${xs.code.mkString(", ")}]")
      }
    }
  }

  "a do block iterating over a hash" should {

    val cpg = code("""
        |hash = { "a" => 1, "b" => 2 }
        |hash.each do |key, value|
        |  puts key
        |  puts value
        |end
        |""".stripMargin)

    "create an anonymous method with associated type declaration" in {
      inside(cpg.method.isModule.l) {
        case program :: Nil =>
          inside(program.astChildren.collectAll[Method].l) {
            case closureMethod :: Nil =>
              closureMethod.name shouldBe "<lambda>0"
              closureMethod.fullName shouldBe s"Test0.rb:$Main.<lambda>0"
              closureMethod.isLambda.nonEmpty shouldBe true
            case xs => fail(s"Expected a one method nodes, instead got [${xs.code.mkString(", ")}]")
          }

          inside(program.astChildren.collectAll[TypeDecl].isLambda.l) {
            case closureType :: Nil =>
              closureType.name shouldBe "<lambda>0"
              closureType.fullName shouldBe s"Test0.rb:$Main.<lambda>0"
              closureType.isLambda.nonEmpty shouldBe true
            case xs => fail(s"Expected a one closure type node, instead got [${xs.code.mkString(", ")}]")
          }
        case xs => fail(s"Expected a single program module, instead got [${xs.code.mkString(", ")}]")
      }
    }

    "have the `key` and `value` parameter in the closure declaration" in {
      inside(cpg.method("<lambda>0").parameter.indexGt(0).l) {
        case keyParam :: valParam :: Nil =>
          keyParam.name shouldBe "key"
          valParam.name shouldBe "value"
        case xs => fail(s"Expected the closure to have two calls, instead got [${xs.code.mkString(", ")}]")
      }
    }

    "specify the closure reference as an argument to the member call with block" in {
      inside(cpg.call("each").argument.l) {
        case (hash: Identifier) :: (lambdaRef: TypeRef) :: Nil =>
          hash.argumentIndex shouldBe 0
          hash.name shouldBe "hash"
          hash.code shouldBe "hash"

          lambdaRef.argumentIndex shouldBe 1
          lambdaRef.typeFullName shouldBe s"Test0.rb:$Main.<lambda>0&Proc"
        case xs =>
          fail(s"Expected `each` call to have call and method ref arguments, instead got [${xs.code.mkString(", ")}]")
      }
    }

    "have the calls under the closure" in {
      inside(cpg.method("<lambda>0").call.nameExact("puts").l) {
        case puts1 :: puts2 :: Nil =>
          puts1.name shouldBe "puts"
          puts1.code shouldBe "puts key"

          puts2.name shouldBe "puts"
          puts2.code shouldBe "puts value"
        case xs => fail(s"Expected the closure to have a single parameter, instead got [${xs.code.mkString(", ")}]")
      }
    }

  }

  "a do block referencing variables from the surrounding scope" should {

    val cpg = code("""myValue = "Jack"
        |
        |x = proc { "Hello #{myValue}" }
        |""".stripMargin)

    // Basic assertions for expected behaviour
    "create the declarations for the closure with captured local" in {
      inside(cpg.method.isLambda.l) {
        case m :: Nil =>
          m.name should startWith("<lambda>")
          val myValue = m.local.nameExact("myValue").head
          myValue.closureBindingId shouldBe Option(s"Test0.rb:$Main.myValue")
        case xs => fail(s"Expected exactly one closure method decl, instead got [${xs.code.mkString(",")}]")
      }

      inside(cpg.typeDecl.isLambda.l) {
        case m :: Nil =>
          m.name should startWith("<lambda>")
        case xs => fail(s"Expected exactly one closure type decl, instead got [${xs.code.mkString(",")}]")
      }
    }

    "annotate the nodes via CAPTURE bindings" in {
      cpg.all.collectAll[ClosureBinding].l match {
        case myValue :: Nil =>
          myValue.closureOriginalName shouldBe Option("myValue")
          inside(myValue._localViaRefOut) {
            case Some(local) =>
              local.name shouldBe "myValue"
              local.method.fullName.headOption shouldBe Option(s"Test0.rb:$Main")
            case None => fail("Expected closure binding refer to the captured local")
          }

          inside(myValue._captureIn.l) {
            case (x: TypeRef) :: Nil => x.typeFullName shouldBe s"Test0.rb:$Main.<lambda>0&Proc"
            case xs                  => fail(s"Expected single method ref binding but got [${xs.mkString(",")}]")
          }

        case xs =>
          fail(s"Expected single closure binding but got [${xs.mkString(",")}]")
      }
    }

  }

  "a block constructor" should {

    val cpg = code("""
        |def bar(x)
        | foo = Array.new(x) { |i| i += 1 }
        |end
        |""".stripMargin)

    "create the usual lowered assignment block, except with a method ref argument for the closure" in {
      inside(cpg.assignment.code("foo = Array.new.*").argument.l) {
        case (foo: Identifier) :: (constrBlock: Block) :: Nil =>
          foo.name shouldBe "foo"

          inside(constrBlock.astChildren.l) {
            case (tmpLocal: Local) :: (tmpAssign: Call) :: (newCall: Call) :: (_: Identifier) :: Nil =>
              tmpLocal.name shouldBe "<tmp-0>"
              tmpAssign.code shouldBe s"<tmp-0> = Array.$Initialize"

              newCall.name shouldBe Initialize
              newCall.methodFullName shouldBe Defines.DynamicCallUnknownFullName
              newCall.dynamicTypeHintFullName should contain(s"$builtinPrefix.Array.$Initialize")

              inside(newCall.argument.l) {
                case (_: Identifier) :: (x: Identifier) :: (closure: TypeRef) :: Nil =>
                  x.name shouldBe "x"
                  closure.typeFullName should endWith("<lambda>0&Proc")
                case xs => fail(s"Expected a base, `x`, and closure ref, instead got [${xs.code.mkString(",")}]")
              }
            case xs =>
              fail(
                s"Expected four nodes under the lowering block of a constructor, instead got [${xs.code.mkString(",")}]"
              )
          }
        case xs =>
          fail(s"Unexpected `foo` assignment children [${xs.code.mkString(",")}]")
      }
    }
  }

  "Hash parameter for Do-block" should {
    val cpg = code("""
        |  define_method(name) do |**args| # <--- Parser error on this line
        |          setting_type.fetch(sendgrid_client: sendgrid_client, name: name, query_params: args)
        |        end
        |""".stripMargin)

    "Create correct parameter for args" in {
      inside(cpg.parameter.name("args").l) {
        case argParam :: Nil =>
          argParam.name shouldBe "args"
          argParam.code shouldBe "**args"
        case _ => fail("Expected parameter `**args` to exist")
      }
    }
  }

  "A command with do block and argument" should {

    val cpg = code("""
        |test_name 'Foo' do
        | puts "a"
        |end
        |""".stripMargin)

    "create a call `test_name` with a test name and lambda argument" in {
      inside(cpg.call.nameExact("test_name").argument.l) {
        case (_: Identifier) :: (testName: Literal) :: (testMethod: TypeRef) :: Nil =>
          testName.code shouldBe "'Foo'"
          cpg.method
            .fullNameExact(testMethod.typ.referencedTypeDecl.member.name("call").dynamicTypeHintFullName.toSeq*)
            .call
            .nameExact("puts")
            .nonEmpty shouldBe true
        case xs => fail(s"Expected a literal and method ref argument, instead got $xs")
      }
    }

  }

  "A lambda with arrow syntax" should {

    val cpg = code("""
        |arrow_lambda = ->(y) { y }
        |""".stripMargin)

    "create a lambda method with a `y` parameter" in {
      inside(cpg.method.isLambda.headOption) {
        case Some(lambda) =>
          lambda.code shouldBe "->(y) { y }"
          lambda.parameter.name.l shouldBe List("self", "y")
        case xs => fail(s"Expected a lambda method")
      }
    }

    "create a method ref assigned to `arrow_lambda`" in {
      inside(cpg.method.isModule.assignment.code("arrow_lambda.*").headOption) {
        case Some(lambdaAssign) =>
          lambdaAssign.target.asInstanceOf[Identifier].name shouldBe "arrow_lambda"
          lambdaAssign.source.asInstanceOf[TypeRef].typeFullName shouldBe s"Test0.rb:$Main.<lambda>0&Proc"
        case xs => fail(s"Expected an assignment to a lambda")
      }
    }

  }

  "A lambda with lambda keyword syntax" should {

    val cpg = code("""
        |a_lambda = lambda { |y| y }
        |""".stripMargin)

    "create a lambda method with a `y` parameter" in {
      inside(cpg.method.isLambda.headOption) {
        case Some(lambda) =>
          lambda.code shouldBe "lambda { |y| y }"
          lambda.parameter.name.l shouldBe List("self", "y")
        case xs => fail(s"Expected a lambda method")
      }
    }

    "create a method ref assigned to `arrow_lambda`" in {
      inside(cpg.method.isModule.assignment.code("a_lambda.*").headOption) {
        case Some(lambdaAssign) =>
          lambdaAssign.target.asInstanceOf[Identifier].name shouldBe "a_lambda"
          lambdaAssign.source.asInstanceOf[TypeRef].typeFullName shouldBe s"Test0.rb:$Main.<lambda>0&Proc"
        case xs => fail(s"Expected an assignment to a lambda")
      }
    }

  }

  "One local node for variable in lambda only" in {
    val cpg = code("""
                     | def get_pto_schedule
                     |    begin
                     |       jfs = []
                     |       schedules = []
                     |       schedules.each do |s|
                     |          hash = Hash.new
                     |          hash[:id] = s[:id]
                     |          hash[:title] = s[:event_name]
                     |          hash[:start] = s[:date_begin]
                     |          hash[:end] = s[:date_end]
                     |          jfs << hash
                     |       end
                     |    rescue
                     |    end
                     |  end
                     |""".stripMargin)

    inside(cpg.local.nameNot("<tmp-\\d>").l) {
      case jfsOutsideLocal :: schedules :: hashInsideLocal :: jfsCapturedLocal :: Nil =>
        jfsOutsideLocal.closureBindingId shouldBe None
        hashInsideLocal.closureBindingId shouldBe None
        jfsCapturedLocal.closureBindingId shouldBe Some("Test0.rb:<main>.get_pto_schedule.jfs")
      case xs => fail(s"Expected 6 locals, got ${xs.code.mkString(",")}")
    }

    inside(cpg.method.isLambda.local.l) {
      case hashLocal :: _ :: jfsLocal :: Nil =>
        hashLocal.closureBindingId shouldBe None
        jfsLocal.closureBindingId shouldBe Some("Test0.rb:<main>.get_pto_schedule.jfs")
      case xs => fail(s"Expected 3 locals in lambda, got ${xs.code.mkString(",")}")
    }
  }

  "Various do-block parameters" should {
    val cpg = code("""
        |f { |a, (b, c), *d, e, (f, *g), **h, &i|
        | puts a
        |}
        |""".stripMargin)

    "Generate correct parameters" in {
      inside(cpg.method.isLambda.parameter.l) {
        case _ :: aParam :: tmp0Param :: dParam :: eParam :: tmp1Param :: hParam :: iParam :: Nil =>
          aParam.name shouldBe "a"
          aParam.code shouldBe "a"

          tmp0Param.name shouldBe "<tmp-0>"
          tmp0Param.code shouldBe "<tmp-0>"

          dParam.name shouldBe "d"
          dParam.code shouldBe "*d"

          eParam.name shouldBe "e"
          eParam.code shouldBe "e"

          tmp1Param.name shouldBe "<tmp-1>"
          tmp1Param.code shouldBe "<tmp-1>"

          hParam.name shouldBe "h"
          hParam.code shouldBe "**h"

          iParam.name shouldBe "i"
          iParam.code shouldBe "&i"
        case xs => fail(s"Expected 8 parameters, got [${xs.name.mkString(", ")}]")
      }
    }

    "Generate required locals" in {
      inside(cpg.method.isLambda.body.local.l) {
        case bLocal :: cLocal :: fLocal :: gSplatLocal :: Nil =>
          bLocal.code shouldBe "b"
          cLocal.code shouldBe "c"

          fLocal.code shouldBe "f"
          gSplatLocal.code shouldBe "g"
        case xs => fail(s"Expected 4 locals, got [${xs.name.mkString(", ")}]")
      }
    }

    "Generate required `assignment` calls" in {
      inside(cpg.method.isLambda.call(Operators.assignment).l) {
        case bAssign :: cAssign :: fAssign :: gAssign :: Nil =>
          bAssign.code shouldBe "b = *<tmp-0>"
          cAssign.code shouldBe "c = *<tmp-0>"

          fAssign.code shouldBe "f = *<tmp-1>"
          gAssign.code shouldBe "*g = *<tmp-1>"
        case xs => fail(s"Expected 4 assignments, got [${xs.code.mkString(", ")}]")
      }
    }

    "Return nil and not the desugaring" in {
      val nilLiteral = cpg.method.isLambda.methodReturn.toReturn.astChildren.isLiteral.head
      nilLiteral.code shouldBe "return nil"
    }
  }

  "Nested grouped parameter in block" in {
    val cpg = code("""
        |def format_result(result)
        |  result.each_with_object({}) do |((label_id, date), count), hash|
        |    label = labels_by_id.fetch(label_id)
        |  end.values
        |end
        |""".stripMargin)

    inside(cpg.method.isLambda.body.astChildren.isCall.name(Operators.assignment).l) {
      case _ :: groupedParam :: countAssignment :: Nil =>
        inside(groupedParam.argument.l) {
          case (labelIdAssign: Call) :: (dateAssign: Call) :: (tmp0Splat: Call) :: Nil =>
            inside(labelIdAssign.argument.l) {
              case (lhs: Identifier) :: (rhs: Call) :: Nil =>
                lhs.code shouldBe "label_id"

                rhs.code shouldBe "*<tmp-1>"
                rhs.methodFullName shouldBe RubyOperators.splat
              case xs =>
                fail(s"Expected lhs and rhs for assignment, got ${xs.code.mkString(",")}")
            }

            inside(dateAssign.argument.l) {
              case (lhs: Identifier) :: (rhs: Call) :: Nil =>
                lhs.code shouldBe "date"

                rhs.code shouldBe "*<tmp-1>"
                rhs.methodFullName shouldBe RubyOperators.splat
              case xs =>
                fail(s"Expected lhs and rhs for assignment, got ${xs.code.mkString(",")}")
            }
          case xs => fail(s"Expected four arguments for call, got [${xs.code.mkString(",")}]")
        }

        inside(countAssignment.argument.l) {
          case (lhs: Identifier) :: (rhs: Call) :: Nil =>
            lhs.code shouldBe "count"
            rhs.code shouldBe "*<tmp-0>"
            rhs.methodFullName shouldBe RubyOperators.splat
          case xs => fail(s"Expected LHS and RHS for assignment, got ${xs.code.mkString(",")}")
        }
      case xs => fail(s"Expected three assignment calls, got [${xs.code.mkString(",")}]")
    }
  }

  "a back reference in a do block should be a field access from `self`" in {
    val cpg = code("""
        |def bar()
        |  foo("something") { urls << $& }
        |end
        |""".stripMargin)
    val backRefCall = cpg.method.isLambda.ast.fieldAccess
      .and(_.fieldIdentifier.canonicalNameExact("$&"), _.argument(1).isIdentifier.nameExact(RubyDefines.Self))
      .head
    backRefCall.name shouldBe Operators.fieldAccess
    backRefCall.code shouldBe "self.$&"
    backRefCall.lineNumber shouldBe Option(3)
    backRefCall.columnNumber shouldBe Option(29)
  }
}
