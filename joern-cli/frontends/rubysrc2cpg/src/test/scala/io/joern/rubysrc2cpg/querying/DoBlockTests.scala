package io.joern.rubysrc2cpg.querying

import io.joern.rubysrc2cpg.passes.GlobalTypes.builtinPrefix
import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.joern.x2cpg.Defines
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*

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

    "create an anonymous method with associated type declaration" in {
      inside(cpg.method.isModule.l) {
        case program :: Nil =>
          inside(program.astChildren.collectAll[Method].l) {
            case foo :: closureMethod :: Nil =>
              foo.name shouldBe "foo"

              closureMethod.name shouldBe "<lambda>0"
              closureMethod.fullName shouldBe "Test0.rb:<global>::program:<lambda>0"
            case xs => fail(s"Expected a two method nodes, instead got [${xs.code.mkString(", ")}]")
          }

          inside(program.astChildren.collectAll[TypeDecl].isLambda.l) {
            case closureType :: Nil =>
              closureType.name shouldBe "<lambda>0"
              closureType.fullName shouldBe "Test0.rb:<global>::program:<lambda>0"

              val callMember = closureType.member.nameExact("call").head
              callMember.typeFullName shouldBe Defines.Any
              callMember.dynamicTypeHintFullName shouldBe Seq("Test0.rb:<global>::program:<lambda>0")
            case xs => fail(s"Expected a one closure type node, instead got [${xs.code.mkString(", ")}]")
          }
        case xs => fail(s"Expected a single program module, instead got [${xs.code.mkString(", ")}]")
      }
    }

    "create a method ref argument with populated type full name, which corresponds to the method type" in {
      val methodRefArg   = cpg.call("foo").argument(1).head.asInstanceOf[MethodRef]
      val lambdaTypeDecl = cpg.typeDecl("<lambda>0").head
      methodRefArg.typeFullName shouldBe lambdaTypeDecl.fullName
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
      inside(cpg.method.nameExact(":program").l) {
        case program :: Nil =>
          inside(program.astChildren.collectAll[Method].l) {
            case closureMethod :: Nil =>
              closureMethod.name shouldBe "<lambda>0"
              closureMethod.fullName shouldBe "Test0.rb:<global>::program:<lambda>0"
            case xs => fail(s"Expected a one method nodes, instead got [${xs.code.mkString(", ")}]")
          }

          inside(program.astChildren.collectAll[TypeDecl].l) {
            case closureType :: Nil =>
              closureType.name shouldBe "<lambda>0"
              closureType.fullName shouldBe "Test0.rb:<global>::program:<lambda>0"
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
        case (myArray: Identifier) :: (lambdaRef: MethodRef) :: Nil =>
          myArray.argumentIndex shouldBe 0
          myArray.name shouldBe "my_array"
          myArray.code shouldBe "my_array"

          lambdaRef.argumentIndex shouldBe 1
          lambdaRef.methodFullName shouldBe "Test0.rb:<global>::program:<lambda>0"
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
      inside(cpg.method.nameExact(":program").l) {
        case program :: Nil =>
          inside(program.astChildren.collectAll[Method].l) {
            case closureMethod :: Nil =>
              closureMethod.name shouldBe "<lambda>0"
              closureMethod.fullName shouldBe "Test0.rb:<global>::program:<lambda>0"
              closureMethod.isLambda.nonEmpty shouldBe true
            case xs => fail(s"Expected a one method nodes, instead got [${xs.code.mkString(", ")}]")
          }

          inside(program.astChildren.collectAll[TypeDecl].l) {
            case closureType :: Nil =>
              closureType.name shouldBe "<lambda>0"
              closureType.fullName shouldBe "Test0.rb:<global>::program:<lambda>0"
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
        case (hash: Identifier) :: (lambdaRef: MethodRef) :: Nil =>
          hash.argumentIndex shouldBe 0
          hash.name shouldBe "hash"
          hash.code shouldBe "hash"

          lambdaRef.argumentIndex shouldBe 1
          lambdaRef.methodFullName shouldBe "Test0.rb:<global>::program:<lambda>0"
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
    "create the declarations for the closure" in {
      inside(cpg.method("<lambda>.*").l) {
        case m :: Nil =>
          m.name should startWith("<lambda>")
        case xs => fail(s"Expected exactly one closure method decl, instead got [${xs.code.mkString(",")}]")
      }

      inside(cpg.typeDecl("<lambda>.*").l) {
        case m :: Nil =>
          m.name should startWith("<lambda>")
        case xs => fail(s"Expected exactly one closure type decl, instead got [${xs.code.mkString(",")}]")
      }
    }

    "annotate the nodes via CAPTURE bindings" in {
      cpg.all.collectAll[ClosureBinding].l match {
        case myValue :: Nil =>
          myValue.closureOriginalName.head shouldBe "myValue"
          inside(myValue._localViaRefOut) {
            case Some(local) =>
              local.name shouldBe "myValue"
              local.method.fullName.headOption shouldBe Option("Test0.rb:<global>::program")
            case None => fail("Expected closure binding refer to the captured local")
          }

          inside(myValue._captureIn.l) {
            case (x: MethodRef) :: Nil => x.methodFullName shouldBe "Test0.rb:<global>::program:<lambda>0"
            case xs                    => fail(s"Expected single method ref binding but got [${xs.mkString(",")}]")
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
              tmpAssign.code shouldBe "<tmp-0> = Array.new(x) { |i| i += 1 }"

              newCall.name shouldBe "new"
              newCall.methodFullName shouldBe s"$builtinPrefix.Array:initialize"

              inside(newCall.argument.l) {
                case (_: Identifier) :: (x: Identifier) :: (closure: MethodRef) :: Nil =>
                  x.name shouldBe "x"
                  closure.methodFullName should endWith("<lambda>0")
                case xs => fail(s"Expected a base, `x`, and closure ref, instead got [${xs.code.mkString(",")}]")
              }
            case xs =>
              fail(
                s"Expected four nodes under the lowering block of a constructor, instead got [${xs.code.mkString(",")}]"
              )
          }
        case xs => fail(s"Unexpected `foo` assignment children [${xs.code.mkString(",")}]")
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
        case (_: Identifier) :: (testName: Literal) :: (testMethod: MethodRef) :: Nil =>
          testName.code shouldBe "'Foo'"
          testMethod.referencedMethod.call.nameExact("puts").nonEmpty shouldBe true
        case xs => fail(s"Expected a literal and method ref argument, instead got $xs")
      }
    }

  }

}
