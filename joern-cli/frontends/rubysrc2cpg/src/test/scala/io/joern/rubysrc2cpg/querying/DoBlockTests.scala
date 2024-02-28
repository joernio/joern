package io.joern.rubysrc2cpg.querying

import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Method, MethodRef, TypeDecl}
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
      inside(cpg.method.nameExact(":program").l) {
        case program :: Nil =>
          inside(program.block.astChildren.collectAll[Method].l) {
            case foo :: closureMethod :: Nil =>
              foo.name shouldBe "foo"

              closureMethod.name shouldBe "<lambda>0"
              closureMethod.fullName shouldBe "Test0.rb:<global>::program:<lambda>0"
            case xs => fail(s"Expected a two method nodes, instead got [${xs.code.mkString(", ")}]")
          }

          inside(program.block.astChildren.collectAll[TypeDecl].l) {
            case closureType :: Nil =>
              closureType.name shouldBe "<lambda>0"
              closureType.fullName shouldBe "Test0.rb:<global>::program:<lambda>0"
            case xs => fail(s"Expected a one closure type node, instead got [${xs.code.mkString(", ")}]")
          }
        case xs => fail(s"Expected a single program module, instead got [${xs.code.mkString(", ")}]")
      }
    }

    "have no parameters in the closure declaration" in {
      inside(cpg.method("<lambda>0").parameter.l) {
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
          inside(program.block.astChildren.collectAll[Method].l) {
            case closureMethod :: Nil =>
              closureMethod.name shouldBe "<lambda>0"
              closureMethod.fullName shouldBe "Test0.rb:<global>::program:<lambda>0"
            case xs => fail(s"Expected a one method nodes, instead got [${xs.code.mkString(", ")}]")
          }

          inside(program.block.astChildren.collectAll[TypeDecl].l) {
            case closureType :: Nil =>
              closureType.name shouldBe "<lambda>0"
              closureType.fullName shouldBe "Test0.rb:<global>::program:<lambda>0"
            case xs => fail(s"Expected a one closure type node, instead got [${xs.code.mkString(", ")}]")
          }
        case xs => fail(s"Expected a single program module, instead got [${xs.code.mkString(", ")}]")
      }
    }

    "have the `item` parameter in the closure declaration" in {
      inside(cpg.method("<lambda>0").parameter.l) {
        case itemParam :: Nil =>
          itemParam.name shouldBe "item"
        case xs => fail(s"Expected the closure to have a single parameter, instead got [${xs.code.mkString(", ")}]")
      }
    }

    "specify the closure reference as an argument to the member call with block" in {
      inside(cpg.call("each").argument.l) {
        case (_: Call) :: (lambdaRef: MethodRef) :: Nil =>
          lambdaRef.methodFullName shouldBe "Test0.rb:<global>::program:<lambda>0"
        case xs =>
          fail(s"Expected `each` call to have call and method ref arguments, instead got [${xs.code.mkString(", ")}]")
      }
    }

    "have the call under the closure" in {
      inside(cpg.method("<lambda>0").call.l) {
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
          inside(program.block.astChildren.collectAll[Method].l) {
            case closureMethod :: Nil =>
              closureMethod.name shouldBe "<lambda>0"
              closureMethod.fullName shouldBe "Test0.rb:<global>::program:<lambda>0"
            case xs => fail(s"Expected a one method nodes, instead got [${xs.code.mkString(", ")}]")
          }

          inside(program.block.astChildren.collectAll[TypeDecl].l) {
            case closureType :: Nil =>
              closureType.name shouldBe "<lambda>0"
              closureType.fullName shouldBe "Test0.rb:<global>::program:<lambda>0"
            case xs => fail(s"Expected a one closure type node, instead got [${xs.code.mkString(", ")}]")
          }
        case xs => fail(s"Expected a single program module, instead got [${xs.code.mkString(", ")}]")
      }
    }

    "have the `key` and `value` parameter in the closure declaration" in {
      inside(cpg.method("<lambda>0").parameter.l) {
        case keyParam :: valParam :: Nil =>
          keyParam.name shouldBe "key"
          valParam.name shouldBe "value"
        case xs => fail(s"Expected the closure to have two calls, instead got [${xs.code.mkString(", ")}]")
      }
    }

    "specify the closure reference as an argument to the member call with block" in {
      inside(cpg.call("each").argument.l) {
        case (_: Call) :: (lambdaRef: MethodRef) :: Nil =>
          lambdaRef.methodFullName shouldBe "Test0.rb:<global>::program:<lambda>0"
        case xs =>
          fail(s"Expected `each` call to have call and method ref arguments, instead got [${xs.code.mkString(", ")}]")
      }
    }

    "have the calls under the closure" in {
      inside(cpg.method("<lambda>0").call.l) {
        case puts1 :: puts2 :: Nil =>
          puts1.name shouldBe "puts"
          puts1.code shouldBe "puts key"

          puts2.name shouldBe "puts"
          puts2.code shouldBe "puts value"
        case xs => fail(s"Expected the closure to have a single parameter, instead got [${xs.code.mkString(", ")}]")
      }
    }

  }

}
