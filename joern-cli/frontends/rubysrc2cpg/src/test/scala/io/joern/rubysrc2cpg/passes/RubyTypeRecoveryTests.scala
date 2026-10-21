package io.joern.rubysrc2cpg.passes

import io.joern.rubysrc2cpg.passes.Defines.Main
import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.nodes.Identifier
import io.shiftleft.semanticcpg.language.*
import scala.collection.immutable.List

class RubyInternalTypeRecoveryTests extends RubyCode2CpgFixture(withPostProcessing = true) {
  "recovering paths for built-in calls" should {
    lazy val cpg = code(
      """
        |print("Hello world")
        |puts "Hello"
        |
        |def sleep(input)
        |end
        |
        |sleep(2)
        |""".stripMargin,
      "main.rb"
    ).cpg

    "resolve 'print' and 'puts' StubbedRubyType calls" in {
      val List(printCall) = cpg.call("print").l
      printCall.methodFullName shouldBe Defines.prefixAsKernelDefined("print")
      val List(maxCall) = cpg.call("puts").l
      maxCall.methodFullName shouldBe Defines.prefixAsKernelDefined("puts")
    }

    "present the declared method name when a built-in with the same name is used in the same compilation unit" in {
      val List(absCall) = cpg.call("sleep").l
      absCall.methodFullName shouldBe s"main.rb:$Main.sleep"
    }
  }

  "Type information for literals" should {
    val cpg = code("""
                     |def func
                     | a = 2
                     | b = "abc"
                     | b
                     |end
                     |
                     |def func2
                     | func
                     |end
                     |
                     |c = func2()
                     |""".stripMargin)

    "propagate function return types" in {
      inside(cpg.method.name("func2?").l) { case func :: func2 :: Nil =>
        func.methodReturn.typeFullName shouldBe Defines.prefixAsCoreType("String")
        func2.methodReturn.typeFullName shouldBe Defines.prefixAsCoreType("String")
      }
    }

    "propagate return type to identifier c" in {
      inside(cpg.identifier.name("c").l) { case cIdent :: Nil =>
        cIdent.typeFullName shouldBe Defines.prefixAsCoreType("String")
      }
    }
  }

  "Type information for imported function" should {
    val cpg = code(
      """
        |class Test2A
        |end
        |
        |module Test2B
        |end
        |
        |def func
        |  "abc"
        |end
        |
        |""".stripMargin,
      "test2.rb"
    )
      .moreCode(
        """
          |require 'test2'
          |a = func
          |
          |b = Test2A.new
          |""".stripMargin,
        "test1.rb"
      )

    // TODO: Revisit
    "propagate to assigned variable" ignore {
      inside(cpg.file("test1.rb").method.name(":program").call.nameExact("<operator>.assignment").l) {
        case funcAssignment :: constructAssignment :: tmpAssignment :: Nil =>
          inside(funcAssignment.argument.l) { case (lhs: Identifier) :: rhs :: Nil =>
            lhs.typeFullName shouldBe Defines.prefixAsCoreType("String")
          }

          inside(constructAssignment.argument.l) { case (lhs: Identifier) :: rhs :: Nil =>
            lhs.typeFullName shouldBe s"test2.rb:$Main.Test2A"
          }
      }
    }
  }

  "Type information for constructors" should {
    val cpg = code("""
                     |class A
                     |end
                     |
                     |def func
                     | d = A.new
                     | d
                     |end
                     |
                     |a = A.new
                     |b = func
                     |""".stripMargin)

    // TODO: Revisit
    "propagate to identifier" ignore {
      inside(cpg.identifier.name("(a|b)").l) { case aIdent :: bIdent :: Nil =>
        aIdent.typeFullName shouldBe s"Test0.rb:$Main.A"
        bIdent.typeFullName shouldBe s"Test0.rb:$Main.A"
      }
    }
  }

}
