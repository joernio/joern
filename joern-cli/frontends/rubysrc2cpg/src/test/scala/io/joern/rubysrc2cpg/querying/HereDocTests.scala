package io.joern.rubysrc2cpg.querying

import io.joern.rubysrc2cpg.passes.Defines
import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*

class HereDocTests extends RubyCode2CpgFixture {

  "HereDoc simple" should {
    val cpg = code("""
        | def f()
        |   a = 10
        |   <<-SQL
        |     this is some sql heredoc code
        |   SQL
        |   a
        | end
        |""".stripMargin)

    "have a LITERAL node" in {
      inside(cpg.method.name("f").l) {
        case func :: Nil =>
          inside(func.block.astChildren.l) {
            case (localAst: Local) :: (callAst: Call) :: (literalAst: Literal) :: (returnAst: Return) :: Nil =>
              localAst.code shouldBe "a"
              callAst.code shouldBe "a = 10"

              literalAst.typeFullName shouldBe Defines.prefixAsCoreType("String")

              returnAst.code shouldBe "a"
            case _ =>
          }
        case _ => fail("Expected one method for f")
      }
    }
  }

  "HereDoc as a literal assignment" should {
    val cpg = code("""
        |def foo()
        | a = <<-SQL
        |   SELECT * FROM TABLE;
        | SQL
        |
        | a
        |end
        |""".stripMargin)

    "parse Heredocs" in {
      inside(cpg.method.name("foo").l) {
        case fooFunc :: Nil =>
          inside(fooFunc.block.astChildren.isCall.l) {
            case assignmentCall :: Nil =>
              inside(assignmentCall.argument.l) {
                case lhsArg :: (rhsArg: Literal) :: Nil =>
                  lhsArg.code shouldBe "a"
                  rhsArg.typeFullName shouldBe Defines.prefixAsCoreType("String")
                case _ => fail("Expected LHS and RHS for assignment")
              }
            case _ => fail("Expected call for assignment")
          }
        case _ => fail("Expected one method for foo")
      }
    }
  }

  "HereDoc as a function argument" should {
    val cpg = code("""
        |def foo(arg)
        |  bar(arg, <<-SOME_HEREDOC, arg + 1)
        |   inside here doc
        |  SOME_HEREDOC
        |end
        |""".stripMargin)

    "create a string literal in the 2nd argument position" in {
      val barCall = cpg.call("bar").head
      val hereDoc = barCall.argument(2).asInstanceOf[Literal]
      hereDoc.code shouldBe "   inside here doc\n"
    }
  }

}
