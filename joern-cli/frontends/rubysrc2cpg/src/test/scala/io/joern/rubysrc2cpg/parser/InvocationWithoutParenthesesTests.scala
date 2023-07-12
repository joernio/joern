package io.joern.rubysrc2cpg.parser

class InvocationWithoutParenthesesTests extends RubyParserAbstractTest {

  "A method invocation without parentheses" should {

    "be parsed as a primary expression" when {

      "it contains a keyword?-named member" in {
        val code = "task.nil?"

        printAst(_.primary(), code) shouldBe
          """ChainedInvocationPrimary
            | VariableReferencePrimary
            |  VariableIdentifierVariableReference
            |   VariableIdentifier
            |    task
            | .
            | MethodName
            |  MethodIdentifier
            |   MethodOnlyIdentifier
            |    Keyword
            |     nil
            |    ?""".stripMargin
      }

      "it is keyword?-named" in {
        val code = "do?"

        printAst(_.primary(), code) shouldBe
          """MethodOnlyIdentifierPrimary
            | MethodOnlyIdentifier
            |  Keyword
            |   do
            |  ?""".stripMargin
      }

      "it is keyword!-named" in {
        val code = "return!"

        printAst(_.primary(), code) shouldBe
          """MethodOnlyIdentifierPrimary
            | MethodOnlyIdentifier
            |  Keyword
            |   return
            |  !""".stripMargin
      }
    }
  }

}
