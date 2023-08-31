package io.joern.rubysrc2cpg.passes.ast

import io.joern.rubysrc2cpg.passes.Defines
import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.nodes.{Identifier, MethodRef}
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, nodes}
import io.shiftleft.semanticcpg.language.*

class CallCpgTests extends RubyCode2CpgFixture {
  "simple call method" should {
    val cpg = code("""foo("a", b)""".stripMargin)

    "test call node properties" in {
      val callNode = cpg.call.name("foo").head
      callNode.code shouldBe """foo("a", b)"""
      callNode.signature shouldBe ""
      callNode.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      callNode.lineNumber shouldBe Some(1)
    }

    "test call arguments" in {
      val callNode = cpg.call.name("foo").head
      val arg1     = callNode.argument(1)
      arg1.code shouldBe "\"a\""

      val arg2 = callNode.argument(2)
      arg2.code shouldBe "b"
    }

    "test astChildren" in {
      val callNode = cpg.call.name("foo").head
      val children = callNode.astChildren
      children.size shouldBe 2

      val firstChild  = children.head
      val secondChild = children.last

      firstChild.code shouldBe "\"a\""
      secondChild.code shouldBe "b"
    }
  }

  "call on identifier with named argument" should {
    val cpg = code("""x.foo("a", b)""".stripMargin)

    "test call node properties" in {
      val callNode = cpg.call.name("foo").head
      callNode.code shouldBe """x.foo("a", b)"""
      callNode.signature shouldBe ""
      callNode.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      callNode.lineNumber shouldBe Some(1)
    }

    "test call arguments" in {
      val callNode = cpg.call.name("foo").head
      val arg1     = callNode.argument(1)
      arg1.code shouldBe "\"a\""

      val arg2 = callNode.argument(2)
      arg2.code shouldBe "b"
    }

    "test astChildren" in {
      val callNode = cpg.call.name("foo").head
      val children = callNode.astChildren
      children.size shouldBe 3

      val firstChild = children.head
      val lastChild  = children.last

      firstChild.code shouldBe "x"
      lastChild.code shouldBe "b"
    }
  }

  "call following a definition within the same module" should {
    val cpg = code("""
        |def func(a, b)
        | return a + b
        |end
        |x = func(a, b)
        |""".stripMargin)

    "test call node properties" in {
      val callNode = cpg.call.name("func").head
      callNode.name shouldBe "func"
      callNode.signature shouldBe ""
      callNode.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      callNode.lineNumber shouldBe Some(5)
    }
  }

  "call with the splat operator" should {
    val cpg = code("""
        |def print_list_of(**books_and_articles)
        |  books_and_articles.each do |book, article|
        |    puts book
        |    puts article
        |  end
        |end
        |# As an argument, we define a hash in which we will write books and articles.
        |books_and_articles_we_love = {
        |  "Ruby on Rails 4": "What is webpack?",
        |  "Ruby essentials": "What is Ruby Object Model?",
        |  "Javascript essentials": "What is Object?"
        |}
        |print_list_of(books_and_articles_we_love)
        |""".stripMargin)

    "test call node properties with children & argument" in {
      val callNode = cpg.call.name("print_list_of").head
      callNode.code shouldBe "print_list_of(books_and_articles_we_love)"
      callNode.signature shouldBe ""
      callNode.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      callNode.lineNumber shouldBe Some(14)
      callNode.astChildren.last.code shouldBe "books_and_articles_we_love"
      callNode.argument.last.code shouldBe "books_and_articles_we_love"
    }
  }

  "call with a heredoc parameter" should {
    val cpg = code("""foo(<<~SQL)
        |SELECT * FROM food
        |WHERE healthy = true
        |SQL
        |""".stripMargin)

    "take note of the here doc location and construct a literal from the following statements" in {
      val List(sql) = cpg.call.nameExact("foo").argument.isLiteral.l: @unchecked
      sql.code shouldBe
        """SELECT * FROM food
          |WHERE healthy = true
          |""".stripMargin.trim
      sql.lineNumber shouldBe Option(1)
      sql.columnNumber shouldBe Option(4)
      sql.typeFullName shouldBe Defines.String
    }
  }

  // TODO: Handle multiple heredoc parameters
  "call with multiple heredoc parameters" ignore {
    val cpg = code("""puts(<<-ONE, <<-TWO)
        |content for heredoc one
        |ONE
        |content for heredoc two
        |TWO
        |""".stripMargin)

    "take note of the here doc locations and construct the literals respectively from the following statements" in {
      cpg.method(":program").dotAst.foreach(println)
      val List(one, two) = cpg.call.nameExact("puts").argument.isLiteral.l: @unchecked
      one.code shouldBe "content for heredoc one"
      one.lineNumber shouldBe Option(1)
      one.columnNumber shouldBe Option(5)
      one.typeFullName shouldBe Defines.String
      two.code shouldBe "content for heredoc two"
      two.lineNumber shouldBe Option(1)
      two.columnNumber shouldBe Option(13)
      two.typeFullName shouldBe Defines.String
    }
  }

  "a call with a normal and a do block argument" should {
    val cpg = code("""
        |def client
        |    Faraday.new(API_HOST) do |builder|
        |      builder.request :json
        |      builder.options[:timeout] = READ_TIMEOUT
        |      builder.options[:open_timeout] = OPEN_TIMEOUT
        |    end
        |end
        |""".stripMargin)

    "have the correct arguments in the correct ordering" in {
      val List(n)                                                          = cpg.call.nameExact("new").l: @unchecked
      val List(faraday: Identifier, apiHost: Identifier, doRef: MethodRef) = n.argument.l: @unchecked
      faraday.name shouldBe "Faraday"
      faraday.argumentIndex shouldBe 0
      apiHost.name shouldBe "API_HOST"
      apiHost.argumentIndex shouldBe 1
      doRef.methodFullName shouldBe "Test0.rb::program.new3"
      doRef.argumentIndex shouldBe 2
    }
  }
}
