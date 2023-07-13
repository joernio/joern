package io.joern.rubysrc2cpg.passes.ast

import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.{EvaluationStrategies, NodeTypes, DispatchTypes, Operators, nodes}
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal

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
      callNode.astChildren.head.code shouldBe "this"
      callNode.astChildren.last.code shouldBe "books_and_articles_we_love"
      callNode.argument.head.code shouldBe "this"
      callNode.argument.last.code shouldBe "books_and_articles_we_love"
    }
  }
}
