package io.joern.rubysrc2cpg.deprecated.passes.ast

import io.joern.rubysrc2cpg.deprecated.passes.Defines
import io.joern.rubysrc2cpg.testfixtures.{RubyCode2CpgFixture, SameInNewFrontend}
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Identifier, MethodRef}
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, nodes}
import io.shiftleft.semanticcpg.language.*

class CallCpgTests extends RubyCode2CpgFixture(withPostProcessing = true, useDeprecatedFrontend = true) {
  "simple call method" should {
    val cpg = code("""foo("a", b)""".stripMargin)

    "test call node properties" taggedAs SameInNewFrontend in {
      val callNode = cpg.call.name("foo").head
      callNode.code shouldBe """foo("a", b)"""
      callNode.signature shouldBe ""
      callNode.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      callNode.lineNumber shouldBe Some(1)
    }

    "test call arguments" taggedAs SameInNewFrontend in {
      val callNode = cpg.call.name("foo").head
      val arg1     = callNode.argument(1)
      arg1.code shouldBe "\"a\""

      val arg2 = callNode.argument(2)
      arg2.code shouldBe "b"
    }

    "test astChildren" taggedAs SameInNewFrontend in {
      val callNode = cpg.call.name("foo").head
      val children = callNode.astChildren.l
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
      val children = callNode.astChildren.l
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

  "a call without parenthesis before the method definition is seen/resolved" should {
    val cpg = code(
      """
        |require "foo.rb"
        |
        |def event_params
        |    @event_params ||= device_params
        |      .merge(params)
        |      .merge(encoded_partner_params)
        |      .merge(
        |        s2s: 1,
        |        created_at_unix: Time.current.to_i,
        |        app_token: app_token,
        |        event_token: event_token,
        |        install_source: install_source
        |      )
        |end
        |""".stripMargin,
      "bar.rb"
    )
      .moreCode(
        """
        |def device_params
        |    case platform
        |    when :android
        |      { adid: adid, gps_adid: gps_adid }
        |    when :ios
        |      { adid: adid, idfa: idfa }
        |    else
        |      {}
        |    end
        |end
        |""".stripMargin,
        "foo.rb"
      )

    "have its call node correctly identified and created" in {
      val List(deviceParams) = cpg.call.nameExact("device_params").l: @unchecked
      deviceParams.name shouldBe "device_params"
      deviceParams.code shouldBe "device_params"
      deviceParams.methodFullName shouldBe "foo.rb::program.device_params"
      deviceParams.typeFullName shouldBe Defines.Any
      deviceParams.lineNumber shouldBe Option(5)
      deviceParams.columnNumber shouldBe Option(22)
      deviceParams.argumentIndex shouldBe 0
    }
  }

  "a parenthesis-less call (defined later in the module) in a call's argument" should {
    val cpg = code("""
        |module Pay
        |  module Webhooks
        |    class BraintreeController < Pay::ApplicationController
        |      if Rails.application.config.action_controller.default_protect_from_forgery
        |        skip_before_action :verify_authenticity_token
        |      end
        |
        |      def create
        |        queue_event(verified_event) # <------ verified event is a call here
        |        head :ok
        |      rescue ::Braintree::InvalidSignature
        |        head :bad_request
        |      end
        |
        |      private
        |
        |      def queue_event(event)
        |        return unless Pay::Webhooks.delegator.listening?("braintree.#{event.kind}")
        |
        |        record = Pay::Webhook.create!(
        |          processor: :braintree,
        |          event_type: event.kind,
        |          event: {bt_signature: params[:bt_signature], bt_payload: params[:bt_payload]}
        |        )
        |        Pay::Webhooks::ProcessJob.perform_later(record)
        |      end
        |
        |      def verified_event
        |        Pay.braintree_gateway.webhook_notification.parse(params[:bt_signature], params[:bt_payload])
        |      end
        |    end
        |  end
        |end
        |""".stripMargin)

    "be a call node instead of an identifier" in {
      inside(cpg.call("queue_event").argument.l) {
        case (verifiedEvent: Call) :: Nil =>
          verifiedEvent.name shouldBe "verified_event"
        case xs =>
          fail(s"Expected a single call argument, received [${xs.map(x => x.label -> x.code).mkString(", ")}] instead!")
      }
    }
  }
}
