package io.joern.rubysrc2cpg.passes

import io.joern.rubysrc2cpg.Config
import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class RubyTypeRecoveryTests extends RubyCode2CpgFixture {

  private val config = Config().withEnableDependencyDownload(true)

  "Type information for nodes with external dependency" should {
    // TODO:
    "be present in (Case 1)" ignore {
      val cpg = code(
        """
          |require "sendgrid-ruby"
          |
          |def func
          |   sg = SendGrid::API.new(api_key: ENV['SENDGRID_API_KEY'])
          |   response = sg.client.mail._('send').post(request_body: data)
          |end
          |""".stripMargin,
        "main.rb"
      ).moreCode(
        """
          |source 'https://rubygems.org'
          |gem 'sendgrid-ruby'
          |
          |""".stripMargin,
        "Gemfile"
      ).withConfig(config)

      cpg.identifier("sg").typeFullName.l shouldBe List("sendgrid-ruby::program:SendGrid.API")
      cpg.call("client").methodFullName.l shouldBe List("sendgrid-ruby::program:SendGrid.API.client")
      // cpg.call("post").methodFullName.l shouldBe List("sendgrid-ruby::program:SendGrid.API.client<returnValue>.mail<returnValue>.anonymous<returnValue>.post")
    }
  }

  // TODO:
  "literals declared from built-in types" ignore {
    val cpg = code(
      """
        |x = 123
        |
        |def newfunc
        | x = "foo"
        |end
        |""".stripMargin,
      "main.rb"
    )
    "resolve 'x' identifier types despite shadowing" in {
      val List(xOuterScope, xInnerScope) = cpg.identifier("x").take(2).l
      xOuterScope.dynamicTypeHintFullName shouldBe Seq("__builtin.Integer", "__builtin.String")
      xInnerScope.dynamicTypeHintFullName shouldBe Seq("__builtin.Integer", "__builtin.String")
    }
  }

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

    "resolve 'print' and 'puts' calls" in {
      val Some(printCall) = cpg.call("print").headOption: @unchecked
      printCall.methodFullName shouldBe "__builtin.print"
      val Some(maxCall) = cpg.call("puts").headOption: @unchecked
      maxCall.methodFullName shouldBe "__builtin.puts"
    }

    "conservatively present either option when an imported function uses the same name as a builtin" ignore {
      val Some(absCall) = cpg.call("sleep").headOption: @unchecked
      absCall.methodFullName shouldBe "main.rb::program.sleep"
      // NOTE: I am not sure if this is a valid expectation. If it is not then we can get rid of this one.
      absCall.dynamicTypeHintFullName shouldBe Seq("main.rb::program.sleep", "__builtin.sleep")
    }
  }

}
