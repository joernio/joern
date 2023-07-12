package io.joern.rubysrc2cpg.passes

import io.joern.rubysrc2cpg.Config
import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class RubyTypeRecoveryTests extends RubyCode2CpgFixture {

  private val config = Config().withEnableDependencyDownload(true)

  "Type information for nodes with external dependency" should {
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

}
