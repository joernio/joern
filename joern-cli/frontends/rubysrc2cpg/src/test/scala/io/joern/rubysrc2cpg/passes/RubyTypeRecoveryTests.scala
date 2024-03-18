package io.joern.rubysrc2cpg.passes

import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.joern.rubysrc2cpg.Config
import io.shiftleft.semanticcpg.language.*

class RubyTypeRecoveryTests extends RubyCode2CpgFixture(withPostProcessing = true, downloadDependencies = true) {

  "Type information for nodes with external dependency" should {

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
        |gem "sendgrid-ruby", '6.7.0'
        |""".stripMargin,
      "Gemfile"
    )

    "be present in (Case 1)" in {
      cpg.identifier("sg").lineNumber(5).typeFullName.l shouldBe List("sendgrid-ruby::program.SendGrid.API")
      cpg.call("client").methodFullName.l shouldBe List("sendgrid-ruby::program.SendGrid.API.client")
    }

    "be present in (Case 2)" ignore {
      cpg.call("post").methodFullName.l shouldBe List(
        "sendgrid-ruby::program.SendGrid.API.client<returnValue>.mail<returnValue>.anonymous<returnValue>.post"
      )
    }
  }

}
