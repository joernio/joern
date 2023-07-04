package io.joern.rubysrc2cpg.querying

import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.nodes.Method
import io.shiftleft.semanticcpg.language._

class RequireTests extends RubyCode2CpgFixture {

  "CPG with require statement" should {
    val cpg = code("require 'json'")

    "should identify `require` call" ignore {
      val List(reqCall) = cpg.call.nameExact("require").l
      reqCall.size shouldBe 1
      reqCall.astChildren.size shouldBe 2
      reqCall.astChildren.filter(_.isIdentifier).code.head shouldBe "require"
      reqCall.astChildren.filter(_.isLiteral).code.head shouldBe "\"json\""
    }
  }

  "CPG with variants of require statements" should {

    val cpg = code("""
        |require 'json'
        |require './my_file.rb'
        |require 'net/http'
        |
        |data = { name: 'John'}
        |json_data = JSON.generate(data)
        |""".stripMargin)

    "should identify all `require` calls" ignore {
      val List(reqCall) = cpg.call.nameExact("require").l
      reqCall.size shouldBe 3
      reqCall.astChildren.filter(_.isLiteral).code.l shouldBe List("\"json\"", "\"./my_file.rb\"", "\"net/http\"")
    }
  }
}
