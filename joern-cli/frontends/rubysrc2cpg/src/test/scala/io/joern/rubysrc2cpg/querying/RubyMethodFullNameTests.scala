package io.joern.rubysrc2cpg.querying

import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.semanticcpg.language._

class RubyMethodFullNameTests extends RubyCode2CpgFixture {

  "Code for method full name when method present in same file for simple class" should {
    val cpg = code("""
        |class Hello
        | def printValue(value)
        |   puts value
        | end
        |end
        |
        |v = Hello.new
        |puts v.printValue("any")
        |""".stripMargin)

    "recognise all call node" in {
      cpg.call.name("printValue").l.size shouldBe 1
    }

    "recognise methodFullName for call node" in {
      cpg.call.name("printValue").head.methodFullName should equal("Hello.printValue:<unresolvedSignature>")
    }
  }

  "Code for method full name when method present in same file for nested class" should {
    val cpg = code("""
        |module Help
        | class InnerHelp
        |   def printValue(value)
        |     puts value
        |   end
        | end
        |end
        |
        |val v = Help::InnerHelp.new
        |puts v.printValue("any")
        |""".stripMargin)

    "recognise all call node" in {
      cpg.call.name("printValue").l.size shouldBe 1
    }

    "recognise methodFullName for call node" in {
    //  cpg.call.name("printValue").head.methodFullName should equal("Help.InnerHelp.printValue:<unresolvedSignature>")
    }
  }
}
