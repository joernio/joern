package io.joern.rubysrc2cpg.passes

import io.joern.rubysrc2cpg.datastructures.{RubyMethod, RubyProgramSummary, RubyStubbedType, RubyType}
import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.joern.x2cpg.Defines as XDefines
import io.shiftleft.semanticcpg.language.importresolver.*
import io.shiftleft.semanticcpg.language.*

import scala.collection.immutable.List

class RubyTypeRecoveryTests extends RubyCode2CpgFixture(withPostProcessing = true, withDataFlow = true) {
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

    "resolve 'print' and 'puts' StubbedRubyType calls" in {
      val List(printCall) = cpg.call("print").l
      printCall.methodFullName shouldBe "__builtin:print"
      val List(maxCall) = cpg.call("puts").l
      maxCall.methodFullName shouldBe "__builtin:puts"
    }

    "present the declared method name when a built-in with the same name is used in the same compilation unit" in {
      val List(absCall) = cpg.call("sleep").l
      absCall.methodFullName shouldBe "main.rb:<global>::program:sleep"
    }
  }
}
