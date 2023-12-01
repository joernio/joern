package io.joern.rubysrc2cpg.deprecated.passes.ast

import io.joern.rubysrc2cpg.testfixtures.{DifferentInNewFrontend, RubyCode2CpgFixture}
import io.shiftleft.codepropertygraph.generated.nodes.MethodRef
import io.shiftleft.semanticcpg.language.*

class ReturnTests extends RubyCode2CpgFixture(useDeprecatedFrontend = true) {

  "a method, where the last statement is a method" should {
    val cpg = code("""
        |Row = Struct.new(:cancel_date) do
        |    def end_date = cancel_date
        |end
        |""".stripMargin)

    "return a method ref" taggedAs DifferentInNewFrontend in {
      val List(mRef: MethodRef) = cpg.method("new2").ast.isReturn.astChildren.l: @unchecked
      mRef.methodFullName shouldBe "Test0.rb::program.end_date"
    }
  }

}
