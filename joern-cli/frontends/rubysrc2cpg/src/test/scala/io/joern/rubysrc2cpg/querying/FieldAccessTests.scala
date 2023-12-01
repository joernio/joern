package io.joern.rubysrc2cpg.querying

import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.codepropertygraph.generated.nodes.Call
import io.shiftleft.semanticcpg.language.*

class FieldAccessTests extends RubyCode2CpgFixture {

  "`x.y` is represented by an `x.y` CALL without arguments" in {
    val cpg = code("""
                     |x.y
                     |""".stripMargin)

    val List(fieldAccess) = cpg.fieldAccess.l

    fieldAccess.code shouldBe "x.y"
    fieldAccess.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
    fieldAccess.lineNumber shouldBe Some(2)
    fieldAccess.fieldIdentifier.code.l shouldBe List("y")

    val List(fieldAccessCall: Call) = fieldAccess.astParent.toList: @unchecked

    fieldAccessCall.code shouldBe "x.y"
    fieldAccessCall.name shouldBe "y"
    fieldAccessCall.receiver.l shouldBe List(fieldAccess)
  }

}
