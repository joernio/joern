package io.joern.rubysrc2cpg.querying

import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Identifier}
import io.shiftleft.semanticcpg.language.*

class FieldAccessTests extends RubyCode2CpgFixture {

  "`x.y` is represented by an `x.y` CALL without arguments" in {
    val cpg = code("""
                     |x.y
                     |""".stripMargin)

    inside(cpg.call("y").headOption) {
      case Some(xyCall) =>
        xyCall.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH
        xyCall.lineNumber shouldBe Some(2)
        xyCall.code shouldBe "x.y"

        inside(xyCall.receiver.headOption) {
          case Some(x: Identifier) =>
            x.name shouldBe "x"
          case _ => fail("Expected an identifier receiver")
        }
      case None => fail("Expected a call with the name `y`")
    }
  }

  "`self.x` should correctly create a `this` node field base" in {

    // Example from railsgoat
    val cpg = code("""
        |class PaidTimeOff < ApplicationRecord
        |  belongs_to :user
        |  has_many :schedule, foreign_key: :user_id, primary_key: :user_id, dependent: :destroy
        |
        |  def sick_days_remaining
        |    self.sick_days_earned - self.sick_days_taken
        |  end
        |end
        |""".stripMargin)

    inside(cpg.call.name("sick_days_earned").l) {
      case sickDays :: _ =>
        sickDays.code shouldBe "self.sick_days_earned"
        sickDays.name shouldBe "sick_days_earned"
        sickDays.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH

        inside(sickDays.argument.l) {
          case (self: Identifier) :: Nil =>
            self.name shouldBe "this"
            self.code shouldBe "self"
            self.typeFullName should endWith("PaidTimeOff")
          case xs => fail(s"Expected exactly two field access arguments, instead got [${xs.code.mkString(", ")}]")
        }
      case Nil => fail("Expected at least one call with `self` base, but got none.")
    }
  }

}
