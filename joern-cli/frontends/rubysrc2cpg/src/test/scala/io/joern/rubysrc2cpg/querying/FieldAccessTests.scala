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

    inside(cpg.fieldAccess.code("self.*").l) {
      case sickDays :: _ =>
        sickDays.code shouldBe "self.sick_days_earned"
        sickDays.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH

        inside(sickDays.argument.l) {
          case (self: Identifier) :: sickDaysEarned :: Nil =>
            self.name shouldBe "this"
            self.code shouldBe "self"
            self.typeFullName should endWith("PaidTimeOff")

            sickDaysEarned.code shouldBe "sick_days_earned"
          case xs => fail(s"Expected exactly two field access arguments, instead got [${xs.code.mkString(", ")}]")
        }
      case Nil => fail("Expected at least one field access with `self` base, but got none.")
    }
  }

}
