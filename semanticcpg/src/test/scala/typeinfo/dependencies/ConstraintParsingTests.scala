package io.shiftleft.semanticcpg.typeinfo.dependencies

import io.shiftleft.semanticcpg.typeinfo._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ConstraintParsingTests extends AnyWordSpec with Matchers {
  "basic semver" should {
    "parse" in {
      Constraint.parse("1.0.0") shouldEqual Eq(SemVer("1.0.0"))
    }
  }
  
  "conjunction of semver" should {
    "parse" in {
      Constraint.parse("!0.0.0 && <= 2.0.0 && >= 1.0.0") shouldEqual 
        And(
          And(
            Not(Eq(SemVer("0.0.0"))), 
            Lte(Eq(SemVer("2.0.0")))),
          Gte(Eq(SemVer("1.0.0"))))
    }
  }
  
  "parenthesized semver" should {
    "parse" in {
      Constraint.parse("(0.0.0 || 2.0.0) && >= 1.0.0") shouldEqual
        And(
          Or(Eq(SemVer("0.0.0")), Eq(SemVer("2.0.0"))),
          Gte(Eq(SemVer("1.0.0")))
        )
    }
  }
}
