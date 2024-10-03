package io.shiftleft.semanticcpg.typeinfo.dependencies

import io.shiftleft.semanticcpg.typeinfo._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class VersionConstraintParsingTests extends AnyWordSpec with Matchers {
  "basic semver" should {
    "parse" in {
      VersionConstraint.parse("1.0.0") shouldEqual Eq(RawVersion("1.0.0"))
    }
  }
  
  "conjunction of semver" should {
    "parse" in {
      VersionConstraint.parse("!0.0.0 && <= 2.0.0 && >= 1.0.0") shouldEqual 
        And(
          And(
            Not(Eq(RawVersion("0.0.0"))), 
            Lte(Eq(RawVersion("2.0.0")))),
          Gte(Eq(RawVersion("1.0.0"))))
    }
  }
  
  "parenthesized semver" should {
    "parse" in {
      VersionConstraint.parse("(0.0.0 || 2.0.0) && >= 1.0.0") shouldEqual
        And(
          Or(Eq(RawVersion("0.0.0")), Eq(RawVersion("2.0.0"))),
          Gte(Eq(RawVersion("1.0.0")))
        )
    }
  }
}
