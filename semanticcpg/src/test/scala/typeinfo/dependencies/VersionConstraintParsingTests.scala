package io.shiftleft.semanticcpg.typeinfo.dependencies

import io.shiftleft.semanticcpg.typeinfo.*
import io.shiftleft.semanticcpg.typeinfo.version.SemVer2
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class VersionConstraintParsingTests extends AnyWordSpec with Matchers {
  "basic semver" should {
    "parse" in {
      VersionConstraint.parse("1.0.0", SemVer2.apply) shouldEqual Eq(SemVer2("1.0.0"))
    }
  }

  "conjunction of semver" should {
    "parse" in {
      VersionConstraint.parse("!0.0.0 && <= 2.0.0 && >= 1.0.0", SemVer2.apply) shouldEqual
        And(And(Not(Eq(SemVer2("0.0.0"))), Lte(Eq(SemVer2("2.0.0")))), Gte(Eq(SemVer2("1.0.0"))))
    }
  }

  "parenthesized semver" should {
    "parse" in {
      VersionConstraint.parse("(0.0.0 || 2.0.0) && >= 1.0.0", SemVer2.apply) shouldEqual
        And(Or(Eq(SemVer2("0.0.0")), Eq(SemVer2("2.0.0"))), Gte(Eq(SemVer2("1.0.0"))))
    }
  }
}
