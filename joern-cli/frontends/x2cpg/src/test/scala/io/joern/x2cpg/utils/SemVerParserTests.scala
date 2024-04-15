package io.joern.x2cpg.utils

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.util.{Failure, Success}

class SemVerParserTests extends AnyWordSpec with Matchers {

  "a basic major.minor.patch version should parse successfully" in {
    val semVer = SemVer("1.9.1")
    semVer.major shouldBe 1
    semVer.minor shouldBe 9
    semVer.patch shouldBe 1
  }

  "a basic major.minor version should parse successfully with defaults" in {
    val semVer = SemVer("1.9")
    semVer.major shouldBe 1
    semVer.minor shouldBe 9
    semVer.patch shouldBe 0
  }

  "a negative integer in the core version should throw an exception" in {
    assertThrows[SemVerParsingException](SemVer("1.-9.0"))
  }

  "an empty string should throw an exception" in {
    assertThrows[SemVerParsingException](SemVer(""))
  }

  "a version with a pre-release entry should parse successfully" in {
    SemVer("1.0.0-0.3.7") shouldBe SemVer(1, 0, 0, "0.3.7" :: Nil, None)
    SemVer("1.0.0-x.7.z.92") shouldBe SemVer(1, 0, 0, "x.7.z.92" :: Nil, None)
    SemVer("1.0.0-x-y-z.--") shouldBe SemVer(1, 0, 0, "x" :: "y" :: "z." :: Nil, None)
  }

  "a version with a build entry should parse successfully" in {
    SemVer("1.0.0+20130313144700") shouldBe SemVer(1, 0, 0, Nil, Option("20130313144700"))
    SemVer("1.0.0+21AF26D3----117B344092BD") shouldBe SemVer(1, 0, 0, Nil, Option("21AF26D3----117B344092BD"))
  }

  "a version with both a pre-release and build entry should parse successfully" in {
    SemVer("1.0.0-alpha+001") shouldBe SemVer(1, 0, 0, "alpha" :: Nil, Option("001"))
    SemVer("1.0.0-beta+exp.sha.5114f85") shouldBe SemVer(1, 0, 0, "beta" :: Nil, Option("exp.sha.5114f85"))
  }

  "two versions with the same version core should be equal, regardless of build meta" in {
    SemVer("1.0.0") shouldEqual SemVer("1.0.0")
    SemVer("1.0.0+foo") shouldEqual SemVer("1.0.0")
    SemVer("1.0.0+foo") shouldEqual SemVer("1.0.0+bar")
  }

  "two versions with differing versions should obey major.minor.patch and pre-release precedence" in {
    // In this test, the set disregards order, then we convert to list and sort
    Set(SemVer("1.0.0"), SemVer("2.0.0"), SemVer("2.1.0"), SemVer("2.1.1"), SemVer("1.0.0-alpha")).toList.sorted
      .map(_.toString) shouldBe List("1.0.0-alpha", "1.0.0", "2.0.0", "2.1.0", "2.1.1")
  }

}
