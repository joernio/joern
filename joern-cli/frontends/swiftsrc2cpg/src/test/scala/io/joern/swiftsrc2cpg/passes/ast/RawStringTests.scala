// This test file has been translated from swift/test/StringProcessing/Parse/raw_string.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.joern.swiftsrc2cpg.testfixtures.SwiftSrc2CpgSuite
import io.shiftleft.semanticcpg.language.*

class RawStringTests extends SwiftSrc2CpgSuite {

  // The swiftastgen-driven CPG does not yet collapse multi-line raw strings (`#"""..."""#`)
  // into a single literal. The triple-quote opener and closer are emitted as their own
  // literals, and an interior line that happens to start with `\"\"` is split out as well.
  // These tests lock that behavior in so any future single-literal collapsing shows up
  // as an intentional change.

  // Helpers to keep expected literal codes readable. The CPG records each literal's `code`
  // verbatim including its surrounding quotes, so e.g. `\"\"` in source becomes the 6-char
  // string [", \, ", \, ", "] on the literal node.
  private val Q   = "\""
  private val EQ  = "\\\""  // escaped quote inside the source: \"
  private val EQQ = EQ + EQ // \"\"

  "RawStringTests" should {

    "testRawString2" in {
      val cpg = code("""
      |_ = #\"\"\"
      |      ###################################################################
      |      ## This source file is part of the Swift.org open source project ##
      |      ###################################################################
      |      \"\"\"#
      |""".stripMargin)
      cpg.literal.code.l shouldBe List(Q + EQQ + Q, Q + EQQ + "#" + Q)
    }

    "testRawString3" in {
      val cpg = code("""
      |_ = #\"\"\"
      |      # H1 #
      |      ## H2 ##
      |      ### H3 ###
      |      \"\"\"#
      |""".stripMargin)
      cpg.literal.code.l shouldBe List(Q + EQQ + Q, Q + EQQ + "#" + Q)
    }

    "testRawString5" in {
      val cpg = code("""
      |_ = ##\"\"\"
      |          One
      |          ""Alpha""
      |          \"\"\"##
      |""".stripMargin)
      cpg.literal.code.l shouldBe List(Q + EQQ + Q, "\"\"", "\"\"", Q + EQQ + "##" + Q)
    }

    "testRawString6" in {
      val cpg = code("""
      |_ = ##\"\"\"
      |          Two
      |        Beta
      |        \"\"\"##
      |""".stripMargin)
      cpg.literal.code.l shouldBe List(Q + EQQ + Q, Q + EQQ + "##" + Q)
    }

    "testRawString7" in {
      val cpg = code("""
      |_ = #\"\"\"
      |          Three\r
      |          Gamma\
      |        \"\"\"#
      |""".stripMargin)
      cpg.literal.code.l shouldBe List(Q + EQQ + Q, Q + EQQ + "#" + Q)
    }

    "testRawString8" in {
      val cpg = code("""
      |_ = ###\"\"\"
      |          Four \(foo)
      |          Delta
      |      \"\"\"###
      |""".stripMargin)
      cpg.literal.code.l shouldBe List(Q + EQQ + Q, Q + EQQ + "###" + Q)
    }

    "testRawString9" in {
      val cpg = code("""
      |_ = ##\"\"\"
      |        print(\"\"\"
      |          Five\##n\##n\##nEpsilon
      |          \"\"\")
      |        \"\"\"##
      |""".stripMargin)
      cpg.literal.code.l shouldBe List(Q + EQQ + Q, Q + EQQ + Q, Q + EQQ + ")" + Q, Q + EQQ + "##" + Q)
    }

    "testRawString11" in {
      val cpg = code("""_ = #""Zeta""#""")
      cpg.literal.code.l shouldBe List("\"\"Zeta\"\"")
    }

    "testRawString12" in {
      val cpg = code("""_ = #""Eta"\#n\#n\#n\#""#""")
      cpg.literal.code.l shouldBe List(Q + "\"Eta\"\\#n" + Q, Q + "\\#n" + Q, Q + "\\#n" + Q, Q + "\\#" + Q + Q)
    }

    "testRawString13" in {
      val cpg = code("""_ = #""Iota"\n\n\n\""#""")
      cpg.literal.code.l shouldBe List(Q + "\"Iota\"\\n\\n\\n\\\"" + Q)
    }

    "testRawString14" in {
      val cpg = code("""_ = #"a raw string with \" in it"#""")
      cpg.literal.code.l shouldBe List("\"a raw string with \\\" in it\"")
    }

    "testRawString15" in {
      val cpg = code("""
      |_ = ##\"\"\"
      |            a raw string with \"\"\" in it
      |            \"\"\"##
      |""".stripMargin)
      cpg.literal.code.l shouldBe List(Q + EQQ + Q, Q + EQQ + " in it" + Q, Q + EQQ + "##" + Q)
    }

    "testRawString17" in {
      val cpg = code("""_ = #"​"​"#""")
      // Zero-width-space-wrapped raw string: the lexer produces a single literal containing the ZWSPs.
      cpg.literal.code.l shouldBe List("\"​\"​\"")
    }

    "testRawString32" in {
      val cpg = code("""_ = #"This is a string"#""")
      cpg.literal.code.l shouldBe List("\"This is a string\"")
    }

    "testRawString33" in {
      val cpg = code("""_ = #####"This is a string"#####""")
      cpg.literal.code.l shouldBe List("\"This is a string\"")
    }

  }
}
