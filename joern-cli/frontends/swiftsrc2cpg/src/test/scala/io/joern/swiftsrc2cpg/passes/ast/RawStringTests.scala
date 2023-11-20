

// This test file has been translated from swift/test/StringProcessing/Parse/raw_string.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.shiftleft.codepropertygraph.generated._
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._

class RawStringTests extends AbstractPassTest {
  "testRawString1" ignore AstFixture("") { cpg =>
    assertParse(
      """
      import Swift
      """
    )
  }

  "testRawString2" ignore AstFixture("") { cpg =>
    assertParse(
      ##"""
      _ = #"""
      ###################################################################
      ## This source file is part of the Swift.org open source project ##
      ###################################################################
      """#
      """##
    )
  }

  "testRawString3" ignore AstFixture("") { cpg =>
    assertParse(
      ####"""
      _ = #"""
          # H1 #
          ## H2 ##
          ### H3 ###
          """#
      """####
    )
  }

  "testRawString5" ignore AstFixture("") { cpg =>
    assertParse(
      ###"""
      _ = ##"""
          One
          ""Alpha""
          """##
      """###
    )
  }

  "testRawString6" ignore AstFixture("") { cpg =>
    assertParse(
      ###"""
      _ = ##"""
          Two
        Beta
        """##
      """###
    )
  }

  "testRawString7" ignore AstFixture("") { cpg =>
    assertParse(
      ##"""
      _ = #"""
          Three\r
          Gamma\
        """#
      """##
    )
  }

  "testRawString8" ignore AstFixture("") { cpg =>
    assertParse(
      ####"""
      _ = ###"""
          Four \(foo)
          Delta
      """###
      """####
    )
  }

  "testRawString9" ignore AstFixture("") { cpg =>
    assertParse(
      ###"""
      _ = ##"""
        print("""
          Five\##n\##n\##nEpsilon
          """)
        """##
      """###
    )
  }

  "testRawString10" ignore AstFixture("") { cpg =>
    assertParse(
      """
      // ===---------- Single line --------===
      """
    )
  }

  "testRawString11" ignore AstFixture("") { cpg =>
    assertParse(
      ##"""
      _ = #""Zeta""#
      """##
    )
  }

  "testRawString12" ignore AstFixture("") { cpg =>
    assertParse(
      ##"""
      _ = #""Eta"\#n\#n\#n\#""#
      """##
    )
  }

  "testRawString13" ignore AstFixture("") { cpg =>
    assertParse(
      ##"""
      _ = #""Iota"\n\n\n\""#
      """##
    )
  }

  "testRawString14" ignore AstFixture("") { cpg =>
    assertParse(
      ##"""
      _ = #"a raw string with \" in it"#
      """##
    )
  }

  "testRawString15" ignore AstFixture("") { cpg =>
    assertParse(
      ###"""
      _ = ##"""
            a raw string with """ in it
            """##
      """###
    )
  }

  "testRawString16" ignore AstFixture("") { cpg =>
    assertParse(
      """
      // ===---------- False Multiline Delimiters --------===
      """
    )
  }

  "testRawString17" ignore AstFixture("") { cpg =>
    assertParse(
      ##"""
      /// Source code contains zero-width character in this format: `#"[U+200B]"[U+200B]"#`
      /// The check contains zero-width character in this format: `"[U+200B]\"[U+200B]"`
      /// If this check fails after you implement `diagnoseZeroWidthMatchAndAdvance`,
      /// then you may need to tweak how to test for single-line string literals that
      /// resemble a multiline delimiter in `advanceIfMultilineDelimiter` so that it
      /// passes again.
      /// See https://github.com/apple/swift/issues/51192.
      _ = #"​"​"#
      """##
    )
  }

  "testRawString18" ignore AstFixture("") { cpg =>
    assertParse(
      ##"""
      _ = #""""#
      """##
    )
  }

  "testRawString19" ignore AstFixture("") { cpg =>
    assertParse(
      ##"""
      _ = #"""""#
      """##
    )
  }

  "testRawString20" ignore AstFixture("") { cpg =>
    assertParse(
      ##"""
      _ = #""""""#
      """##
    )
  }

  "testRawString21" ignore AstFixture("") { cpg =>
    assertParse(
      ##"""
      _ = #"""#
      """##
    )
  }

  "testRawString22" ignore AstFixture("") { cpg =>
    assertParse(
      ###"""
      _ = ##""" foo # "# "##
      """###
    )
  }

  "testRawString23" ignore AstFixture("") { cpg =>
    assertParse(
      ####"""
      _ = ###""" "# "## "###
      """####
    )
  }

  "testRawString24" ignore AstFixture("") { cpg =>
    assertParse(
      ####"""
      _ = ###"""##"###
      """####
    )
  }

  "testRawString25" ignore AstFixture("") { cpg =>
    assertParse(
      ##"""
      _ = "interpolating \(#"""false delimiter"#)"
      """##
    )
  }

  "testRawString26" ignore AstFixture("") { cpg =>
    assertParse(
      ##"""
      _ = """
        interpolating \(#"""false delimiters"""#)
        """
      """##
    )
  }

  "testRawString27" ignore AstFixture("") { cpg =>
    assertParse(
      ##"""
      let foo = "Interpolation"
      _ = #"\b\b \#(foo)\#(foo) Kappa"#
      """##
    )
  }

  "testRawString28" ignore AstFixture("") { cpg =>
    assertParse(
      ###"""
      _ = """
        interpolating \(##"""
          delimited \##("string")\#n\##n
          """##)
        """
      """###
    )
  }

  "testRawString30" ignore AstFixture("") { cpg =>
    assertParse(
      ##"""
      #"unused literal"#
      """##
    )
  }

  "testRawString32" ignore AstFixture("") { cpg =>
    assertParse(
      ##"""
      _ = #"This is a string"#
      """##
    )
  }

  "testRawString33" ignore AstFixture("") { cpg =>
    assertParse(
      ######"""
      _ = #####"This is a string"#####
      """######
    )
  }

  "testRawString34" ignore AstFixture("") { cpg =>
    assertParse(
      ###"""
      _ = #"enum\s+.+\{.*case\s+[:upper:]"#
      _ = #"Alice: "How long is forever?" White Rabbit: "Sometimes, just one second.""#
      _ = #"\#\#1"#
      _ = ##"\#1"##
      _ = #"c:\windows\system32"#
      _ = #"\d{3) \d{3} \d{4}"#
      _ = #"""
          a string with
          """
          in it
          """#
      _ = #"a raw string containing \r\n"#
      _ = #"""
          [
              {
                  "id": "12345",
                  "title": "A title that \"contains\" \\\""
              }
          ]
          """#
      _ = #"# #"#
      """###
    )
  }
}
