// This test file has been translated from swift/test/StringProcessing/Parse/raw_string.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.joern.swiftsrc2cpg.testfixtures.AstSwiftSrc2CpgSuite

class RawStringTests extends AstSwiftSrc2CpgSuite {

  "RawStringTests" should {

    "testRawString2" ignore {
      val cpg = code("""
      |_ = #\"\"\"
      |      ###################################################################
      |      ## This source file is part of the Swift.org open source project ##
      |      ###################################################################
      |      \"\"\"#
      |""".stripMargin)
      ???
    }

    "testRawString3" ignore {
      val cpg = code("""
      |_ = #\"\"\"
      |      # H1 #
      |      ## H2 ##
      |      ### H3 ###
      |      \"\"\"#
      |""".stripMargin)
      ???
    }

    "testRawString5" ignore {
      val cpg = code("""
      |_ = ##\"\"\"
      |          One
      |          ""Alpha""
      |          \"\"\"##
      |""".stripMargin)
      ???
    }

    "testRawString6" ignore {
      val cpg = code("""
      |_ = ##\"\"\"
      |          Two
      |        Beta
      |        \"\"\"##
      |""".stripMargin)
      ???
    }

    "testRawString7" ignore {
      val cpg = code("""
      |_ = #\"\"\"
      |          Three\r
      |          Gamma\
      |        \"\"\"#
      |""".stripMargin)
      ???
    }

    "testRawString8" ignore {
      val cpg = code("""
      |_ = ###\"\"\"
      |          Four \(foo)
      |          Delta
      |      \"\"\"###
      |""".stripMargin)
      ???
    }

    "testRawString9" ignore {
      val cpg = code("""
      |_ = ##\"\"\"
      |        print(\"\"\"
      |          Five\##n\##n\##nEpsilon
      |          \"\"\")
      |        \"\"\"##
      |""".stripMargin)
      ???
    }

    "testRawString11" ignore {
      val cpg = code("""_ = #""Zeta""#""")
      ???
    }

    "testRawString12" ignore {
      val cpg = code("""_ = #""Eta"\#n\#n\#n\#""#""")
      ???
    }

    "testRawString13" ignore {
      val cpg = code("""_ = #""Iota"\n\n\n\""#""")
      ???
    }

    "testRawString14" ignore {
      val cpg = code("""_ = #"a raw string with \" in it"#""")
      ???
    }

    "testRawString15" ignore {
      val cpg = code("""
      |_ = ##\"\"\"
      |            a raw string with \"\"\" in it
      |            \"\"\"##
      |""".stripMargin)
      ???
    }

    "testRawString17" ignore {
      val cpg = code("""_ = #"​"​"#""")
      ???
    }

    "testRawString32" ignore {
      val cpg = code("""_ = #"This is a string"#""")
      ???
    }

    "testRawString33" ignore {
      val cpg = code("""_ = #####"This is a string"#####""")
      ???
    }

  }
}
