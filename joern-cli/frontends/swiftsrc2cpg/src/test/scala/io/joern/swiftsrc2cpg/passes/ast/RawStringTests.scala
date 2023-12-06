// This test file has been translated from swift/test/StringProcessing/Parse/raw_string.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.shiftleft.codepropertygraph.generated._
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._

class RawStringTests extends AbstractPassTest {

  "RawStringTests" should {

    "testRawString2" ignore AstFixture("""
      |_ = #\"\"\"
      |      ###################################################################
      |      ## This source file is part of the Swift.org open source project ##
      |      ###################################################################
      |      \"\"\"#
      |""".stripMargin) { cpg => ??? }

    "testRawString3" ignore AstFixture("""
      |_ = #\"\"\"
      |      # H1 #
      |      ## H2 ##
      |      ### H3 ###
      |      \"\"\"#
      |""".stripMargin) { cpg => ??? }

    "testRawString5" ignore AstFixture("""
      |_ = ##\"\"\"
      |          One
      |          ""Alpha""
      |          \"\"\"##
      |""".stripMargin) { cpg => ??? }

    "testRawString6" ignore AstFixture("""
      |_ = ##\"\"\"
      |          Two
      |        Beta
      |        \"\"\"##
      |""".stripMargin) { cpg => ??? }

    "testRawString7" ignore AstFixture("""
      |_ = #\"\"\"
      |          Three\r
      |          Gamma\
      |        \"\"\"#
      |""".stripMargin) { cpg => ??? }

    "testRawString8" ignore AstFixture("""
      |_ = ###\"\"\"
      |          Four \(foo)
      |          Delta
      |      \"\"\"###
      |""".stripMargin) { cpg => ??? }

    "testRawString9" ignore AstFixture("""
      |_ = ##\"\"\"
      |        print(\"\"\"
      |          Five\##n\##n\##nEpsilon
      |          \"\"\")
      |        \"\"\"##
      |""".stripMargin) { cpg => ??? }

    "testRawString11" ignore AstFixture("""_ = #""Zeta""#""") { cpg => ??? }

    "testRawString12" ignore AstFixture("""_ = #""Eta"\#n\#n\#n\#""#""") { cpg => ??? }

    "testRawString13" ignore AstFixture("""_ = #""Iota"\n\n\n\""#""") { cpg => ??? }

    "testRawString14" ignore AstFixture("""_ = #"a raw string with \" in it"#""") { cpg => ??? }

    "testRawString15" ignore AstFixture("""
      |_ = ##\"\"\"
      |            a raw string with \"\"\" in it
      |            \"\"\"##
      |""".stripMargin) { cpg => ??? }

    "testRawString17" ignore AstFixture("""_ = #"​"​"#""") { cpg => ??? }

    "testRawString32" ignore AstFixture("""_ = #"This is a string"#""") { cpg => ??? }

    "testRawString33" ignore AstFixture("""_ = #####"This is a string"#####""") { cpg => ??? }

  }
}
