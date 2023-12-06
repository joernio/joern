//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// This test file has been translated from swift/test/StringProcessing/Parse/regex.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.shiftleft.codepropertygraph.generated._
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._

class RegexTests extends AbstractPassTest {

  "RegexTests" should {

    "testRegex1" ignore AstFixture("""
        |_ = /abc/
        |_ = #/abc/#
        |_ = ##/abc/##
        |""".stripMargin) { cpg => ??? }

    "testRegex3" ignore AstFixture("foo(/abc/, #/abc/#, ##/abc/##)") { cpg => ??? }

    "testRegex4" ignore AstFixture("let arr = [/abc/, #/abc/#, ##/abc/##]") { cpg => ??? }

    "testRegex5" ignore AstFixture("""
        |_ = /\w+/.self
        |_ = #/\w+/#.self
        |_ = ##/\w+/##.self
        |""".stripMargin) { cpg => ??? }

  }

}
