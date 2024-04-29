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

import io.joern.swiftsrc2cpg.testfixtures.AstSwiftSrc2CpgSuite

class RegexTests extends AstSwiftSrc2CpgSuite {

  "RegexTests" should {

    "testRegex1" ignore {
      val cpg = code("""
        |_ = /abc/
        |_ = #/abc/#
        |_ = ##/abc/##
        |""".stripMargin)
      ???
    }

    "testRegex3" ignore {
      val cpg = code("foo(/abc/, #/abc/#, ##/abc/##)")
      ???
    }

    "testRegex4" ignore {
      val cpg = code("let arr = [/abc/, #/abc/#, ##/abc/##]")
      ???
    }

    "testRegex5" ignore {
      val cpg = code("""
        |_ = /\w+/.self
        |_ = #/\w+/#.self
        |_ = ##/\w+/##.self
        |""".stripMargin)
      ???
    }

  }

}
