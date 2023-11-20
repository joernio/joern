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
  "testRegex1" ignore AstFixture("") { cpg =>
    assertParse(
      """
      _ = /abc/
      _ = #/abc/#
      _ = ##/abc/##
      """
    )
  }

  "testRegex3" ignore AstFixture("") { cpg =>
    assertParse(
      """
      foo(/abc/, #/abc/#, ##/abc/##)
      """
    )
  }

  "testRegex4" ignore AstFixture("") { cpg =>
    assertParse(
      """
      let arr = [/abc/, #/abc/#, ##/abc/##]
      """
    )
  }

  "testRegex5" ignore AstFixture("") { cpg =>
    assertParse(
      #"""
      _ = /\w+/.self
      _ = #/\w+/#.self
      _ = ##/\w+/##.self
      """#
    )
  }

  "testRegex6" ignore AstFixture("") { cpg =>
    assertParse(
      ##"""
      _ = /#\/\#\\/
      _ = #/#/\/\#\\/#
      _ = ##/#|\|\#\\/##
      """##
    )
  }

  "testRegex7" ignore AstFixture("") { cpg =>
    assertParse(
      """
      _ = (#/[*/#, #/+]/#, #/.]/#)
      """
    )
  }

}
