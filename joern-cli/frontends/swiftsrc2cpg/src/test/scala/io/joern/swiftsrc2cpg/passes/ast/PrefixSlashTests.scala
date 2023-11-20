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

// This test file has been translated from swift/test/StringProcessing/Parse/prefix-slash.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.shiftleft.codepropertygraph.generated._
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._

class PrefixSlashTests extends AbstractPassTest {
  "testPrefixSlash2" ignore AstFixture("") { cpg =>
    assertParse(
      """
      prefix operator /
      prefix func / <T> (_ x: T) -> T { x }
      """
    )
  }

  "testPrefixSlash4" ignore AstFixture("") { cpg =>
    assertParse(
      """
      _ = /E.e
      (/E.e).foo(/0)
      """
    )
  }

  "testPrefixSlash6" ignore AstFixture("") { cpg =>
    assertParse(
      """
      foo(/E.e, /E.e)
      foo((/E.e), /E.e)
      foo((/)(E.e), /E.e)
      """
    )
  }

  "testPrefixSlash8" ignore AstFixture("") { cpg =>
    assertParse(
      """
      _ = bar(/E.e) / 2
      """
    )
  }
}
