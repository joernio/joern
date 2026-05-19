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

import io.joern.swiftsrc2cpg.testfixtures.SwiftSrc2CpgSuite
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.semanticcpg.language.*

class RegexTests extends SwiftSrc2CpgSuite {

  "RegexTests" should {

    "testRegex1" in {
      val cpg = code("""
        |_ = /abc/
        |_ = #/abc/#
        |_ = ##/abc/##
        |""".stripMargin)
      val assigns = cpg.call.nameExact(Operators.assignment).code.l
      assigns shouldBe List("_ = /abc/", "_ = #/abc/#", "_ = ##/abc/##")
      cpg.unknown.code.l shouldBe List("/abc/", "#/abc/#", "##/abc/##")
    }

    "testRegex3" in {
      val cpg           = code("foo(/abc/, #/abc/#, ##/abc/##)")
      val List(fooCall) = cpg.call.nameExact("foo").l
      fooCall.code shouldBe "foo(/abc/, #/abc/#, ##/abc/##)"
      cpg.unknown.code.l shouldBe List("/abc/", "#/abc/#", "##/abc/##")
    }

    "testRegex4" in {
      val cpg             = code("let arr = [/abc/, #/abc/#, ##/abc/##]")
      val List(arrayInit) = cpg.call.nameExact(Operators.arrayInitializer).l
      arrayInit.code shouldBe "[/abc/, #/abc/#, ##/abc/##]"
      arrayInit.argument.code.l shouldBe List("/abc/", "#/abc/#", "##/abc/##")
      cpg.unknown.code.l shouldBe List("/abc/", "#/abc/#", "##/abc/##")
    }

    "testRegex5" in {
      val cpg = code("""
        |_ = /\w+/.self
        |_ = #/\w+/#.self
        |_ = ##/\w+/##.self
        |""".stripMargin)
      val assigns = cpg.call.nameExact(Operators.assignment).code.l
      assigns shouldBe List("_ = /\\w+/.self", "_ = #/\\w+/#.self", "_ = ##/\\w+/##.self")
      cpg.unknown.code.l should contain allOf ("/\\w+/", "#/\\w+/#", "##/\\w+/##")
    }

  }

}
