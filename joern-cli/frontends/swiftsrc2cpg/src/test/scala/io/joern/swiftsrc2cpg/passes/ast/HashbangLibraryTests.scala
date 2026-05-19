// This test file has been translated from swift/test/Parse/hashbang_library.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.joern.swiftsrc2cpg.testfixtures.SwiftSrc2CpgSuite
import io.shiftleft.semanticcpg.language.*

class HashbangLibraryTests extends SwiftSrc2CpgSuite {

  "HashbangLibraryTests" should {

    "testHashbangLibrary1" in {
      val cpg = code("""
        |#!/usr/bin/swift
        |class Foo {}
        |""".stripMargin)
      // The shebang line must not produce any CPG node — the class decl should still be parsed.
      val List(fooType) = cpg.typeDecl.nameExact("Foo").l
      fooType.fullName shouldBe "Test0.swift:<global>.Foo"
    }

  }

}
