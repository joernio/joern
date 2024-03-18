// This test file has been translated from swift/test/Parse/hashbang_library.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.joern.swiftsrc2cpg.testfixtures.AstSwiftSrc2CpgSuite

class HashbangLibraryTests extends AstSwiftSrc2CpgSuite {

  "HashbangLibraryTests" should {

    "testHashbangLibrary1" ignore {
      val cpg = code("""
        |#!/usr/bin/swift
        |class Foo {}
        |""".stripMargin)
      ???
    }

  }

}
