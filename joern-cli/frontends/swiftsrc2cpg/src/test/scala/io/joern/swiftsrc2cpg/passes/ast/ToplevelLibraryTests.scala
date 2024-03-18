// This test file has been translated from swift/test/Parse/toplevel_library.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.joern.swiftsrc2cpg.testfixtures.AstSwiftSrc2CpgSuite

class ToplevelLibraryTests extends AstSwiftSrc2CpgSuite {

  "ToplevelLibraryTests" should {

    "testToplevelLibrary1" ignore {
      val cpg = code("var x = 4;")
      ???
    }

    "testToplevelLibraryInvalid1" ignore {
      val cpg = code("""
      |let x = 42
      |x + x;
      |x + x;
      |// Make sure we don't crash on closures at the top level
      |{ })
      |({ 5 }())
      |""".stripMargin)
      ???
    }

    "testToplevelLibraryInvalid2" ignore {
      val cpg = code("for i in foo() {}")
      ???
    }

  }

}
