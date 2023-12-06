// This test file has been translated from swift/test/Parse/toplevel_library.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.shiftleft.codepropertygraph.generated._
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._

class ToplevelLibraryTests extends AbstractPassTest {

  "ToplevelLibraryTests" should {

    "testToplevelLibrary1" ignore AstFixture("var x = 4;") { cpg => ??? }

    "testToplevelLibraryInvalid1" ignore AstFixture("""
      |let x = 42
      |x + x;
      |x + x;
      |// Make sure we don't crash on closures at the top level
      |{ })
      |({ 5 }())
      |""".stripMargin) { cpg => ??? }

    "testToplevelLibraryInvalid2" ignore AstFixture("for i in foo() {}") { cpg => ??? }

  }

}
