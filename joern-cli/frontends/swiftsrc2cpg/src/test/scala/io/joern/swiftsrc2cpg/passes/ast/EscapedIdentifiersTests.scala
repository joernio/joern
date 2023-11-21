// This test file has been translated from swift/test/Parse/escaped_identifiers.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.shiftleft.codepropertygraph.generated._
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._

class EscapedIdentifiersTests extends AbstractPassTest {

  "EscapedIdentifiersTests" should {

    "testEscapedIdentifiers1" ignore AstFixture("""
      func `protocol`() {}
      """) { cpg => ??? }

    "testEscapedIdentifiers2" ignore AstFixture("""
      `protocol`()
      """) { cpg => ??? }

    "testEscapedIdentifiers3" ignore AstFixture("""
      class `Type` {}
      """) { cpg => ??? }

    "testEscapedIdentifiers4" ignore AstFixture("""
      var `class` = `Type`.self
      """) { cpg => ??? }

    "testEscapedIdentifiers5" ignore AstFixture("""
      func foo() {}
      """) { cpg => ??? }

    "testEscapedIdentifiers6" ignore AstFixture("""
      `foo`()
      """) { cpg => ??? }

    "testEscapedIdentifiers7" ignore AstFixture("""
      // Escaping suppresses identifier contextualization.
      var get: (() -> ()) -> () = { $0() }
      """) { cpg => ??? }

    "testEscapedIdentifiers8" ignore AstFixture("""
      var applyGet: Int {
        `get` { }
        return 0
      }
      """) { cpg => ??? }

    "testEscapedIdentifiers9" ignore AstFixture("""
      enum `switch` {}
      """) { cpg => ??? }

    "testEscapedIdentifiers10" ignore AstFixture("""
      typealias `Self` = Int
      """) { cpg => ??? }

  }

}
