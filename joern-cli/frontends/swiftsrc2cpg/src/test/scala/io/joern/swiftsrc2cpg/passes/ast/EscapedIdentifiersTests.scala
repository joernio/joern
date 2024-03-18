// This test file has been translated from swift/test/Parse/escaped_identifiers.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.joern.swiftsrc2cpg.testfixtures.AstSwiftSrc2CpgSuite

class EscapedIdentifiersTests extends AstSwiftSrc2CpgSuite {

  "EscapedIdentifiersTests" should {

    "testEscapedIdentifiers1" ignore {
      val cpg = code("""
      func `protocol`() {}
      """)
      ???
    }

    "testEscapedIdentifiers2" ignore {
      val cpg = code("""
      `protocol`()
      """)
      ???
    }

    "testEscapedIdentifiers3" ignore {
      val cpg = code("""
      class `Type` {}
      """)
      ???
    }

    "testEscapedIdentifiers4" ignore {
      val cpg = code("""
      var `class` = `Type`.self
      """)
      ???
    }

    "testEscapedIdentifiers5" ignore {
      val cpg = code("""
      func foo() {}
      """)
      ???
    }

    "testEscapedIdentifiers6" ignore {
      val cpg = code("""
      `foo`()
      """)
      ???
    }

    "testEscapedIdentifiers7" ignore {
      val cpg = code("""
      // Escaping suppresses identifier contextualization.
      var get: (() -> ()) -> () = { $0() }
      """)
      ???
    }

    "testEscapedIdentifiers8" ignore {
      val cpg = code("""
      var applyGet: Int {
        `get` { }
        return 0
      }
      """)
      ???
    }

    "testEscapedIdentifiers9" ignore {
      val cpg = code("""
      enum `switch` {}
      """)
      ???
    }

    "testEscapedIdentifiers10" ignore {
      val cpg = code("""
      typealias `Self` = Int
      """)
      ???
    }

  }

}
