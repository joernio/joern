// This test file has been translated from swift/test/Parse/object_literals.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.joern.swiftsrc2cpg.testfixtures.AstSwiftSrc2CpgSuite

class ObjectLiteralsTests extends AstSwiftSrc2CpgSuite {

  "ObjectLiteralsTests" should {

    "testObjectLiterals2a" ignore {
      val cpg = code("let _ = #Color(colorLiteralRed: red, green: green, blue: blue, alpha: alpha)")
      ???
    }

    "testObjectLiterals2b" ignore {
      val cpg = code("let _ = #Image(imageLiteral: localResourceNameAsString)")
      ???
    }

    "testObjectLiterals3a" ignore {
      val cpg = code("let _ = #notAPound")
      ???
    }

    "testObjectLiterals3b" ignore {
      val cpg = code("let _ = #notAPound(1, 2)")
      ???
    }

    "testObjectLiterals3c" ignore {
      val cpg = code("let _ = #Color")
      ???
    }

    "testObjectLiterals5" ignore {
      val cpg = code(" let _ = [#Color(_: 1, green: 1, 2)]")
      ???
    }

    "testObjectLiterals8" ignore {
      val cpg = code(" let _ = #Color(_: 1, green: 1)")
      ???
    }

  }

}
