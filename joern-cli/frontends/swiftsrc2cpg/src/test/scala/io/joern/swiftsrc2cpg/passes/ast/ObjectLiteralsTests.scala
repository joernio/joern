// This test file has been translated from swift/test/Parse/object_literals.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.joern.swiftsrc2cpg.testfixtures.SwiftSrc2CpgSuite
import io.shiftleft.semanticcpg.language.*

class ObjectLiteralsTests extends SwiftSrc2CpgSuite {

  "ObjectLiteralsTests" should {

    "testObjectLiterals2a" in {
      val cpg             = code("let _ = #Color(colorLiteralRed: red, green: green, blue: blue, alpha: alpha)")
      val List(colorCall) = cpg.call.nameExact("Color").l
      colorCall.code shouldBe "#Color(colorLiteralRed: red, green: green, blue: blue, alpha: alpha)"
      colorCall.ast.isIdentifier.name.l shouldBe List("red", "green", "blue", "alpha")
    }

    "testObjectLiterals2b" in {
      val cpg             = code("let _ = #Image(imageLiteral: localResourceNameAsString)")
      val List(imageCall) = cpg.call.nameExact("Image").l
      imageCall.code shouldBe "#Image(imageLiteral: localResourceNameAsString)"
      imageCall.ast.isIdentifier.name.l shouldBe List("localResourceNameAsString")
    }

    "testObjectLiterals3a" in {
      val cpg                 = code("let _ = #notAPound")
      val List(notAPoundCall) = cpg.call.nameExact("notAPound").l
      notAPoundCall.code shouldBe "#notAPound"
    }

    "testObjectLiterals3b" in {
      val cpg            = code("let _ = #notAPound(1, 2)")
      val List(callNode) = cpg.call.nameExact("notAPound").l
      callNode.code shouldBe "#notAPound(1, 2)"
      callNode.ast.isLiteral.code.l shouldBe List("1", "2")
    }

    "testObjectLiterals3c" in {
      val cpg             = code("let _ = #Color")
      val List(colorCall) = cpg.call.nameExact("Color").l
      colorCall.code shouldBe "#Color"
    }

    "testObjectLiterals5" in {
      val cpg             = code(" let _ = [#Color(_: 1, green: 1, 2)]")
      val List(arrayInit) = cpg.call.nameExact("<operator>.arrayInitializer").l
      arrayInit.code shouldBe "[#Color(_: 1, green: 1, 2)]"
      val List(colorCall) = cpg.call.nameExact("Color").l
      colorCall.code shouldBe "#Color(_: 1, green: 1, 2)"
      colorCall.ast.isLiteral.code.l shouldBe List("1", "1", "2")
    }

    "testObjectLiterals8" in {
      val cpg             = code(" let _ = #Color(_: 1, green: 1)")
      val List(colorCall) = cpg.call.nameExact("Color").l
      colorCall.code shouldBe "#Color(_: 1, green: 1)"
      colorCall.ast.isLiteral.code.l shouldBe List("1", "1")
    }

  }

}
