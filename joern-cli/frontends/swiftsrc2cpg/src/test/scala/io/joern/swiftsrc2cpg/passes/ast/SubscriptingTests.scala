// This test file has been translated from swift/test/Parse/subscripting.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.joern.swiftsrc2cpg.testfixtures.SwiftSrc2CpgSuite
import io.shiftleft.semanticcpg.language.*

class SubscriptingTests extends SwiftSrc2CpgSuite {

  "SubscriptingTests" should {

    "testSubscripting2" ignore {
      val cpg = code("""
        |struct X1 {
        |  var stored: Int
        |  subscript(i: Int) -> Int {
        |    get {
        |      return stored
        |    }
        |    mutating
        |    set {
        |      stored = newValue
        |    }
        |  }
        |}
        |""".stripMargin)
      ???
    }

    "testSubscripting3" ignore {
      val cpg = code("""
        |struct X2 {
        |  var stored: Int
        |  subscript(i: Int) -> Int {
        |    get {
        |      return stored + i
        |    }
        |    set(v) {
        |      stored = v - i
        |    }
        |  }
        |}""".stripMargin)
      ???
    }

    "testSubscripting10" ignore {
      val cpg = code("""
        |struct Y2 {
        |  subscript(_: Int) -> Int {
        |    mutating get { return 0 }
        |  }
        |}""".stripMargin)
      ???
    }

    "testSubscripting17" ignore {
      val cpg = code("""
        |struct A5 {
        |  subscript(i : Int) -> Int
        |}""".stripMargin)
      ???
    }

    "testSubscripting19" in {
      // Invalid: `class` cannot be used as a modifier on subscript inside a struct.
      // The struct still produces a TypeDecl; assert that.
      val cpg = code("""
        |struct A7 {
        |  class subscript(a: Float) -> Int {
        |    get {
        |      return 42
        |    }
        |  }
        |}""".stripMargin)
      val List(structDecl) = cpg.typeDecl.nameExact("A7").l
      structDecl.fullName shouldBe "Test0.swift:<global>.A7"
    }

    "testSubscripting20" in {
      // Invalid modifier ordering (`class static subscript`); class TypeDecl is still produced.
      val cpg = code("""
        |class A7b {
        |  class static subscript(a: Float) -> Int {
        |    get {
        |      return 42
        |    }
        |  }
        |}
        |""".stripMargin)
      val List(classDecl) = cpg.typeDecl.nameExact("A7b").l
      classDecl.fullName shouldBe "Test0.swift:<global>.A7b"
    }

  }

}
