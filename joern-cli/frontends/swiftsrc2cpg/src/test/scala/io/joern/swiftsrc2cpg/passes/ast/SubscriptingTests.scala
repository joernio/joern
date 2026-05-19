// This test file has been translated from swift/test/Parse/subscripting.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.joern.swiftsrc2cpg.testfixtures.SwiftSrc2CpgSuite
import io.shiftleft.semanticcpg.language.*

class SubscriptingTests extends SwiftSrc2CpgSuite {

  "SubscriptingTests" should {

    "testSubscripting2" in {
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
      val List(structDecl) = cpg.typeDecl.nameExact("X1").l
      structDecl.fullName shouldBe "Test0.swift:<global>.X1"
      val List(stored) = structDecl.member.nameExact("stored").l
      stored.typeFullName shouldBe "Swift.Int"
      cpg.method.fullName.l should contain allOf (
        "Test0.swift:<global>.X1.subscript:(i:Swift.Int).getter:Swift.Int",
        "Test0.swift:<global>.X1.subscript:(i:Swift.Int).setter:Swift.Int"
      )
    }

    "testSubscripting3" in {
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
      val List(structDecl) = cpg.typeDecl.nameExact("X2").l
      structDecl.fullName shouldBe "Test0.swift:<global>.X2"
      cpg.method.fullName.l should contain allOf (
        "Test0.swift:<global>.X2.subscript:(i:Swift.Int).getter:Swift.Int",
        "Test0.swift:<global>.X2.subscript:(i:Swift.Int).setter:Swift.Int"
      )
    }

    "testSubscripting10" in {
      val cpg = code("""
        |struct Y2 {
        |  subscript(_: Int) -> Int {
        |    mutating get { return 0 }
        |  }
        |}""".stripMargin)
      val List(structDecl) = cpg.typeDecl.nameExact("Y2").l
      structDecl.fullName shouldBe "Test0.swift:<global>.Y2"
      // Read-only subscript: only a getter is emitted, no setter.
      cpg.method.fullName.l should contain("Test0.swift:<global>.Y2.subscript:(_:Swift.Int).getter:Swift.Int")
      cpg.method.fullName.filter(_.contains("Y2.subscript")).filter(_.endsWith(".setter:Swift.Int")).l shouldBe empty
    }

    "testSubscripting17" in {
      val cpg = code("""
        |struct A5 {
        |  subscript(i : Int) -> Int
        |}""".stripMargin)
      // Subscript declaration with no body — no getter/setter is materialized; instead
      // a method named `subscript` carries the declared signature.
      val List(structDecl) = cpg.typeDecl.nameExact("A5").l
      structDecl.fullName shouldBe "Test0.swift:<global>.A5"
      val List(subscriptMethod) = cpg.method.nameExact("subscript").l
      subscriptMethod.fullName shouldBe "Test0.swift:<global>.A5.subscript:(i:Swift.Int)->Swift.Int"
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
