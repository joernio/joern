// This test file has been translated from swift/test/Parse/subscripting.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.joern.swiftsrc2cpg.testfixtures.AstSwiftSrc2CpgSuite

class SubscriptingTests extends AstSwiftSrc2CpgSuite {

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

    "testSubscripting19" ignore {
      val cpg = code("""
        |struct A7 {
        |  class subscript(a: Float) -> Int {
        |    get {
        |      return 42
        |    }
        |  }
        |}""".stripMargin)
      ???
    }

    "testSubscripting20" ignore {
      val cpg = code("""
        |class A7b {
        |  class static subscript(a: Float) -> Int {
        |    get {
        |      return 42
        |    }
        |  }
        |}
        |""".stripMargin)
      ???
    }

  }

}
