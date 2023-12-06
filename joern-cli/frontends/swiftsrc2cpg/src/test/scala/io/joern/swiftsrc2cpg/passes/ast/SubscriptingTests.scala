// This test file has been translated from swift/test/Parse/subscripting.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.shiftleft.codepropertygraph.generated._
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._

class SubscriptingTests extends AbstractPassTest {

  "SubscriptingTests" should {

    "testSubscripting2" ignore AstFixture("""
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
        |""".stripMargin) { cpg => ??? }

    "testSubscripting3" ignore AstFixture("""
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
        |}""".stripMargin) { cpg => ??? }

    "testSubscripting10" ignore AstFixture("""
        |struct Y2 {
        |  subscript(_: Int) -> Int {
        |    mutating get { return 0 }
        |  }
        |}""".stripMargin) { cpg => ??? }

    "testSubscripting17" ignore AstFixture("""
        |struct A5 {
        |  subscript(i : Int) -> Int
        |}""".stripMargin) { cpg => ??? }

    "testSubscripting19" ignore AstFixture("""
        |struct A7 {
        |  class subscript(a: Float) -> Int {
        |    get {
        |      return 42
        |    }
        |  }
        |}""".stripMargin) { cpg => ??? }

    "testSubscripting20" ignore AstFixture("""
        |class A7b {
        |  class static subscript(a: Float) -> Int {
        |    get {
        |      return 42
        |    }
        |  }
        |}
        |""".stripMargin) { cpg => ??? }

  }

}
