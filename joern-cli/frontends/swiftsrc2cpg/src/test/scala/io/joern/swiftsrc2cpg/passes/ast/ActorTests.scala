// This test file has been translated from swift/test/Parse/actor.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.joern.swiftsrc2cpg.testfixtures.AstSwiftSrc2CpgSuite

class ActorTests extends AstSwiftSrc2CpgSuite {

  "ActorTests" should {

    "testActor1" ignore {
      val cpg = code("actor MyActor1 {}")
      ???
    }

    "testActor2" ignore {
      val cpg = code("""
        |actor MyActor2 {
        |  init() {}
        |  func hello() {}
        |}""".stripMargin)
      ???
    }
  }

}
