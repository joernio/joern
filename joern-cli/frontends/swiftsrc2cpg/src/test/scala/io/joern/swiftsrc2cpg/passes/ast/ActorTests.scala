// This test file has been translated from swift/test/Parse/actor.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.shiftleft.codepropertygraph.generated._
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._

class ActorTests extends AbstractPassTest {

  "ActorTests" should {

    "testActor1" ignore AstFixture("actor MyActor1 {}") { cpg => ??? }

    "testActor2" ignore AstFixture("""
        |actor MyActor2 {
        |  init() {}
        |  func hello() {}
        |}""".stripMargin) { cpg => ??? }
  }

}
