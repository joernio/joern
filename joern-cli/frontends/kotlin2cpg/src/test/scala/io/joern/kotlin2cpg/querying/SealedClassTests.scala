package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.TestContext
import io.shiftleft.semanticcpg.language._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import overflowdb.traversal.jIteratortoTraversal

class SealedClassTests extends AnyFreeSpec with Matchers {

  "CPG for code with sealed class and annotated member" - {
    lazy val cpg = TestContext.buildCpg("""
        |package mypkg
        |
        |sealed class StateEvent {
        |    class Install(
        |        @JvmField val consecutiveLaunchCrashes: Int,
        |    ) : StateEvent()
        |    class UpdateLastRunInfo(@JvmField val consecutiveLaunchCrashes: Int) : StateEvent()
        |}
        |""".stripMargin)

    "should not contain any identifiers without AST parents" in {
      cpg.identifier
        .filter(_._astIn.size == 0)
        .code
        .l shouldBe Seq()
    }
  }
}
