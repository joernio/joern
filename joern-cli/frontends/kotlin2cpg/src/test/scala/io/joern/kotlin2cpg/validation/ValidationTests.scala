package io.joern.kotlin2cpg.validation

import io.joern.kotlin2cpg.Kt2CpgTestContext
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.edges.Ast
import io.shiftleft.codepropertygraph.generated.nodes.{FieldIdentifier, Identifier}
import io.shiftleft.proto.cpg.Cpg.DispatchTypes
import io.shiftleft.semanticcpg.language._
import overflowdb.traversal.jIteratortoTraversal

class ValidationTests extends AnyFreeSpec with Matchers {
  "CPG for code with stray identifier" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
          |package com.bugsnag.android
          |
          |import com.bugsnag.android.internal.ImmutableConfig
          |
          |internal class ClientObservable : BaseObservable() {
          |    fun postOrientationChange(orientation: String?) {
          |        updateState { StateEvent.UpdateOrientation(orientation) }
          |    }
          |
          |    fun postNdkInstall(
          |        conf: ImmutableConfig,
          |        lastRunInfoPath: String,
          |        consecutiveLaunchCrashes: Int
          |    ) {
          |        updateState {
          |            StateEvent.Install(
          |                conf.apiKey,
          |                conf.enabledErrorTypes.ndkCrashes,
          |                conf.appVersion,
          |                conf.buildUuid,
          |                conf.releaseStage,
          |                lastRunInfoPath,
          |                consecutiveLaunchCrashes,
          |                conf.sendThreads
          |            )
          |        }
          |    }
          |
          |    fun postNdkDeliverPending() {
          |        updateState { StateEvent.DeliverPending }
          |    }
          |}
          |""".stripMargin)

    "should not contain any IDENTIFIER nodes without inbound AST edges" in {
      cpg.identifier
        .filter(_._astIn.size == 0)
        .code
        .l shouldBe Seq()
    }
  }

}
