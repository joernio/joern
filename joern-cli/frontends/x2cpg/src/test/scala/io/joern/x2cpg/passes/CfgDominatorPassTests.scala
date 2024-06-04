package io.joern.x2cpg.passes

import io.shiftleft.OverflowDbTestInstance
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, NodeTypes}
import io.joern.x2cpg.passes.controlflow.cfgdominator.CfgDominatorPass
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import overflowdb._

import scala.jdk.CollectionConverters._

class CfgDominatorPassTests extends AnyWordSpec with Matchers {
  "Have correct DOMINATE/POST_DOMINATE edges after CfgDominatorPass run." in {
    val graph = OverflowDbTestInstance.create
    val cpg   = new Cpg(graph)

    val v0 = graph + NodeTypes.METHOD
    val v1 = graph + NodeTypes.UNKNOWN
    val v2 = graph + NodeTypes.UNKNOWN
    val v3 = graph + NodeTypes.UNKNOWN
    val v4 = graph + NodeTypes.UNKNOWN
    val v5 = graph + NodeTypes.UNKNOWN
    val v6 = graph + NodeTypes.METHOD_RETURN

    v0 --- EdgeTypes.AST --> v6

    v0 --- EdgeTypes.CFG --> v1
    v1 --- EdgeTypes.CFG --> v2
    v2 --- EdgeTypes.CFG --> v3
    v2 --- EdgeTypes.CFG --> v5
    v3 --- EdgeTypes.CFG --> v4
    v4 --- EdgeTypes.CFG --> v2
    v4 --- EdgeTypes.CFG --> v5
    v5 --- EdgeTypes.CFG --> v6

    val dominatorTreePass = new CfgDominatorPass(cpg)
    dominatorTreePass.createAndApply()

    val v0Dominates = v0.out(EdgeTypes.DOMINATE).asScala.toList
    v0Dominates.size shouldBe 1
    v0Dominates.toSet shouldBe Set(v1)
    val v1Dominates = v1.out(EdgeTypes.DOMINATE).asScala.toList
    v1Dominates.size shouldBe 1
    v1Dominates.toSet shouldBe Set(v2)
    val v2Dominates = v2.out(EdgeTypes.DOMINATE).asScala.toList
    v2Dominates.size shouldBe 2
    v2Dominates.toSet shouldBe Set(v3, v5)
    val v3Dominates = v3.out(EdgeTypes.DOMINATE).asScala.toList
    v3Dominates.size shouldBe 1
    v3Dominates.toSet shouldBe Set(v4)
    val v4Dominates = v4.out(EdgeTypes.DOMINATE).asScala.toList
    v4Dominates.size shouldBe 0
    val v5Dominates = v5.out(EdgeTypes.DOMINATE).asScala.toList
    v5Dominates.size shouldBe 1
    v5Dominates.toSet shouldBe Set(v6)
    val v6Dominates = v6.out(EdgeTypes.DOMINATE).asScala.toList
    v6Dominates.size shouldBe 0

    val v6PostDominates = v6.out(EdgeTypes.POST_DOMINATE).asScala.toList
    v6PostDominates.size shouldBe 1
    v6PostDominates.toSet shouldBe Set(v5)
    val v5PostDominates = v5.out(EdgeTypes.POST_DOMINATE).asScala.toList
    v5PostDominates.size shouldBe 2
    v5PostDominates.toSet shouldBe Set(v2, v4)
    val v4PostDominates = v4.out(EdgeTypes.POST_DOMINATE).asScala.toList
    v4PostDominates.size shouldBe 1
    v4PostDominates.toSet shouldBe Set(v3)
    val v3PostDominates = v3.out(EdgeTypes.POST_DOMINATE).asScala.toList
    v3PostDominates.size shouldBe 0
    val v2PostDominates = v2.out(EdgeTypes.POST_DOMINATE).asScala.toList
    v2PostDominates.size shouldBe 1
    v2PostDominates.toSet shouldBe Set(v1)
    val v1PostDominates = v1.out(EdgeTypes.POST_DOMINATE).asScala.toList
    v1PostDominates.size shouldBe 1
    v1PostDominates.toSet shouldBe Set(v0)
    val v0PostDominates = v0.out(EdgeTypes.POST_DOMINATE).asScala.toList
    v0PostDominates.size shouldBe 0
  }
}
