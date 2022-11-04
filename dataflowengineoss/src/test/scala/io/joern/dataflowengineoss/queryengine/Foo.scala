package io.joern.dataflowengineoss.queryengine

import io.shiftleft.codepropertygraph.generated.Cpg
import org.scalatest.wordspec.AnyWordSpec
import io.shiftleft.semanticcpg.language._
import io.joern.dataflowengineoss.language._
import org.scalatest.matchers.should.Matchers

class Foo extends AnyWordSpec with Matchers {

  implicit val context: EngineContext = EngineContext()
  implicit val engine: Engine         = new Engine(context)

  "opencrx" should {
    val cpg = Cpg.withStorage("/Users/david/Documents/Privado-Inc/opencrx.bin")

    "load all good" in {
      cpg.graph.nodeCount() should be > 0
      cpg.graph.edgeCount() should be > 0
    }

    "get stuck" in {
      cpg.method("executeUpdate").methodReturn.reachableByFlows(cpg.literal).l
    }
  }

}
