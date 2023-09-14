package io.joern.jssrc2cpg.passes

import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.Languages
import io.shiftleft.semanticcpg.language.*
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.BeforeAndAfterEach
import org.scalatest.Inside

import scala.jdk.CollectionConverters.*

class JsMetaDataPassTests extends AnyWordSpec with Matchers with Inside {

  "MetaDataPass" should {
    val cpg = Cpg.empty
    new JavaScriptMetaDataPass(cpg, "somehash", "").createAndApply()

    "create exactly 1 node" in {
      cpg.graph.allNodes.size shouldBe 1
    }

    "create no edges" in {
      cpg.graph.allNodes.outE.size shouldBe 0
    }

    "create a metadata node with correct language" in {
      cpg.metaData.language.l shouldBe List(Languages.JSSRC)
    }

    "create a metadata node with correct hash" in {
      cpg.metaData.hash.l shouldBe List("somehash")
    }
  }

}
