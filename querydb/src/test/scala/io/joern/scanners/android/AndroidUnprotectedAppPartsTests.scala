package io.joern.scanners.android

import io.joern.console.scan._
import io.shiftleft.codepropertygraph.generated.nodes.CfgNode
import io.joern.suites.KotlinQueryTestSuite
import overflowdb.traversal.iterableToTraversal
import io.shiftleft.semanticcpg.language._

class AndroidUnprotectedAppPartsTests extends KotlinQueryTestSuite {
  override def queryBundle = AndroidUnprotectedAppParts

  "should match all positive examples" in {
    val query = queryBundle.intentRedirection()
    query(cpg).flatMap(_.evidence).collect { case cfgNode: CfgNode => (cfgNode.method.name, cfgNode.code) }.l shouldBe
      List(("matchingExample1", "intent.getParcelableExtra<Intent>(\"very_forward_of_you\")"))
  }
}
