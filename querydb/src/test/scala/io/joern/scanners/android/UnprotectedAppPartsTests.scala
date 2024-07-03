package io.joern.scanners.android

import io.joern.console.scan.*
import io.shiftleft.codepropertygraph.generated.nodes.CfgNode
import io.joern.suites.KotlinQueryTestSuite
import io.shiftleft.semanticcpg.language.*

class UnprotectedAppPartsTests extends KotlinQueryTestSuite(UnprotectedAppParts) {

  "should match all positive examples" in {
    val query = queryBundle.intentRedirection()
    query(cpg).flatMap(_.evidence).collect { case cfgNode: CfgNode => (cfgNode.method.name, cfgNode.code) }.l shouldBe
      List(("matchingExample1", "intent.getParcelableExtra<Intent>(\"very_forward_of_you\")"))
  }
}
