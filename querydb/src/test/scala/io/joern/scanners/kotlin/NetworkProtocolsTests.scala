package io.joern.scanners.kotlin

import io.joern.console.scan.*
import io.joern.suites.KotlinQueryTestSuite
import io.shiftleft.codepropertygraph.generated.nodes.Call
import io.shiftleft.semanticcpg.language.*
class NetworkProtocolsTests extends KotlinQueryTestSuite(NetworkProtocols) {

  "should find calls relevant to insecure network protocol usage" in {
    val query = queryBundle.usageOfInsecureProtocol()
    query(cpg).flatMap(_.evidence).collect { case c: Call => c.code }.l shouldBe List("URL(\"http://phrack.org\")")
  }
}
