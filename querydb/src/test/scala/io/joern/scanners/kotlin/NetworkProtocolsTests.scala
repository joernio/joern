package io.joern.scanners.kotlin

import io.joern.console.scan._
import io.joern.suites.KotlinQueryTestSuite
import io.shiftleft.codepropertygraph.generated.nodes.Call

class NetworkProtocolsTests extends KotlinQueryTestSuite {

  override def queryBundle = NetworkProtocols

  "should find calls relevant to insecure network protocol usage" in {
    val query = queryBundle.usageOfInsecureProtocol()
    query(cpg).flatMap(_.evidence).collect { case c: Call => c }.size shouldBe 1
  }
}
