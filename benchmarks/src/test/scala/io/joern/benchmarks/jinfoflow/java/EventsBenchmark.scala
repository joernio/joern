package io.joern.benchmarks.jinfoflow.java

import io.joern.benchmarks.BenchmarkTags.Events
import io.joern.benchmarks.testfixtures.BenchmarkFixture._
import io.joern.benchmarks.testfixtures.JInfoFlowEventsFixture
import io.shiftleft.semanticcpg.language._

class EventsBenchmark1 extends JInfoFlowEventsFixture(JAVA_EXT, 1) {

  s"Events$benchmarkNo line 36" should "report insecure" taggedAs Events in {
    assertIsInsecure(cpg.call(".*readLine.*"), cpg.call(".*println.*").lineNumber(36))
  }

  s"Events$benchmarkNo line 41" should "report secure" taggedAs Events in {
    assertIsSecure(cpg.call(".*readLine.*"), cpg.call(".*println.*").lineNumber(41))
  }

}

class EventsBenchmark2 extends JInfoFlowEventsFixture(JAVA_EXT, 2) {

  s"Events$benchmarkNo" should "report insecure" taggedAs Events in {
    assertIsInsecure(cpg.call(".*readLine.*").receiver, cpg.call(".*println.*"))
  }

}

class EventsBenchmark3 extends JInfoFlowEventsFixture(JAVA_EXT, 3) {

  s"Events$benchmarkNo line 41" should "report insecure" taggedAs Events in {
    assertIsInsecure(cpg.call(".*readLine.*").receiver, cpg.call(".*println.*").lineNumber(41))
  }

  s"Events$benchmarkNo line 42" should "report secure" taggedAs Events in {
    assertIsSecure(cpg.call(".*readLine.*").receiver, cpg.call(".*println.*").lineNumber(42))
  }

}

class EventsBenchmark4 extends JInfoFlowEventsFixture(JAVA_EXT, 4) {

  s"Events$benchmarkNo" should "report insecure" taggedAs Events in {
    assertIsInsecure(cpg.call(".*readLine.*").receiver, cpg.call(".*println.*"))
  }

}

class EventsBenchmark5 extends JInfoFlowEventsFixture(JAVA_EXT, 5) {

  s"Events$benchmarkNo line 27" should "report insecure" taggedAs Events in {
    assertIsInsecure(cpg.call(".*readLine.*").receiver, cpg.call(".*println.*").lineNumber(27))
  }

  s"Events$benchmarkNo line 51" should "report secure" taggedAs Events in {
    assertIsSecure(cpg.call(".*readLine.*").receiver, cpg.call(".*println.*").lineNumber(51))
  }

}
