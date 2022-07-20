package io.joern.benchmarks.jinfoflow.jvm

import io.joern.benchmarks.BenchmarkTags.Basic
import io.joern.benchmarks.testfixtures.BenchmarkFixture._
import io.joern.benchmarks.testfixtures.JInfoFlowBasicFixture
import io.shiftleft.semanticcpg.language._

// Arrays
class BasicBenchmark1 extends JInfoFlowBasicFixture(JVM_EXT, 1) {

  s"Basic$benchmarkNo line 17" should "report insecure" taggedAs Basic in {
    assertIsInsecure(cpg.call(".*readLine.*"), cpg.call(".*println.*").lineNumber(17))
  }

  s"Basic$benchmarkNo line 19" should "report secure" taggedAs Basic in {
    assertIsSecure(cpg.call(".*readLine.*"), cpg.call(".*println.*").lineNumber(19))
  }

}

// Substrings
class BasicBenchmark2 extends JInfoFlowBasicFixture(JVM_EXT, 2) {

  s"Basic$benchmarkNo line 14" should "report secure" taggedAs Basic in {
    assertIsSecure(cpg.call(".*readLine.*"), cpg.call(".*println.*").lineNumber(14))
  }

  s"Basic$benchmarkNo line 15" should "report insecure" taggedAs Basic in {
    assertIsInsecure(cpg.call(".*readLine.*"), cpg.call(".*println.*").lineNumber(15))
  }

}
