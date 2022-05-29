package io.joern.benchmarks.jinfoflow.java

import io.joern.benchmarks.BenchmarkTags.Context
import io.joern.benchmarks.testfixtures.BenchmarkFixture._
import io.joern.benchmarks.testfixtures.JInfoFlowCtxFixture
import io.shiftleft.semanticcpg.language._

// InstanceAndStaticMethods
class CtxBenchmark1 extends JInfoFlowCtxFixture(JAVA_EXT, 1) {

  s"Ctx$benchmarkNo line 47" should "report insecure" taggedAs Context in {
    assertIsInsecure(cpg.call(".*readLine.*"), cpg.call(".*println.*").lineNumber(47))
  }

  s"Ctx$benchmarkNo line 48" should "report secure" taggedAs Context in {
    assertIsSecure(cpg.call(".*readLine.*"), cpg.call(".*println.*").lineNumber(48))
  }

}

// InstanceMethods
class CtxBenchmark2 extends JInfoFlowCtxFixture(JAVA_EXT, 2) {

  s"Ctx$benchmarkNo" should "report insecure" taggedAs Context in {
    assertIsInsecure(cpg.call(".*readLine.*").receiver, cpg.call(".*println.*"))
  }

}

// OverwriteSinks
class CtxBenchmark3 extends JInfoFlowCtxFixture(JAVA_EXT, 3) {

  s"Ctx$benchmarkNo line 35" should "report secure" taggedAs Context in {
    assertIsSecure(cpg.call(".*readLine.*").receiver, cpg.call(".*println.*").lineNumber(35))
  }

  s"Ctx$benchmarkNo line 37" should "report insecure" taggedAs Context in {
    assertIsInsecure(cpg.call(".*readLine.*").receiver, cpg.call(".*println.*").lineNumber(37))
  }

}

// OverwriteSources
class CtxBenchmark4 extends JInfoFlowCtxFixture(JAVA_EXT, 4) {

  s"Ctx$benchmarkNo line 34" should "report secure" taggedAs Context in {
    assertIsSecure(cpg.call(".*readLine.*").receiver, cpg.call(".*println.*").lineNumber(34))
  }

  s"Ctx$benchmarkNo line 36" should "report insecure" taggedAs Context in {
    assertIsInsecure(cpg.call(".*readLine.*").receiver, cpg.call(".*println.*").lineNumber(36))
  }

}

// StaticMethods
class CtxBenchmark5 extends JInfoFlowCtxFixture(JAVA_EXT, 5) {

  s"Ctx$benchmarkNo" should "report insecure" taggedAs Context in {
    assertIsInsecure(cpg.call(".*readLine.*").receiver, cpg.call(".*println.*"))
  }

}
