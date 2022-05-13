package io.joern.benchmarks.securibench.micro.jvm

import io.joern.benchmarks.BenchmarkTags.Session
import io.joern.benchmarks.testfixtures.BenchmarkFixture._
import io.joern.benchmarks.testfixtures.SecuribenchMicroSessionFixture
import io.shiftleft.semanticcpg.language._

class SessionBenchmark1 extends SecuribenchMicroSessionFixture(JVM_EXT, 1) {

  s"Session$benchmarkNo" should "report insecure" taggedAs Session in {
    assertIsInsecure(cpg.literal(".*name.*"), cpg.call(".*println.*"))
  }

}

class SessionBenchmark2 extends SecuribenchMicroSessionFixture(JVM_EXT, 2) {

  s"Session$benchmarkNo line 47" should "report insecure" taggedAs Session in {
    assertIsInsecure(cpg.literal(".*name.*"), cpg.call(".*println.*").lineNumber(47))
  }

  s"Session$benchmarkNo line 48" should "report secure" taggedAs Session in {
    assertIsSecure(cpg.literal(".*name.*"), cpg.call(".*println.*").lineNumber(48))
  }

}

class SessionBenchmark3 extends SecuribenchMicroSessionFixture(JVM_EXT, 3) {

  s"Session$benchmarkNo" should "report insecure" taggedAs Session in {
    assertIsInsecure(cpg.literal(".*name.*"), cpg.call(".*println.*"))
  }

}
