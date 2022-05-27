package io.joern.benchmarks.securibench.micro.jvm

import io.joern.benchmarks.BenchmarkTags.StrongUpdates
import io.joern.benchmarks.testfixtures.BenchmarkFixture._
import io.joern.benchmarks.testfixtures.SecuribenchMicroStrongUpdatesFixture
import io.shiftleft.semanticcpg.language._

class StrongUpdatesBenchmark1 extends SecuribenchMicroStrongUpdatesFixture(JAVA_EXT, 1) {

  s"StrongUpdates$benchmarkNo" should "report secure" taggedAs StrongUpdates in {
    assertIsSecure(cpg.literal(".*name.*"), cpg.call(".*println.*"))
  }

}

class StrongUpdatesBenchmark2 extends SecuribenchMicroStrongUpdatesFixture(JAVA_EXT, 2) {

  s"StrongUpdates$benchmarkNo" should "report secure" taggedAs StrongUpdates in {
    assertIsSecure(cpg.literal(".*name.*"), cpg.call(".*println.*"))
  }

}

class StrongUpdatesBenchmark3 extends SecuribenchMicroStrongUpdatesFixture(JAVA_EXT, 3) {

  s"StrongUpdates$benchmarkNo" should "report secure" taggedAs StrongUpdates in {
    assertIsSecure(cpg.literal(".*name.*"), cpg.call(".*println.*"))
  }

}

class StrongUpdatesBenchmark4 extends SecuribenchMicroStrongUpdatesFixture(JAVA_EXT, 4) {

  s"StrongUpdates$benchmarkNo" should "report insecure" taggedAs StrongUpdates in {
    assertIsInsecure(cpg.literal(".*name.*"), cpg.call(".*println.*"))
  }

}

class StrongUpdatesBenchmark5 extends SecuribenchMicroStrongUpdatesFixture(JAVA_EXT, 3) {

  s"StrongUpdates$benchmarkNo" should "report secure" taggedAs StrongUpdates in {
    assertIsSecure(cpg.literal(".*name.*"), cpg.call(".*println.*"))
  }

}
