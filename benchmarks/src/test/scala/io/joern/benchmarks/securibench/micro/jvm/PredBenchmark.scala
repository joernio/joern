package io.joern.benchmarks.securibench.micro.jvm

import io.joern.benchmarks.BenchmarkTags.Pred
import io.joern.benchmarks.testfixtures.BenchmarkFixture._
import io.joern.benchmarks.testfixtures.SecuribenchMicroPredFixture
import io.shiftleft.semanticcpg.language._

class PredBenchmark1 extends SecuribenchMicroPredFixture(JVM_EXT, 1) {

  s"Pred$benchmarkNo" should "report secure" taggedAs Pred in {
    assertIsSecure(cpg.literal(".*name.*"), cpg.call(".*println.*"))
  }

}

class PredBenchmark2 extends SecuribenchMicroPredFixture(JVM_EXT, 2) {

  s"Pred$benchmarkNo" should "report insecure" taggedAs Pred in {
    assertIsInsecure(cpg.literal(".*name.*"), cpg.call(".*println.*"))
  }

}

class PredBenchmark3 extends SecuribenchMicroPredFixture(JVM_EXT, 3) {

  s"Pred$benchmarkNo" should "report secure" taggedAs Pred in {
    assertIsSecure(cpg.literal(".*name.*"), cpg.call(".*println.*"))
  }

}

class PredBenchmark4 extends SecuribenchMicroPredFixture(JVM_EXT, 4) {

  s"Pred$benchmarkNo" should "report insecure" taggedAs Pred in {
    assertIsInsecure(cpg.literal(".*name.*"), cpg.call(".*println.*"))
  }

}

class PredBenchmark5 extends SecuribenchMicroPredFixture(JVM_EXT, 5) {

  s"Pred$benchmarkNo" should "report insecure" taggedAs Pred in {
    assertIsInsecure(cpg.literal(".*name.*"), cpg.call(".*println.*"))
  }

}

class PredBenchmark6 extends SecuribenchMicroPredFixture(JVM_EXT, 6) {

  s"Pred$benchmarkNo" should "report secure" taggedAs Pred in {
    assertIsSecure(cpg.literal(".*name.*"), cpg.call(".*println.*"))
  }

}

class PredBenchmark7 extends SecuribenchMicroPredFixture(JVM_EXT, 7) {

  s"Pred$benchmarkNo" should "report secure" taggedAs Pred in {
    assertIsSecure(cpg.literal(".*name.*"), cpg.call(".*println.*"))
  }

}

class PredBenchmark8 extends SecuribenchMicroPredFixture(JVM_EXT, 8) {

  s"Pred$benchmarkNo" should "report insecure" taggedAs Pred in {
    assertIsInsecure(cpg.literal(".*name.*"), cpg.call(".*println.*"))
  }

}

class PredBenchmark9 extends SecuribenchMicroPredFixture(JVM_EXT, 9) {

  s"Pred$benchmarkNo" should "report insecure" taggedAs Pred in {
    assertIsInsecure(cpg.literal(".*name.*"), cpg.call(".*println.*"))
  }

}
