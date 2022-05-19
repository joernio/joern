package io.joern.benchmarks.securibench.micro.jvm

import io.joern.benchmarks.BenchmarkTags.Factories
import io.joern.benchmarks.testfixtures.BenchmarkFixture._
import io.joern.benchmarks.testfixtures.SecuribenchMicroFactoriesFixture
import io.shiftleft.semanticcpg.language._

class FactoriesBenchmark1 extends SecuribenchMicroFactoriesFixture(JVM_EXT, 1) {

  s"DataStructures$benchmarkNo line 43" should "report insecure" taggedAs Factories in {
    assertIsInsecure(cpg.literal(".*name.*"), cpg.call(".*println.*").lineNumber(43))
  }

  s"DataStructures$benchmarkNo line 44" should "report secure" taggedAs Factories in {
    assertIsSecure(cpg.literal(".*name.*"), cpg.call(".*println.*").lineNumber(44))
  }

}

class FactoriesBenchmark2 extends SecuribenchMicroFactoriesFixture(JVM_EXT, 2) {

  s"DataStructures$benchmarkNo line 43" should "report insecure" taggedAs Factories in {
    assertIsInsecure(cpg.literal(".*name.*"), cpg.call(".*println.*").lineNumber(43))
  }

  s"DataStructures$benchmarkNo line 44" should "report secure" taggedAs Factories in {
    assertIsSecure(cpg.literal(".*name.*"), cpg.call(".*println.*").lineNumber(44))
  }

}

class FactoriesBenchmark3 extends SecuribenchMicroFactoriesFixture(JVM_EXT, 3) {

  s"DataStructures$benchmarkNo line 55" should "report insecure" taggedAs Factories in {
    assertIsInsecure(cpg.literal(".*name.*"), cpg.call(".*println.*").lineNumber(55))
  }

  s"DataStructures$benchmarkNo line 56" should "report secure" taggedAs Factories in {
    assertIsSecure(cpg.literal(".*name.*"), cpg.call(".*println.*").lineNumber(56))
  }

}
