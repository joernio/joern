package io.joern.benchmarks.securibench.micro.jvm

import io.joern.benchmarks.BenchmarkTags.Refl
import io.joern.benchmarks.testfixtures.BenchmarkFixture._
import io.joern.benchmarks.testfixtures.SecuribenchMicroReflectionFixture
import io.shiftleft.semanticcpg.language._

class ReflBenchmark1 extends SecuribenchMicroReflectionFixture(JVM_EXT, 1) {

  s"Refl$benchmarkNo" should "report secure" taggedAs Refl in {
    assertIsSecure(cpg.literal(".*name.*"), cpg.call(".*println.*"))
  }

}

class ReflBenchmark2 extends SecuribenchMicroReflectionFixture(JVM_EXT, 2) {

  s"Refl$benchmarkNo" should "report insecure" taggedAs Refl in {
    assertIsInsecure(cpg.literal(".*name.*"), cpg.call(".*println.*"))
  }

}

class ReflBenchmark3 extends SecuribenchMicroReflectionFixture(JVM_EXT, 3) {

  s"Refl$benchmarkNo" should "report insecure" taggedAs Refl in {
    assertIsInsecure(cpg.literal(".*name.*"), cpg.call(".*println.*"))
  }

}

class ReflBenchmark4 extends SecuribenchMicroReflectionFixture(JVM_EXT, 4) {

  s"Refl$benchmarkNo" should "report insecure" taggedAs Refl in {
    assertIsInsecure(cpg.literal(".*name.*"), cpg.call(".*println.*"))
  }

}
