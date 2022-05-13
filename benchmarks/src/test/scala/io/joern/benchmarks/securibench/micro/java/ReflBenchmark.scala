package io.joern.benchmarks.securibench.micro.java

import io.joern.benchmarks.BenchmarkTags.Refl
import io.joern.benchmarks.testfixtures.BenchmarkFixture._
import io.joern.benchmarks.testfixtures.SecuribenchMicroReflectionFixture
import io.shiftleft.semanticcpg.language._

class ReflBenchmark1 extends SecuribenchMicroReflectionFixture(JAVA_EXT, 1) {

  s"Refl$benchmarkNo" should "report secure" taggedAs Refl in {
    assertIsSecure(cpg.fieldAccess.code(".*FIELD_NAME.*"), cpg.call(".*println.*"))
  }

}

class ReflBenchmark2 extends SecuribenchMicroReflectionFixture(JAVA_EXT, 2) {

  s"Refl$benchmarkNo" should "report insecure" taggedAs Refl in {
    assertIsInsecure(cpg.fieldAccess.code(".*FIELD_NAME.*"), cpg.call(".*println.*"))
  }

}

class ReflBenchmark3 extends SecuribenchMicroReflectionFixture(JAVA_EXT, 3) {

  s"Refl$benchmarkNo" should "report insecure" taggedAs Refl in {
    assertIsInsecure(cpg.fieldAccess.code(".*FIELD_NAME.*"), cpg.call(".*println.*"))
  }

}

class ReflBenchmark4 extends SecuribenchMicroReflectionFixture(JAVA_EXT, 4) {

  s"Refl$benchmarkNo" should "report insecure" taggedAs Refl in {
    assertIsInsecure(cpg.fieldAccess.code(".*FIELD_NAME.*"), cpg.call(".*println.*"))
  }

}
