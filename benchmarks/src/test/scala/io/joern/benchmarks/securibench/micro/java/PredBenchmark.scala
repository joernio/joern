package io.joern.benchmarks.securibench.micro.java

import io.joern.benchmarks.BenchmarkTags.Pred
import io.joern.benchmarks.testfixtures.BenchmarkFixture._
import io.joern.benchmarks.testfixtures.SecuribenchMicroPredFixture
import io.shiftleft.semanticcpg.language._

class PredBenchmark1 extends SecuribenchMicroPredFixture(JAVA_EXT, 1) {

  s"Pred$benchmarkNo" should "report secure" taggedAs Pred in {
    assertIsSecure(cpg.fieldAccess.code(".*FIELD_NAME.*"), cpg.call(".*println.*"))
  }

}

class PredBenchmark2 extends SecuribenchMicroPredFixture(JAVA_EXT, 2) {

  s"Pred$benchmarkNo" should "report insecure" taggedAs Pred in {
    assertIsInsecure(cpg.fieldAccess.code(".*FIELD_NAME.*"), cpg.call(".*println.*"))
  }

}

class PredBenchmark3 extends SecuribenchMicroPredFixture(JAVA_EXT, 3) {

  s"Pred$benchmarkNo" should "report secure" taggedAs Pred in {
    assertIsSecure(cpg.fieldAccess.code(".*FIELD_NAME.*"), cpg.call(".*println.*"))
  }

}

class PredBenchmark4 extends SecuribenchMicroPredFixture(JAVA_EXT, 4) {

  s"Pred$benchmarkNo" should "report insecure" taggedAs Pred in {
    assertIsInsecure(cpg.fieldAccess.code(".*FIELD_NAME.*"), cpg.call(".*println.*"))
  }

}

class PredBenchmark5 extends SecuribenchMicroPredFixture(JAVA_EXT, 5) {

  s"Pred$benchmarkNo" should "report insecure" taggedAs Pred in {
    assertIsInsecure(cpg.fieldAccess.code(".*FIELD_NAME.*"), cpg.call(".*println.*"))
  }

}

class PredBenchmark6 extends SecuribenchMicroPredFixture(JAVA_EXT, 6) {

  s"Pred$benchmarkNo" should "report secure" taggedAs Pred in {
    assertIsSecure(cpg.fieldAccess.code(".*FIELD_NAME.*"), cpg.call(".*println.*"))
  }

}

class PredBenchmark7 extends SecuribenchMicroPredFixture(JAVA_EXT, 7) {

  s"Pred$benchmarkNo" should "report secure" taggedAs Pred in {
    assertIsSecure(cpg.fieldAccess.code(".*FIELD_NAME.*"), cpg.call(".*println.*"))
  }

}

class PredBenchmark8 extends SecuribenchMicroPredFixture(JAVA_EXT, 8) {

  s"Pred$benchmarkNo" should "report insecure" taggedAs Pred in {
    assertIsInsecure(cpg.fieldAccess.code(".*FIELD_NAME.*"), cpg.call(".*println.*"))
  }

}

class PredBenchmark9 extends SecuribenchMicroPredFixture(JAVA_EXT, 9) {

  s"Pred$benchmarkNo" should "report insecure" taggedAs Pred in {
    assertIsInsecure(cpg.fieldAccess.code(".*FIELD_NAME.*"), cpg.call(".*println.*"))
  }

}
