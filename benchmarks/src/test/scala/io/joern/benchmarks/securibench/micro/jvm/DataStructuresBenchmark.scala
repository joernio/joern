package io.joern.benchmarks.securibench.micro.jvm

import io.joern.benchmarks.BenchmarkTags.DataStructures
import io.joern.benchmarks.testfixtures.BenchmarkFixture._
import io.joern.benchmarks.testfixtures.SecuribenchMicroDatastructuresFixture
import io.shiftleft.semanticcpg.language._

class DataStructuresBenchmark1 extends SecuribenchMicroDatastructuresFixture(JVM_EXT, 1) {

  s"DataStructures$benchmarkNo line 57" should "report insecure" taggedAs DataStructures in {
    assertIsInsecure(cpg.literal(".*name.*"), cpg.call(".*println.*").lineNumber(57))
  }

  s"DataStructures$benchmarkNo line 58" should "report secure" taggedAs DataStructures in {
    assertIsSecure(cpg.literal(".*name.*"), cpg.call(".*println.*").lineNumber(58))
  }

}

class DataStructuresBenchmark2 extends SecuribenchMicroDatastructuresFixture(JVM_EXT, 2) {

  s"DataStructures$benchmarkNo line 59" should "report secure" taggedAs DataStructures in {
    assertIsSecure(cpg.literal(".*name.*"), cpg.call(".*println.*").lineNumber(59))
  }

  s"DataStructures$benchmarkNo line 60" should "report insecure" taggedAs DataStructures in {
    assertIsInsecure(cpg.literal(".*name.*"), cpg.call(".*println.*").lineNumber(60))
  }

}

class DataStructuresBenchmark3 extends SecuribenchMicroDatastructuresFixture(JVM_EXT, 3) {

  s"DataStructures$benchmarkNo" should "report insecure" taggedAs DataStructures in {
    assertIsInsecure(cpg.literal(".*name.*"), cpg.call(".*println.*"))
  }

}

class DataStructuresBenchmark4 extends SecuribenchMicroDatastructuresFixture(JVM_EXT, 4) {

  s"DataStructures$benchmarkNo" should "report secure" taggedAs DataStructures in {
    assertIsSecure(cpg.literal(".*name.*"), cpg.call(".*println.*"))
  }

}

class DataStructuresBenchmark5 extends SecuribenchMicroDatastructuresFixture(JVM_EXT, 5) {

  s"DataStructures$benchmarkNo" should "report insecure" taggedAs DataStructures in {
    assertIsInsecure(cpg.literal(".*name.*"), cpg.call(".*println.*"))
  }

}

class DataStructuresBenchmark6 extends SecuribenchMicroDatastructuresFixture(JVM_EXT, 6) {

  s"DataStructures$benchmarkNo" should "report insecure" taggedAs DataStructures in {
    assertIsInsecure(cpg.literal(".*name.*"), cpg.call(".*println.*"))
  }

}
