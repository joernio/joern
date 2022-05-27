package io.joern.benchmarks.securibench.micro.jvm

import io.joern.benchmarks.BenchmarkTags.Sanitizers
import io.joern.benchmarks.testfixtures.BenchmarkFixture._
import io.joern.benchmarks.testfixtures.SecuribenchMicroSanitizersFixture
import io.shiftleft.semanticcpg.language._

class SanitizersBenchmark1 extends SecuribenchMicroSanitizersFixture(JVM_EXT, 1) {

  s"Sanitizers$benchmarkNo line 47" should "report insecure" taggedAs Sanitizers in {
    assertIsInsecure(cpg.literal(".*name.*"), cpg.call(".*println.*").lineNumber(47))
  }

  s"Sanitizers$benchmarkNo line 48" should "report secure" taggedAs Sanitizers in {
    assertIsSecure(cpg.literal(".*name.*"), cpg.call(".*println.*").lineNumber(48))
  }

}

class SanitizersBenchmark2 extends SecuribenchMicroSanitizersFixture(JVM_EXT, 2) {

  s"Sanitizers$benchmarkNo" should "report secure" taggedAs Sanitizers in {
    assertIsSecure(cpg.literal(".*name.*"), cpg.call(".*println.*"))
  }

}

class SanitizersBenchmark3 extends SecuribenchMicroSanitizersFixture(JVM_EXT, 3) {

  s"Sanitizers$benchmarkNo" should "report secure" taggedAs Sanitizers in {
    assertIsSecure(cpg.literal(".*name.*"), cpg.call(".*println.*"))
  }

}

class SanitizersBenchmark4 extends SecuribenchMicroSanitizersFixture(JVM_EXT, 4) {

  s"Sanitizers$benchmarkNo line 45" should "report insecure" taggedAs Sanitizers in {
    assertIsInsecure(cpg.literal(".*name.*"), cpg.call(".*println.*").lineNumber(45))
  }

  s"Sanitizers$benchmarkNo line 46" should "report insecure" taggedAs Sanitizers in {
    assertIsInsecure(cpg.literal(".*name.*"), cpg.call(".*println.*").lineNumber(46))
  }

}

class SanitizersBenchmark5 extends SecuribenchMicroSanitizersFixture(JVM_EXT, 5) {

  s"Sanitizers$benchmarkNo line 46" should "report insecure" taggedAs Sanitizers in {
    assertIsInsecure(cpg.literal(".*name.*"), cpg.call(".*println.*").lineNumber(46))
  }

  s"Sanitizers$benchmarkNo line 47" should "report secure" taggedAs Sanitizers in {
    assertIsSecure(cpg.literal(".*name.*"), cpg.call(".*println.*").lineNumber(47))
  }

}

class SanitizersBenchmark6 extends SecuribenchMicroSanitizersFixture(JVM_EXT, 6) {

  s"Sanitizers$benchmarkNo" should "report secure" taggedAs Sanitizers in {
    assertIsSecure(cpg.literal(".*name.*"), cpg.call(".*println.*"))
  }

}
