package io.joern.benchmarks.ifspec.jvm

import io.joern.benchmarks.BenchmarkTags.{ExplicitFlows, HighConditional, ImplicitFlows}
import io.joern.benchmarks.testfixtures.BenchmarkFixture._
import io.joern.benchmarks.testfixtures.IfspecHighConditionalFixture
import io.shiftleft.semanticcpg.language._

// HighConditionalIncrementalLeakInsecure
class HighConditionalBenchmark1 extends IfspecHighConditionalFixture(JVM_EXT, 1) {

  s"HighConditional$benchmarkNo" should "report insecure" taggedAs (HighConditional, ImplicitFlows) in {
    assertIsInsecure(cpg.call(".*f.*").argument, cpg.method("f").methodReturn)
  }

}

// HighConditionalIncrementalLeakSecure
class HighConditionalBenchmark2 extends IfspecHighConditionalFixture(JVM_EXT, 2) {

  s"HighConditional$benchmarkNo" should "report secure" taggedAs (HighConditional, ImplicitFlows) in {
    assertIsSecure(cpg.method("f").parameter.code(".*h.*"), cpg.method("f").methodReturn)
    assertIsSecure(cpg.method("f").parameter.code(".*l.*"), cpg.method("f").parameter.code(".*h.*"))
  }

}

// IFLoop1
class HighConditionalBenchmark3 extends IfspecHighConditionalFixture(JVM_EXT, 3) {

  s"HighConditional$benchmarkNo" should "report secure" taggedAs (HighConditional, ExplicitFlows) in {
    assertIsSecure(cpg.method("secure_ifl").call(".*secure_ifl.*").argument(1), cpg.method("secure_ifl").methodReturn)
  }

}

// IFLoop2
class HighConditionalBenchmark4 extends IfspecHighConditionalFixture(JVM_EXT, 4) {

  s"HighConditional$benchmarkNo" should "report insecure" taggedAs (HighConditional, ExplicitFlows) in {
    assertIsInsecure(cpg.fieldAccess.code(".*high.*"), cpg.fieldAccess.code(".*low.*"))
  }

}

// ScenarioBankingInsecure
class HighConditionalBenchmark5 extends IfspecHighConditionalFixture(JVM_EXT, 5) {

  s"HighConditional$benchmarkNo" should "report insecure" taggedAs (HighConditional, ImplicitFlows) in {
    assertIsInsecure(cpg.fieldAccess.code(".*balance.*"), cpg.call(".*log.*").argument)
  }

}

// ScenarioBankingSecure
class HighConditionalBenchmark6 extends IfspecHighConditionalFixture(JVM_EXT, 6) {

  s"HighConditional$benchmarkNo" should "report secure" taggedAs (HighConditional, ImplicitFlows) in {
    assertIsSecure(cpg.fieldAccess.code(".*balance.*"), cpg.call(".*log.*").argument)
  }

}

// ScenarioPasswordInsecure
class HighConditionalBenchmark7 extends IfspecHighConditionalFixture(JVM_EXT, 7) {

  s"HighConditional$benchmarkNo" should "report insecure" taggedAs (HighConditional, ImplicitFlows) in {
    assertIsInsecure(cpg.fieldAccess.code(".*[password|loggedIn|invalidTries].*"), cpg.call(".*println.*").argument)
  }

}

// ScenarioPasswordSecure
class HighConditionalBenchmark8 extends IfspecHighConditionalFixture(JVM_EXT, 8) {

  s"HighConditional$benchmarkNo" should "report secure" taggedAs (HighConditional, ImplicitFlows) in {
    assertIsSecure(cpg.fieldAccess.code(".*[password|loggedIn|invalidTries].*"), cpg.call(".*println.*").argument)
  }

}

// SimpleConditionalAssignmentEqual
class HighConditionalBenchmark9 extends IfspecHighConditionalFixture(JVM_EXT, 9) {

  s"HighConditional$benchmarkNo" should "report secure" taggedAs (HighConditional, ImplicitFlows) in {
    assertIsSecure(cpg.fieldAccess.code(".*secret.*"), cpg.method("test").methodReturn)
  }

}

// SimpleErasureByConditionalChecks
class HighConditionalBenchmark10 extends IfspecHighConditionalFixture(JVM_EXT, 10) {

  s"HighConditional$benchmarkNo" should "report secure" taggedAs (HighConditional, ImplicitFlows) in {
    assertIsSecure(cpg.call(".*computeSecretly.*").argument(1), cpg.method("computeSecretly").methodReturn)
  }

}

// SimpleTypes
class HighConditionalBenchmark11 extends IfspecHighConditionalFixture(JVM_EXT, 11) {

  s"HighConditional$benchmarkNo" should "report insecure" taggedAs (HighConditional, ImplicitFlows) in {
    assertIsInsecure(cpg.fieldAccess.code(".*secret.*"), cpg.method("test").methodReturn)
  }

}

// Webstore4
class HighConditionalBenchmark12 extends IfspecHighConditionalFixture(JVM_EXT, 12) {

  s"HighConditional$benchmarkNo" should "report secure" taggedAs (HighConditional, ImplicitFlows) in {
    assertIsSecure(cpg.fieldAccess.code(".*[vPrime|street].*"), cpg.method("seePreview").methodReturn)
    assertIsSecure(cpg.call(".*reinit.*").argument, cpg.method("seePreview").methodReturn)
  }

}
