package io.joern.benchmarks.ifspec.java

import io.joern.benchmarks.BenchmarkTags.{ExplicitFlows, ImplicitFlows, Simple}
import io.joern.benchmarks.testfixtures.BenchmarkFixture._
import io.joern.benchmarks.testfixtures.IfspecSimpleFixture
import io.shiftleft.semanticcpg.language._

// BooleanOperationsInsecure
class SimpleBenchmark1 extends IfspecSimpleFixture(JAVA_EXT, 1) {

  s"Simple$benchmarkNo" should "report insecure" taggedAs (Simple, ExplicitFlows) in {
    assertIsInsecure(cpg.method("leakyMethod").parameter, cpg.method("leakyMethod").methodReturn)
  }

}

// BooleanOperationsSecure
class SimpleBenchmark2 extends IfspecSimpleFixture(JAVA_EXT, 2) {

  s"Simple$benchmarkNo" should "report secure" taggedAs (Simple, ExplicitFlows) in {
    assertIsSecure(cpg.method("leakyMethod").parameter, cpg.method("leakyMethod").methodReturn)
  }

}

// CallContext
class SimpleBenchmark3 extends IfspecSimpleFixture(JAVA_EXT, 3) {

  s"Simple$benchmarkNo" should "report secure" taggedAs (Simple, ExplicitFlows) in {
    assertIsSecure(cpg.call(".*foo.*").argument(1), cpg.method("foo").methodReturn)
  }

}

// DeepCall1
class SimpleBenchmark4 extends IfspecSimpleFixture(JAVA_EXT, 4) {

  s"Simple$benchmarkNo" should "report insecure" taggedAs (Simple, ExplicitFlows) in {
    assertIsInsecure(cpg.call(".*foo.*").argument(1), cpg.method("foo").methodReturn)
  }

}

// DeepCall2
class SimpleBenchmark5 extends IfspecSimpleFixture(JAVA_EXT, 5) {

  s"Simple$benchmarkNo" should "report secure" taggedAs (Simple, ExplicitFlows) in {
    assertIsSecure(cpg.call(".*foo.*").argument(1), cpg.method("foo").methodReturn)
  }

}

// DirectAssignment
class SimpleBenchmark6 extends IfspecSimpleFixture(JAVA_EXT, 6) {

  s"Simple$benchmarkNo" should "report insecure" taggedAs (Simple, ExplicitFlows) in {
    assertIsInsecure(cpg.call(".*leakyMethod.*").argument(1), cpg.method("leakyMethod").methodReturn)
  }

}

// DirectAssignmentLeak
class SimpleBenchmark7 extends IfspecSimpleFixture(JAVA_EXT, 7) {

  s"Simple$benchmarkNo" should "report insecure" taggedAs (Simple, ExplicitFlows) in {
    assertIsInsecure(cpg.call(".*f.*").argument(1), cpg.method("f").methodReturn)
  }

}

// DirectAssignmentSecure
class SimpleBenchmark8 extends IfspecSimpleFixture(JAVA_EXT, 8) {

  s"Simple$benchmarkNo" should "report secure" taggedAs (Simple, ExplicitFlows) in {
    assertIsSecure(cpg.call(".*leakyMethod.*").argument(1), cpg.method("leakyMethod").methodReturn)
  }

}

// IFMethodContract1
class SimpleBenchmark9 extends IfspecSimpleFixture(JAVA_EXT, 9) {

  s"Simple$benchmarkNo" should "report secure" taggedAs (Simple, ExplicitFlows) in {
    assertIsSecure(cpg.fieldAccess.code(".*high.*"), cpg.fieldAccess.code(".*low.*"))
  }

}

// IFMethodContract2
class SimpleBenchmark10 extends IfspecSimpleFixture(JAVA_EXT, 10) {

  s"Simple$benchmarkNo" should "report secure" taggedAs (Simple, ExplicitFlows) in {
    assertIsSecure(cpg.method("insecure_if_high_n1").parameter.code(".*high.*"), cpg.method("insecure_if_high_n1"))
  }

}

// ObjectSensLeak
class SimpleBenchmark11 extends IfspecSimpleFixture(JAVA_EXT, 11) {

  s"Simple$benchmarkNo" should "report secure" taggedAs (Simple, ExplicitFlows) in {
    assertIsSecure(cpg.call(".*test.*").argument, cpg.method("test").methodReturn)
  }

}

// Polynomial
class SimpleBenchmark12 extends IfspecSimpleFixture(JAVA_EXT, 12) {

  s"Simple$benchmarkNo" should "report secure" taggedAs (Simple, ImplicitFlows) in {
    assertIsSecure(cpg.call(".*compute.*").argument(1), cpg.method("compute").methodReturn)
  }

}

// ReviewerAnonymityLeak
class SimpleBenchmark13 extends IfspecSimpleFixture(JAVA_EXT, 13) {

  s"Simple$benchmarkNo" should "report insecure" taggedAs (Simple, ExplicitFlows) in {
    assertIsInsecure(cpg.call(".*addReview.*").argument, cpg.call("println").argument)
  }

}

// ReviewerAnonymityNoLeak
class SimpleBenchmark14 extends IfspecSimpleFixture(JAVA_EXT, 14) {

  s"Simple$benchmarkNo" should "report secure" taggedAs (Simple, ExplicitFlows) in {
    assertIsSecure(cpg.call(".*addReview.*").argument(1), cpg.call("println").argument)
  }

}

// SimpleRandomErasure1
class SimpleBenchmark15 extends IfspecSimpleFixture(JAVA_EXT, 15) {

  s"Simple$benchmarkNo" should "report insecure" taggedAs (Simple, ImplicitFlows) in {
    assertIsInsecure(cpg.fieldAccess.code(".*secret.*"), cpg.call("println").argument)
  }

}

// SimpleRandomErasure2
class SimpleBenchmark16 extends IfspecSimpleFixture(JAVA_EXT, 16) {

  s"Simple$benchmarkNo" should "report secure" taggedAs (Simple, ImplicitFlows) in {
    assertIsSecure(cpg.fieldAccess.code(".*secret.*"), cpg.call("println").argument)
  }

}

// StaticDispatching
class SimpleBenchmark17 extends IfspecSimpleFixture(JAVA_EXT, 17) {

  s"Simple$benchmarkNo" should "report secure" taggedAs (Simple, ExplicitFlows) in {
    assertIsInsecure(cpg.fieldAccess.code(".*[h|l].*"), cpg.fieldAccess.code(".*[hsink|lsink].*"))
  }

}

// Webstore3
class SimpleBenchmark18 extends IfspecSimpleFixture(JAVA_EXT, 18) {

  s"Simple$benchmarkNo" should "report secure" taggedAs (Simple, ExplicitFlows) in {
    assertIsSecure(cpg.call(".*setDeliveryAdr.*").argument, cpg.fieldAccess.code(".*[name|street].*"))
  }

}
