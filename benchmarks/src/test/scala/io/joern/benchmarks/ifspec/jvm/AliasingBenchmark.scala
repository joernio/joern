package io.joern.benchmarks.ifspec.jvm

import io.joern.benchmarks.BenchmarkTags.{Aliasing, ExplicitFlows, ImplicitFlows}
import io.joern.benchmarks.testfixtures.BenchmarkFixture._
import io.joern.benchmarks.testfixtures.IfspecAliasingFixture
import io.shiftleft.semanticcpg.language._

// AliasingControlFlowInsecure
class AliasingBenchmark1 extends IfspecAliasingFixture(JVM_EXT, 1) {

  s"Aliasing$benchmarkNo" should "report insecure" taggedAs (Aliasing, ImplicitFlows) in {
    assertIsInsecure(cpg.fieldAccess.code(".*secret"), cpg.call(".*println.*").argument(1))
  }

}

// AliasingControlFlowSecure
class AliasingBenchmark2 extends IfspecAliasingFixture(JVM_EXT, 2) {

  s"Aliasing$benchmarkNo" should "report secure" taggedAs (Aliasing, ImplicitFlows) in {
    assertIsSecure(cpg.fieldAccess.code(".*secret"), cpg.call(".*println.*").argument(1))
  }

}

// AliasingInterProceduralInsecure
class AliasingBenchmark3 extends IfspecAliasingFixture(JVM_EXT, 3) {

  s"Aliasing$benchmarkNo" should "report insecure" taggedAs (Aliasing, ExplicitFlows) in {
    assertIsInsecure(cpg.fieldAccess.code(".*secret"), cpg.call(".*println.*").argument(1))
  }

}

// AliasingInterProceduralSecure
class AliasingBenchmark4 extends IfspecAliasingFixture(JVM_EXT, 4) {

  s"Aliasing$benchmarkNo" should "report secure" taggedAs (Aliasing, ExplicitFlows) in {
    assertIsSecure(cpg.fieldAccess.code(".*secret"), cpg.call(".*println.*").argument(1))
  }

}

// AliasingNestedInsecure
class AliasingBenchmark5 extends IfspecAliasingFixture(JVM_EXT, 5) {

  s"Aliasing$benchmarkNo" should "report insecure" taggedAs (Aliasing, ExplicitFlows) in {
    assertIsInsecure(cpg.fieldAccess.code(".*secret"), cpg.call(".*println.*").argument(1))
  }

}

// AliasingNestedSecure
class AliasingBenchmark6 extends IfspecAliasingFixture(JVM_EXT, 6) {

  s"Aliasing$benchmarkNo" should "report secure" taggedAs (Aliasing, ExplicitFlows) in {
    assertIsSecure(cpg.fieldAccess.code(".*secret"), cpg.call(".*println.*").argument(1))
  }

}

// AliasingSimpleInsecure
class AliasingBenchmark7 extends IfspecAliasingFixture(JVM_EXT, 7) {

  s"Aliasing$benchmarkNo" should "report insecure" taggedAs (Aliasing, ExplicitFlows) in {
    assertIsInsecure(cpg.method.call(".*test.*").argument(1), cpg.method("test").ast.isReturn)
  }

}

// AliasingSimpleSecure
class AliasingBenchmark8 extends IfspecAliasingFixture(JVM_EXT, 8) {

  s"Aliasing$benchmarkNo" should "report secure" taggedAs (Aliasing, ExplicitFlows) in {
    assertIsSecure(cpg.method.call(".*test.*").argument(1), cpg.method("test").ast.isReturn)
  }

}

// AliasingStrongUpdateSecure
class AliasingBenchmark9 extends IfspecAliasingFixture(JVM_EXT, 9) {

  s"Aliasing$benchmarkNo" should "report secure" taggedAs (Aliasing, ImplicitFlows) in {
    assertIsSecure(cpg.fieldAccess.code(".*secret"), cpg.call(".*println.*").argument(1))
  }

}

// DeepAlias1
class AliasingBenchmark10 extends IfspecAliasingFixture(JVM_EXT, 10) {

  s"Aliasing$benchmarkNo" should "report insecure" taggedAs (Aliasing, ExplicitFlows) in {
    assertIsInsecure(cpg.call(".*foo.*").argument(1), cpg.method("foo").ast.isReturn)
  }

}

// DeepAlias2
class AliasingBenchmark11 extends IfspecAliasingFixture(JVM_EXT, 11) {

  s"Aliasing$benchmarkNo" should "report secure" taggedAs (Aliasing, ExplicitFlows) in {
    assertIsSecure(cpg.call(".*foo.*").argument(1), cpg.method("foo").ast.isReturn)
  }

}
