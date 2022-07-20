package io.joern.benchmarks.ifspec.jvm

import io.joern.benchmarks.BenchmarkTags.{Casting, Exceptions, ExplicitFlows, ImplicitFlows}
import io.joern.benchmarks.testfixtures.BenchmarkFixture._
import io.joern.benchmarks.testfixtures.IfspecCastingFixture
import io.shiftleft.semanticcpg.language._

// LostInCast
class CastingBenchmark1 extends IfspecCastingFixture(JVM_EXT, 1) {

  s"Casting$benchmarkNo" should "report secure" taggedAs (Casting, ExplicitFlows) in {
    assertIsSecure(cpg.call(".*doIt.*").argument(1), cpg.method("doIt").methodReturn)
  }

}

// SimpleTypesCastingError
class CastingBenchmark2 extends IfspecCastingFixture(JVM_EXT, 2) {

  s"Casting$benchmarkNo" should "report insecure" taggedAs (Exceptions, Casting, ImplicitFlows) in {
    assertIsInsecure(cpg.fieldAccess.code(".*secret.*"), cpg.method("test").methodReturn)
  }

}
