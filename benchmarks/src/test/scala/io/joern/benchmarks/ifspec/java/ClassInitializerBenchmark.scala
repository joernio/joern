package io.joern.benchmarks.ifspec.java

import io.joern.benchmarks.BenchmarkTags.{Arrays, ClassInitializer, ExplicitFlows}
import io.joern.benchmarks.testfixtures.BenchmarkFixture._
import io.joern.benchmarks.testfixtures.IfspecClassInitializerFixture
import io.shiftleft.semanticcpg.language._

// StaticInitializersArrayAccessInsecure
class ClassInitializerBenchmark1 extends IfspecClassInitializerFixture(JAVA_EXT, 1) {

  s"ClassInitializer$benchmarkNo" should "report insecure" taggedAs (Arrays, ClassInitializer, ExplicitFlows) in {
    assertIsInsecure(cpg.fieldAccess.code(".*secret.*"), cpg.call(".*println.*").argument(1))
  }

}

// StaticInitializersArrayAccessSecure
class ClassInitializerBenchmark2 extends IfspecClassInitializerFixture(JAVA_EXT, 2) {

  s"ClassInitializer$benchmarkNo" should "report secure" taggedAs (Arrays, ClassInitializer, ExplicitFlows) in {
    assertIsSecure(cpg.fieldAccess.code(".*secret.*"), cpg.call(".*println.*").argument(1))
  }

}

// StaticInitializersHighAccessInsecure
class ClassInitializerBenchmark3 extends IfspecClassInitializerFixture(JAVA_EXT, 3) {

  s"ClassInitializer$benchmarkNo" should "report insecure" taggedAs (ClassInitializer, ExplicitFlows) in {
    assertIsInsecure(cpg.fieldAccess.code(".*secret.*"), cpg.call(".*println.*").argument(1))
  }

}

// StaticInitializersHighAccessSecure
class ClassInitializerBenchmark4 extends IfspecClassInitializerFixture(JAVA_EXT, 4) {

  s"ClassInitializer$benchmarkNo" should "report secure" taggedAs (ClassInitializer, ExplicitFlows) in {
    assertIsSecure(cpg.fieldAccess.code(".*secret.*"), cpg.call(".*println.*").argument(1))
  }

}

// StaticInitializersLeak
class ClassInitializerBenchmark5 extends IfspecClassInitializerFixture(JAVA_EXT, 5) {

  s"ClassInitializer$benchmarkNo" should "report insecure" taggedAs (ClassInitializer, ExplicitFlows) in {
    assertIsInsecure(cpg.fieldAccess.code(".*h.*"), cpg.fieldAccess.code(".*l.*"))
  }

}

// StaticInitializersNoLeak
class ClassInitializerBenchmark6 extends IfspecClassInitializerFixture(JAVA_EXT, 6) {

  s"ClassInitializer$benchmarkNo" should "report secure" taggedAs (ClassInitializer, ExplicitFlows) in {
    assertIsSecure(cpg.fieldAccess.code(".*h.*"), cpg.fieldAccess.code(".*l.*"))
  }

}

// StaticInitializersNotCalled
class ClassInitializerBenchmark7 extends IfspecClassInitializerFixture(JAVA_EXT, 7) {

  s"ClassInitializer$benchmarkNo" should "report secure" taggedAs (ClassInitializer, ExplicitFlows) in {
    assertIsSecure(cpg.fieldAccess.code(".*secret"), cpg.call(".*println.*").argument(1))
  }

}
