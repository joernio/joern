package io.joern.benchmarks.securibench.micro.java

import io.joern.benchmarks.BenchmarkTags.Aliasing
import io.joern.benchmarks.testfixtures.SecuribenchMicroAliasingFixture
import io.shiftleft.semanticcpg.language._
import io.joern.benchmarks.testfixtures.BenchmarkFixture._

class AliasingBenchmark1 extends SecuribenchMicroAliasingFixture(JAVA_EXT, 1) {

  s"Aliasing$benchmarkNo" should "report insecure" taggedAs Aliasing in {
    assertIsInsecure(cpg.fieldAccess.code(".*FIELD_NAME.*"), cpg.call(".*println.*"))
  }

}

class AliasingBenchmark2 extends SecuribenchMicroAliasingFixture(JAVA_EXT, 2) {

  s"Aliasing$benchmarkNo" should "report secure" taggedAs Aliasing in {
    assertIsSecure(cpg.fieldAccess.code(".*FIELD_NAME.*"), cpg.call(".*println.*"))
  }

}

class AliasingBenchmark3 extends SecuribenchMicroAliasingFixture(JAVA_EXT, 3) {

  s"Aliasing$benchmarkNo" should "report insecure" taggedAs Aliasing in {
    assertIsInsecure(cpg.fieldAccess.code(".*FIELD_NAME.*"), cpg.call(".*println.*"))
  }

}

class AliasingBenchmark4 extends SecuribenchMicroAliasingFixture(JAVA_EXT, 4) {

  s"Aliasing$benchmarkNo line 45" should "report insecure" taggedAs Aliasing in {
    assertIsInsecure(cpg.fieldAccess.code(".*FIELD_NAME.*"), cpg.call(".*println.*").lineNumber(45))
  }

  s"Aliasing$benchmarkNo line 46" should "report insecure" taggedAs Aliasing in {
    assertIsInsecure(cpg.fieldAccess.code(".*FIELD_NAME.*"), cpg.call(".*println.*").lineNumber(46))
  }

  s"Aliasing$benchmarkNo line 47" should "report secure" taggedAs Aliasing in {
    assertIsSecure(cpg.fieldAccess.code(".*FIELD_NAME.*"), cpg.call(".*println.*").lineNumber(47))
  }

}

class AliasingBenchmark5 extends SecuribenchMicroAliasingFixture(JAVA_EXT, 5) {

  s"Aliasing$benchmarkNo" should "report insecure" taggedAs Aliasing in {
    assertIsInsecure(cpg.fieldAccess.code(".*FIELD_NAME.*"), cpg.call(".*println.*"))
  }

}

class AliasingBenchmark6 extends SecuribenchMicroAliasingFixture(JAVA_EXT, 6) {

  s"Aliasing$benchmarkNo line 48" should "report insecure" taggedAs Aliasing in {
    assertIsInsecure(cpg.fieldAccess.code(".*FIELD_NAME.*"), cpg.call(".*println.*").lineNumber(48))
  }

  s"Aliasing$benchmarkNo line 49" should "report insecure" taggedAs Aliasing in {
    assertIsInsecure(cpg.fieldAccess.code(".*FIELD_NAME.*"), cpg.call(".*println.*").lineNumber(49))
  }

  s"Aliasing$benchmarkNo line 50" should "report insecure" taggedAs Aliasing in {
    assertIsInsecure(cpg.fieldAccess.code(".*FIELD_NAME.*"), cpg.call(".*println.*").lineNumber(50))
  }

  s"Aliasing$benchmarkNo line 51" should "report insecure" taggedAs Aliasing in {
    assertIsInsecure(cpg.fieldAccess.code(".*FIELD_NAME.*"), cpg.call(".*println.*").lineNumber(51))
  }

  s"Aliasing$benchmarkNo line 52" should "report insecure" taggedAs Aliasing in {
    assertIsInsecure(cpg.fieldAccess.code(".*FIELD_NAME.*"), cpg.call(".*println.*").lineNumber(52))
  }

  s"Aliasing$benchmarkNo line 53" should "report insecure" taggedAs Aliasing in {
    assertIsInsecure(cpg.fieldAccess.code(".*FIELD_NAME.*"), cpg.call(".*println.*").lineNumber(53))
  }

  // This passes since this is the root of the alias propagation
  s"Aliasing$benchmarkNo line 54" should "report insecure" taggedAs Aliasing in {
    assertIsInsecure(cpg.fieldAccess.code(".*FIELD_NAME.*"), cpg.call(".*println.*").lineNumber(54))
  }

}
