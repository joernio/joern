package io.joern.benchmarks.securibench.micro.java

import io.joern.benchmarks.BenchmarkTags.Inter
import io.joern.benchmarks.testfixtures.BenchmarkFixture._
import io.joern.benchmarks.testfixtures.SecuribenchMicroInterFixture
import io.shiftleft.semanticcpg.language._

class InterBenchmark1 extends SecuribenchMicroInterFixture(JAVA_EXT, 1) {

  s"Inter$benchmarkNo line 45" should "report insecure" taggedAs Inter in {
    assertIsInsecure(cpg.fieldAccess.code(".*FIELD_NAME.*"), cpg.call(".*println.*").lineNumber(45))
  }

  s"Inter$benchmarkNo line 46" should "report secure" taggedAs Inter in {
    assertIsSecure(cpg.fieldAccess.code(".*FIELD_NAME.*"), cpg.call(".*println.*").lineNumber(46))
  }

}

class InterBenchmark2 extends SecuribenchMicroInterFixture(JAVA_EXT, 2) {

  s"Inter$benchmarkNo line 44" should "report insecure" taggedAs Inter in {
    assertIsInsecure(cpg.fieldAccess.code(".*FIELD_NAME.*"), cpg.call(".*println.*").lineNumber(44))
  }

  s"Inter$benchmarkNo line 45" should "report secure" taggedAs Inter in {
    assertIsSecure(cpg.fieldAccess.code(".*FIELD_NAME.*"), cpg.call(".*println.*").lineNumber(45))
  }

}

class InterBenchmark3 extends SecuribenchMicroInterFixture(JAVA_EXT, 3) {

  s"Inter$benchmarkNo line 84" should "report insecure" taggedAs Inter in {
    assertIsInsecure(cpg.fieldAccess.code(".*FIELD_NAME.*"), cpg.call(".*println.*").lineNumber(84))
  }

  s"Inter$benchmarkNo line 94" should "report secure" taggedAs Inter in {
    assertIsSecure(cpg.fieldAccess.code(".*FIELD_NAME.*"), cpg.call(".*println.*").lineNumber(94))
  }

}

class InterBenchmark4 extends SecuribenchMicroInterFixture(JAVA_EXT, 4) {

  s"Inter$benchmarkNo" should "report insecure" taggedAs Inter in {
    assertIsInsecure(cpg.fieldAccess.code(".*FIELD_NAME.*"), cpg.call(".*println.*"))
  }

}

class InterBenchmark5 extends SecuribenchMicroInterFixture(JAVA_EXT, 5) {

  s"Inter$benchmarkNo line 45" should "report insecure" taggedAs Inter in {
    assertIsInsecure(cpg.fieldAccess.code(".*FIELD_NAME.*"), cpg.call(".*println.*").lineNumber(45))
  }

  s"Inter$benchmarkNo line 46" should "report secure" taggedAs Inter in {
    assertIsSecure(cpg.fieldAccess.code(".*FIELD_NAME.*"), cpg.call(".*println.*").lineNumber(46))
  }

}

class InterBenchmark6 extends SecuribenchMicroInterFixture(JAVA_EXT, 6) {

  s"Inter$benchmarkNo" should "report insecure" taggedAs Inter in {
    assertIsInsecure(cpg.fieldAccess.code(".*FIELD_NAME.*"), cpg.call(".*println.*"))
  }

}

class InterBenchmark7 extends SecuribenchMicroInterFixture(JAVA_EXT, 7) {

  s"Inter$benchmarkNo" should "report insecure" taggedAs Inter in {
    assertIsInsecure(cpg.fieldAccess.code(".*FIELD_NAME.*"), cpg.call(".*println.*"))
  }

}

class InterBenchmark8 extends SecuribenchMicroInterFixture(JAVA_EXT, 8) {

  s"Inter$benchmarkNo line 45" should "report insecure" taggedAs Inter in {
    assertIsInsecure(cpg.fieldAccess.code(".*FIELD_NAME.*"), cpg.call(".*println.*").lineNumber(45))
  }

  s"Inter$benchmarkNo line 46" should "report secure" taggedAs Inter in {
    assertIsSecure(cpg.fieldAccess.code(".*FIELD_NAME.*"), cpg.call(".*println.*").lineNumber(46))
  }

}

class InterBenchmark9 extends SecuribenchMicroInterFixture(JAVA_EXT, 9) {

  s"Inter$benchmarkNo line 47" should "report insecure" taggedAs Inter in {
    assertIsInsecure(cpg.fieldAccess.code(".*FIELD_NAME.*"), cpg.call(".*println.*").lineNumber(47))
  }

  s"Inter$benchmarkNo line 48" should "report secure" taggedAs Inter in {
    assertIsSecure(cpg.fieldAccess.code(".*FIELD_NAME.*"), cpg.call(".*println.*").lineNumber(48))
  }

  s"Inter$benchmarkNo line 53" should "report insecure" taggedAs Inter in {
    assertIsInsecure(cpg.fieldAccess.code(".*FIELD_NAME.*"), cpg.call(".*println.*").lineNumber(53))
  }

  s"Inter$benchmarkNo line 54" should "report secure" taggedAs Inter in {
    assertIsSecure(cpg.fieldAccess.code(".*FIELD_NAME.*"), cpg.call(".*println.*").lineNumber(54))
  }

}

class InterBenchmark10 extends SecuribenchMicroInterFixture(JAVA_EXT, 10) {

  s"Inter$benchmarkNo line 47" should "report insecure" taggedAs Inter in {
    assertIsInsecure(cpg.fieldAccess.code(".*FIELD_NAME.*"), cpg.call(".*println.*").lineNumber(47))
  }

  s"Inter$benchmarkNo line 48" should "report secure" taggedAs Inter in {
    assertIsSecure(cpg.fieldAccess.code(".*FIELD_NAME.*"), cpg.call(".*println.*").lineNumber(48))
  }

}

class InterBenchmark11 extends SecuribenchMicroInterFixture(JAVA_EXT, 11) {

  s"Inter$benchmarkNo line 47" should "report insecure" taggedAs Inter in {
    assertIsInsecure(cpg.fieldAccess.code(".*FIELD_NAME.*"), cpg.call(".*println.*").lineNumber(47))
  }

  s"Inter$benchmarkNo line 48" should "report secure" taggedAs Inter in {
    assertIsSecure(cpg.fieldAccess.code(".*FIELD_NAME.*"), cpg.call(".*println.*").lineNumber(48))
  }

}

class InterBenchmark12 extends SecuribenchMicroInterFixture(JAVA_EXT, 12) {

  s"Inter$benchmarkNo line 54" should "report insecure" taggedAs Inter in {
    assertIsInsecure(cpg.fieldAccess.code(".*FIELD_NAME.*"), cpg.call(".*println.*").lineNumber(54))
  }

  s"Inter$benchmarkNo line 55" should "report secure" taggedAs Inter in {
    assertIsSecure(cpg.fieldAccess.code(".*FIELD_NAME.*"), cpg.call(".*println.*").lineNumber(55))
  }

}

class InterBenchmark13 extends SecuribenchMicroInterFixture(JAVA_EXT, 13) {

  s"Inter$benchmarkNo line 52" should "report insecure" taggedAs Inter in {
    assertIsInsecure(cpg.fieldAccess.code(".*FIELD_NAME.*"), cpg.call(".*println.*").lineNumber(52))
  }

}

class InterBenchmark14 extends SecuribenchMicroInterFixture(JAVA_EXT, 14) {

  s"Inter$benchmarkNo line 54" should "report insecure" taggedAs Inter in {
    assertIsInsecure(cpg.fieldAccess.code(".*FIELD_NAME.*"), cpg.call(".*println.*").lineNumber(54))
  }

}
