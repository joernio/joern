package io.joern.benchmarks.securibench.micro.java

import io.joern.benchmarks.BenchmarkTags.Basic
import io.joern.benchmarks.testfixtures.BenchmarkFixture._
import io.joern.benchmarks.testfixtures.SecuribenchMicroBasicFixture
import io.shiftleft.semanticcpg.language._

class BasicBenchmark1 extends SecuribenchMicroBasicFixture(JAVA_EXT, 1) {

  s"Basic$benchmarkNo" should "report insecure" taggedAs Basic in {
    assertIsInsecure(cpg.literal.code(".*name.*"), cpg.call(".*println.*"))
  }

}

class BasicBenchmark2 extends SecuribenchMicroBasicFixture(JAVA_EXT, 2) {

  s"Basic$benchmarkNo" should "report insecure" taggedAs Basic in {
    assertIsInsecure(cpg.literal.code(".*name.*"), cpg.call(".*println.*"))
  }

}

class BasicBenchmark3 extends SecuribenchMicroBasicFixture(JAVA_EXT, 3) {

  s"Basic$benchmarkNo" should "report insecure" taggedAs Basic in {
    assertIsInsecure(cpg.literal.code(".*name.*"), cpg.call(".*println.*"))
  }

}

class BasicBenchmark4 extends SecuribenchMicroBasicFixture(JAVA_EXT, 4) {

  s"Basic$benchmarkNo" should "report insecure" taggedAs Basic in {
    assertIsInsecure(cpg.literal.code(".*name.*"), cpg.call(".*println.*"))
  }

}

class BasicBenchmark5 extends SecuribenchMicroBasicFixture(JAVA_EXT, 5) {

  s"Basic$benchmarkNo line 43" should "report insecure" taggedAs Basic in {
    assertIsInsecure(cpg.literal.code(".*name.*"), cpg.call(".*println.*").lineNumber(43))
  }

  s"Basic$benchmarkNo line 44" should "report insecure" taggedAs Basic in {
    assertIsInsecure(cpg.literal.code(".*name.*"), cpg.call(".*println.*").lineNumber(44))
  }

  s"Basic$benchmarkNo line 45" should "report insecure" taggedAs Basic in {
    assertIsInsecure(cpg.literal.code(".*name.*"), cpg.call(".*println.*").lineNumber(45))
  }

}

class BasicBenchmark6 extends SecuribenchMicroBasicFixture(JAVA_EXT, 6) {

  s"Basic$benchmarkNo" should "report insecure" taggedAs Basic in {
    assertIsInsecure(cpg.literal.code(".*name.*"), cpg.call(".*println.*"))
  }

}

class BasicBenchmark7 extends SecuribenchMicroBasicFixture(JAVA_EXT, 7) {

  s"Basic$benchmarkNo" should "report insecure" taggedAs Basic in {
    assertIsInsecure(cpg.literal.code(".*name.*"), cpg.call(".*println.*"))
  }

}

class BasicBenchmark8 extends SecuribenchMicroBasicFixture(JAVA_EXT, 8) {

  s"Basic$benchmarkNo" should "report insecure" taggedAs Basic in {
    assertIsInsecure(cpg.literal.code(".*name.*"), cpg.call(".*println.*"))
  }

}

class BasicBenchmark9 extends SecuribenchMicroBasicFixture(JAVA_EXT, 9) {

  s"Basic$benchmarkNo" should "report insecure" taggedAs Basic in {
    assertIsInsecure(cpg.literal.code(".*name.*"), cpg.call(".*println.*"))
  }

}

class BasicBenchmark10 extends SecuribenchMicroBasicFixture(JAVA_EXT, 10) {

  s"Basic$benchmarkNo" should "report insecure" taggedAs Basic in {
    assertIsInsecure(cpg.literal.code(".*name.*"), cpg.call(".*println.*"))
  }

}

class BasicBenchmark11 extends SecuribenchMicroBasicFixture(JAVA_EXT, 11) {

  s"Basic$benchmarkNo line 42" should "report insecure" taggedAs Basic in {
    assertIsInsecure(cpg.literal.code(".*name.*"), cpg.call(".*println.*").lineNumber(42))
  }

  s"Basic$benchmarkNo line 43" should "report insecure" taggedAs Basic in {
    assertIsInsecure(cpg.literal.code(".*name.*"), cpg.call(".*println.*").lineNumber(43))
  }

  s"Basic$benchmarkNo line 44" should "report secure" taggedAs Basic in {
    assertIsSecure(cpg.literal.code(".*name.*"), cpg.call(".*println.*").lineNumber(44))
  }

}

class BasicBenchmark12 extends SecuribenchMicroBasicFixture(JAVA_EXT, 12) {

  s"Basic$benchmarkNo line 42" should "report insecure" taggedAs Basic in {
    assertIsInsecure(cpg.literal.code(".*name.*"), cpg.call(".*println.*").lineNumber(42))
  }

  s"Basic$benchmarkNo line 44" should "report insecure" taggedAs Basic in {
    assertIsInsecure(cpg.literal.code(".*name.*"), cpg.call(".*println.*").lineNumber(44))
  }

  s"Basic$benchmarkNo line 47" should "report secure" taggedAs Basic in {
    assertIsSecure(cpg.literal.code(".*name.*"), cpg.call(".*println.*").lineNumber(47))
  }

}

class BasicBenchmark13 extends SecuribenchMicroBasicFixture(JAVA_EXT, 13) {

  s"Basic$benchmarkNo" should "report insecure" taggedAs Basic in {
    assertIsInsecure(cpg.literal.code(".*name.*"), cpg.call(".*println.*"))
  }

}

class BasicBenchmark14 extends SecuribenchMicroBasicFixture(JAVA_EXT, 14) {

  s"Basic$benchmarkNo" should "report insecure" taggedAs Basic in {
    assertIsInsecure(cpg.call(".*getServletConfig.*").receiver, cpg.call(".*println.*"))
  }

}

class BasicBenchmark15 extends SecuribenchMicroBasicFixture(JAVA_EXT, 15) {

  s"Basic$benchmarkNo" should "report insecure" taggedAs Basic in {
    assertIsInsecure(cpg.fieldAccess.code(".*FIELD_NAME.*"), cpg.call(".*println.*"))
  }

}

class BasicBenchmark16 extends SecuribenchMicroBasicFixture(JAVA_EXT, 16) {

  s"Basic$benchmarkNo" should "report insecure" taggedAs Basic in {
    assertIsInsecure(cpg.fieldAccess.code(".*FIELD_NAME.*"), cpg.call(".*println.*"))
  }

}

class BasicBenchmark17 extends SecuribenchMicroBasicFixture(JAVA_EXT, 17) {

  s"Basic$benchmarkNo line 58" should "report insecure" taggedAs Basic in {
    assertIsInsecure(cpg.fieldAccess.code(".*FIELD_NAME.*"), cpg.call(".*println.*").lineNumber(58))
  }

  s"Basic$benchmarkNo line 59" should "report secure" taggedAs Basic in {
    assertIsSecure(cpg.fieldAccess.code(".*FIELD_NAME.*"), cpg.call(".*println.*").lineNumber(59))
  }

}

class BasicBenchmark18 extends SecuribenchMicroBasicFixture(JAVA_EXT, 18) {

  s"Basic$benchmarkNo" should "report insecure" taggedAs Basic in {
    assertIsInsecure(cpg.fieldAccess.code(".*FIELD_NAME.*"), cpg.call(".*println.*"))
  }

}

class BasicBenchmark19 extends SecuribenchMicroBasicFixture(JAVA_EXT, 19) {

  // If this went into argument two it would be safe
  s"Basic$benchmarkNo" should "report insecure" taggedAs Basic in {
    assertIsInsecure(cpg.fieldAccess.code(".*FIELD_NAME.*"), cpg.call(".*prepareStatement.*").argument(1))
  }

}

class BasicBenchmark20 extends SecuribenchMicroBasicFixture(JAVA_EXT, 20) {

  s"Basic$benchmarkNo" should "report insecure" taggedAs Basic in {
    assertIsInsecure(cpg.fieldAccess.code(".*FIELD_NAME.*"), cpg.call(".*execute.*").argument(1))
  }

}

class BasicBenchmark21 extends SecuribenchMicroBasicFixture(JAVA_EXT, 21) {

  s"Basic$benchmarkNo line 49" should "report insecure" taggedAs Basic in {
    assertIsInsecure(cpg.fieldAccess.code(".*FIELD_NAME.*"), cpg.call(".*executeUpdate.*").argument(1).lineNumber(49))
  }

  s"Basic$benchmarkNo line 50" should "report insecure" taggedAs Basic in {
    assertIsInsecure(cpg.fieldAccess.code(".*FIELD_NAME.*"), cpg.call(".*executeUpdate.*").argument(1).lineNumber(50))
  }

  s"Basic$benchmarkNo line 51" should "report insecure" taggedAs Basic in {
    assertIsInsecure(cpg.fieldAccess.code(".*FIELD_NAME.*"), cpg.call(".*executeUpdate.*").argument(1).lineNumber(51))
  }

  s"Basic$benchmarkNo line 53" should "report insecure" taggedAs Basic in {
    assertIsInsecure(cpg.fieldAccess.code(".*FIELD_NAME.*"), cpg.call(".*executeQuery.*").argument(1).lineNumber(53))
  }

}

class BasicBenchmark22 extends SecuribenchMicroBasicFixture(JAVA_EXT, 22) {

  s"Basic$benchmarkNo" should "report insecure" taggedAs Basic in {
    assertIsInsecure(cpg.fieldAccess.code(".*FIELD_NAME.*"), cpg.call(".*createNewFile.*"))
  }

}

class BasicBenchmark23 extends SecuribenchMicroBasicFixture(JAVA_EXT, 23) {

  s"Basic$benchmarkNo line 44" should "report insecure" taggedAs Basic in {
    assertIsInsecure(
      cpg.fieldAccess.code(".*FIELD_NAME.*"),
      cpg.call.nameExact("<init>").code(".*FileWriter.*").argument(1).lineNumber(44)
    )
  }

  s"Basic$benchmarkNo line 45" should "report insecure" taggedAs Basic in {
    assertIsInsecure(
      cpg.fieldAccess.code(".*FIELD_NAME.*"),
      cpg.call.nameExact("<init>").code(".*FileWriter.*").argument(1).lineNumber(45)
    )
  }

  s"Basic$benchmarkNo line 46" should "report insecure" taggedAs Basic in {
    assertIsInsecure(
      cpg.fieldAccess.code(".*FIELD_NAME.*"),
      cpg.call.nameExact("<init>").code(".*FileInputStream.*").argument(1).lineNumber(46)
    )
  }

}

class BasicBenchmark24 extends SecuribenchMicroBasicFixture(JAVA_EXT, 24) {

  s"Basic$benchmarkNo" should "report insecure" taggedAs Basic in {
    assertIsInsecure(cpg.fieldAccess.code(".*FIELD_NAME.*"), cpg.call(".*sendRedirect.*").argument(1))
  }

}

class BasicBenchmark25 extends SecuribenchMicroBasicFixture(JAVA_EXT, 25) {

  s"Basic$benchmarkNo" should "report insecure" taggedAs Basic in {
    assertIsInsecure(cpg.fieldAccess.code(".*FIELD_NAME.*"), cpg.call(".*println.*"))
  }

}

class BasicBenchmark26 extends SecuribenchMicroBasicFixture(JAVA_EXT, 26) {

  s"Basic$benchmarkNo" should "report insecure" taggedAs Basic in {
    assertIsInsecure(cpg.fieldAccess.code(".*FIELD_NAME.*"), cpg.call(".*println.*"))
  }

}

class BasicBenchmark27 extends SecuribenchMicroBasicFixture(JAVA_EXT, 27) {

  s"Basic$benchmarkNo" should "report insecure" taggedAs Basic in {
    assertIsInsecure(cpg.fieldAccess.code(".*FIELD_NAME.*"), cpg.call(".*println.*"))
  }

}

class BasicBenchmark28 extends SecuribenchMicroBasicFixture(JAVA_EXT, 28) {

  s"Basic$benchmarkNo line 72" should "report insecure" taggedAs Basic in {
    assertIsInsecure(cpg.literal.code(".*name.*"), cpg.call(".*println.*").argument(1).lineNumber(72))
  }

  s"Basic$benchmarkNo line 140" should "report insecure" taggedAs Basic in {
    assertIsInsecure(cpg.literal.code(".*name.*"), cpg.call(".*println.*").argument(1).lineNumber(140))
  }

}

class BasicBenchmark29 extends SecuribenchMicroBasicFixture(JAVA_EXT, 29) {

  s"Basic$benchmarkNo line 48" should "report insecure" taggedAs Basic in {
    assertIsInsecure(cpg.literal.code(".*name.*"), cpg.call(".*println.*").lineNumber(48))
  }

  s"Basic$benchmarkNo line 49" should "report insecure" taggedAs Basic in {
    assertIsInsecure(cpg.literal.code(".*name.*"), cpg.call(".*println.*").lineNumber(49))
  }

  s"Basic$benchmarkNo line 50" should "report secure" taggedAs Basic in {
    assertIsSecure(cpg.literal.code(".*name.*"), cpg.call(".*println.*").lineNumber(50))
  }

}

class BasicBenchmark30 extends SecuribenchMicroBasicFixture(JAVA_EXT, 30) {

  s"Basic$benchmarkNo line 47" should "report secure" taggedAs Basic in {
    assertIsSecure(cpg.literal.code(".*name.*"), cpg.call(".*println.*").lineNumber(47))
  }

  s"Basic$benchmarkNo line 48" should "report insecure" taggedAs Basic in {
    assertIsInsecure(cpg.literal.code(".*name.*"), cpg.call(".*println.*").lineNumber(48))
  }

}

class BasicBenchmark31 extends SecuribenchMicroBasicFixture(JAVA_EXT, 31) {

  s"Basic$benchmarkNo line 51" should "report insecure" taggedAs Basic in {
    assertIsInsecure(cpg.call(".*getCookies.*").receiver, cpg.call(".*println.*").lineNumber(51))
  }

  s"Basic$benchmarkNo line 54" should "report insecure" taggedAs Basic in {
    assertIsInsecure(cpg.call(".*getCookies.*").receiver, cpg.call(".*println.*").lineNumber(54))
  }

  s"Basic$benchmarkNo line 57" should "report insecure" taggedAs Basic in {
    assertIsInsecure(cpg.call(".*getCookies.*").receiver, cpg.call(".*println.*").lineNumber(57))
  }

}

class BasicBenchmark32 extends SecuribenchMicroBasicFixture(JAVA_EXT, 32) {

  s"Basic$benchmarkNo" should "report insecure" taggedAs Basic in {
    assertIsInsecure(cpg.call(".*getHeader.*").receiver, cpg.call(".*println.*"))
  }

}

class BasicBenchmark33 extends SecuribenchMicroBasicFixture(JAVA_EXT, 33) {

  s"Basic$benchmarkNo" should "report insecure" taggedAs Basic in {
    assertIsInsecure(cpg.call(".*getHeader.*").receiver, cpg.call(".*println.*"))
  }

}

class BasicBenchmark34 extends SecuribenchMicroBasicFixture(JAVA_EXT, 34) {

  s"Basic$benchmarkNo line 45" should "report insecure" taggedAs Basic in {
    assertIsInsecure(cpg.call(".*getHeader.*").receiver, cpg.call(".*println.*").lineNumber(45))
  }

  s"Basic$benchmarkNo line 46" should "report insecure" taggedAs Basic in {
    assertIsInsecure(cpg.call(".*getHeader.*").receiver, cpg.call(".*println.*").lineNumber(46))
  }

}

class BasicBenchmark35 extends SecuribenchMicroBasicFixture(JAVA_EXT, 35) {

  s"Basic$benchmarkNo line 42" should "report insecure" taggedAs Basic in {
    assertIsInsecure(cpg.call(".*getHeaderNames.*").receiver, cpg.call(".*println.*").lineNumber(42))
  }

  s"Basic$benchmarkNo line 43" should "report insecure" taggedAs Basic in {
    assertIsInsecure(cpg.call(".*getHeaderNames.*").receiver, cpg.call(".*println.*").lineNumber(43))
  }

  s"Basic$benchmarkNo line 44" should "report insecure" taggedAs Basic in {
    assertIsInsecure(cpg.call(".*getHeaderNames.*").receiver, cpg.call(".*println.*").lineNumber(44))
  }

  s"Basic$benchmarkNo line 45" should "report insecure" taggedAs Basic in {
    assertIsInsecure(cpg.call(".*getHeaderNames.*").receiver, cpg.call(".*println.*").lineNumber(45))
  }

  s"Basic$benchmarkNo line 46" should "report insecure" taggedAs Basic in {
    assertIsInsecure(cpg.call(".*getHeaderNames.*").receiver, cpg.call(".*println.*").lineNumber(46))
  }

  s"Basic$benchmarkNo line 47" should "report insecure" taggedAs Basic in {
    assertIsInsecure(cpg.call(".*getHeaderNames.*").receiver, cpg.call(".*println.*").lineNumber(47))
  }

}

class BasicBenchmark36 extends SecuribenchMicroBasicFixture(JAVA_EXT, 36) {

  s"Basic$benchmarkNo" should "report insecure" taggedAs Basic in {
    assertIsInsecure(cpg.call(".*getInputStream.*").receiver, cpg.call(".*println.*"))
  }

}

class BasicBenchmark37 extends SecuribenchMicroBasicFixture(JAVA_EXT, 37) {

  s"Basic$benchmarkNo" should "report insecure" taggedAs Basic in {
    assertIsInsecure(cpg.fieldAccess.code(".*FIELD_NAME.*"), cpg.call(".*println.*"))
  }

}

class BasicBenchmark38 extends SecuribenchMicroBasicFixture(JAVA_EXT, 38) {

  s"Basic$benchmarkNo line 45" should "report insecure" taggedAs Basic in {
    assertIsInsecure(cpg.fieldAccess.code(".*FIELD_NAME.*"), cpg.call(".*println.*").lineNumber(45))
  }

  s"Basic$benchmarkNo line 46" should "report secure" taggedAs Basic in {
    assertIsSecure(cpg.fieldAccess.code(".*FIELD_NAME.*"), cpg.call(".*println.*").lineNumber(46))
  }

}

class BasicBenchmark39 extends SecuribenchMicroBasicFixture(JAVA_EXT, 39) {

  s"Basic$benchmarkNo" should "report insecure" taggedAs Basic in {
    assertIsInsecure(cpg.fieldAccess.code(".*FIELD_NAME.*"), cpg.call(".*println.*"))
  }

}

class BasicBenchmark40 extends SecuribenchMicroBasicFixture(JAVA_EXT, 40) {

  s"Basic$benchmarkNo" should "report insecure" taggedAs Basic in {
    assertIsInsecure(cpg.fieldAccess.code(".*FIELD_NAME.*"), cpg.call(".*println.*"))
  }

}

class BasicBenchmark41 extends SecuribenchMicroBasicFixture(JAVA_EXT, 41) {

  s"Basic$benchmarkNo" should "report insecure" taggedAs Basic in {
    assertIsInsecure(cpg.literal.code(".*name.*"), cpg.call(".*println.*"))
  }

}

class BasicBenchmark42 extends SecuribenchMicroBasicFixture(JAVA_EXT, 42) {

  s"Basic$benchmarkNo" should "report insecure" taggedAs Basic in {
    assertIsInsecure(cpg.call(".*getInitParameterNames.*").receiver, cpg.call(".*println.*"))
  }

}
