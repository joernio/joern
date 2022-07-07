package io.joern.benchmarks.ifspec.jvm

import io.joern.benchmarks.BenchmarkTags.{Arrays, Exceptions, ExplicitFlows, ImplicitFlows}
import io.joern.benchmarks.testfixtures.BenchmarkFixture._
import io.joern.benchmarks.testfixtures.IfspecArraysFixture
import io.shiftleft.semanticcpg.language._

// ArrayCopyDirectLeak
class ArraysBenchmark1 extends IfspecArraysFixture(JVM_EXT, 1) {

  s"Arrays$benchmarkNo" should "report insecure" taggedAs (Arrays, ExplicitFlows) in {
    assertIsInsecure(cpg.call(".*f.*").argumentIndex(1, 3), cpg.method("f").methodReturn)
  }

}

// ArrayIndexExceptionInsecure
class ArraysBenchmark2 extends IfspecArraysFixture(JVM_EXT, 2) {

  s"Arrays$benchmarkNo" should "report insecure" taggedAs (Arrays, ImplicitFlows, Exceptions) in {
    assertIsInsecure(cpg.fieldAccess.code(".*secret"), cpg.call(".*println.*").argument(1))
  }

}

// ArrayIndexExceptionSecure
class ArraysBenchmark3 extends IfspecArraysFixture(JVM_EXT, 3) {

  s"Arrays$benchmarkNo" should "report secure" taggedAs (Arrays, ImplicitFlows, Exceptions) in {
    assertIsSecure(cpg.fieldAccess.code(".*secret"), cpg.call(".*println.*").argument(1))
  }

}

// ArrayIndexSensitivitySecure
class ArraysBenchmark4 extends IfspecArraysFixture(JVM_EXT, 4) {

  s"Arrays$benchmarkNo" should "report secure" taggedAs (Arrays, ExplicitFlows) in {
    assertIsSecure(cpg.method("foo").parameter, cpg.method("foo").methodReturn)
  }

}

// ArraysImplicitLeakInsecure
class ArraysBenchmark5 extends IfspecArraysFixture(JVM_EXT, 5) {

  s"Arrays$benchmarkNo" should "report insecure" taggedAs (Arrays, ImplicitFlows) in {
    assertIsInsecure(cpg.fieldAccess.code(".*secret"), cpg.call(".*println.*").argument(1))
  }

}

// ArraysImplicitLeakSecure
class ArraysBenchmark6 extends IfspecArraysFixture(JVM_EXT, 6) {

  s"Arrays$benchmarkNo" should "report secure" taggedAs (Arrays, ImplicitFlows) in {
    assertIsSecure(cpg.fieldAccess.code(".*secret"), cpg.call(".*println.*").argument(1))
  }

}

// ArraySizeStrongUpdate
class ArraysBenchmark7 extends IfspecArraysFixture(JVM_EXT, 7) {

  s"Arrays$benchmarkNo" should "report secure" taggedAs (Arrays, ExplicitFlows) in {
    assertIsSecure(cpg.fieldAccess.code(".*secret"), cpg.call(".*println.*").argument(1))
  }

}

// SimpleArraySize
class ArraysBenchmark8 extends IfspecArraysFixture(JVM_EXT, 8) {

  s"Arrays$benchmarkNo" should "report insecure" taggedAs (Arrays, ExplicitFlows) in {
    assertIsInsecure(cpg.call(".*arraySizeLeak.*").argument(1), cpg.method("arraySizeLeak").methodReturn)
  }

}

// Webstore1
class ArraysBenchmark9 extends IfspecArraysFixture(JVM_EXT, 9) {

  s"Arrays$benchmarkNo" should "report secure" taggedAs (Arrays, ExplicitFlows) in {
    assertIsSecure(cpg.call(".*buyProduct.*").argument, cpg.method("buyProduct").methodReturn)
  }

}

// Webstore2
class ArraysBenchmark10 extends IfspecArraysFixture(JVM_EXT, 10) {

  s"Arrays$benchmarkNo" should "report secure" taggedAs (Arrays, ExplicitFlows) in {
    assertIsSecure(cpg.fieldAccess.code(".*vPrime.*"), cpg.method("seePreview").methodReturn)
  }

}
