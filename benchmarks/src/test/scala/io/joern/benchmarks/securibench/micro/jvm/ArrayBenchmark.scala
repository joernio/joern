package io.joern.benchmarks.securibench.micro.jvm

import io.joern.benchmarks.BenchmarkTags.Arrays
import io.joern.benchmarks.testfixtures.BenchmarkFixture._
import io.joern.benchmarks.testfixtures.SecuribenchMicroArraysFixture
import io.shiftleft.semanticcpg.language._

class ArraysBenchmark1 extends SecuribenchMicroArraysFixture(JVM_EXT, 1) {

  s"Arrays$benchmarkNo" should "report insecure" taggedAs Arrays in {
    assertIsInsecure(cpg.literal.code(".*name.*"), cpg.call(".*println.*"))
  }

}

class ArraysBenchmark2 extends SecuribenchMicroArraysFixture(JVM_EXT, 2) {

  s"Arrays$benchmarkNo line 42" should "report insecure" taggedAs Arrays in {
    assertIsInsecure(cpg.literal.code(".*name.*"), cpg.call(".*println.*").lineNumber(42))
  }

  s"Arrays$benchmarkNo line 43" should "report secure" taggedAs Arrays in {
    assertIsSecure(cpg.literal.code(".*name.*"), cpg.call(".*println.*").lineNumber(43))
  }

  s"Arrays$benchmarkNo line 44" should "report secure" taggedAs Arrays in {
    assertIsSecure(cpg.literal.code(".*name.*"), cpg.call(".*println.*").lineNumber(44))
  }

}

class ArraysBenchmark3 extends SecuribenchMicroArraysFixture(JVM_EXT, 3) {

  s"Arrays$benchmarkNo line 45" should "report insecure" taggedAs Arrays in {
    assertIsInsecure(cpg.literal.code(".*name.*"), cpg.call(".*println.*").lineNumber(45))
  }

  s"Arrays$benchmarkNo line 46" should "report secure" taggedAs Arrays in {
    assertIsSecure(cpg.literal.code(".*name.*"), cpg.call(".*println.*").lineNumber(46))
  }

}

class ArraysBenchmark4 extends SecuribenchMicroArraysFixture(JVM_EXT, 4) {

  s"Arrays$benchmarkNo" should "report insecure" taggedAs Arrays in {
    assertIsInsecure(cpg.literal.code(".*name.*"), cpg.call(".*println.*"))
  }

}

class ArraysBenchmark5 extends SecuribenchMicroArraysFixture(JVM_EXT, 5) {

  s"Arrays$benchmarkNo" should "report secure" taggedAs Arrays in {
    assertIsSecure(cpg.literal.code(".*name.*"), cpg.call(".*println.*"))
  }

}

class ArraysBenchmark6 extends SecuribenchMicroArraysFixture(JVM_EXT, 6) {

  s"Arrays$benchmarkNo" should "report insecure" taggedAs Arrays in {
    assertIsInsecure(cpg.literal.code(".*name.*"), cpg.call(".*println.*"))
  }

}

class ArraysBenchmark7 extends SecuribenchMicroArraysFixture(JVM_EXT, 7) {

  s"Arrays$benchmarkNo" should "report insecure" taggedAs Arrays in {
    assertIsInsecure(cpg.literal.code(".*name.*"), cpg.call(".*println.*"))
  }

}

class ArraysBenchmark8 extends SecuribenchMicroArraysFixture(JVM_EXT, 8) {

  s"Arrays$benchmarkNo line 41" should "report insecure" taggedAs Arrays in {
    assertIsInsecure(cpg.literal.code(".*name.*"), cpg.call(".*println.*").lineNumber(41))
  }

  s"Arrays$benchmarkNo line 42" should "report secure" taggedAs Arrays in {
    assertIsSecure(cpg.literal.code(".*name.*"), cpg.call(".*println.*").lineNumber(42))
  }

}

class ArraysBenchmark9 extends SecuribenchMicroArraysFixture(JVM_EXT, 9) {

  s"Arrays$benchmarkNo" should "report insecure" taggedAs Arrays in {
    assertIsInsecure(cpg.literal.code(".*name.*"), cpg.call(".*println.*"))
  }

}

class ArraysBenchmark10 extends SecuribenchMicroArraysFixture(JVM_EXT, 10) {

  s"Arrays$benchmarkNo line 42" should "report insecure" taggedAs Arrays in {
    assertIsInsecure(cpg.literal.code(".*name.*"), cpg.call(".*println.*").lineNumber(42))
  }

  s"Arrays$benchmarkNo line 43" should "report secure" taggedAs Arrays in {
    assertIsSecure(cpg.literal.code(".*name.*"), cpg.call(".*println.*").lineNumber(43))
  }

}
