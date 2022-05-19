package io.joern.benchmarks.securibench.micro.jvm

import io.joern.benchmarks.BenchmarkTags.Collections
import io.joern.benchmarks.testfixtures.BenchmarkFixture._
import io.joern.benchmarks.testfixtures.SecuribenchMicroCollectionsFixture
import io.shiftleft.semanticcpg.language._

class CollectionsBenchmark1 extends SecuribenchMicroCollectionsFixture(JVM_EXT, "1") {

  s"Collections$benchmarkNo" should "report insecure" taggedAs Collections in {
    assertIsInsecure(cpg.literal(".*name.*"), cpg.call(".*println.*"))
  }

}

class CollectionsBenchmark2 extends SecuribenchMicroCollectionsFixture(JVM_EXT, "2") {

  s"Collections$benchmarkNo line 50" should "report insecure" taggedAs Collections in {
    assertIsInsecure(cpg.literal(".*name.*"), cpg.call(".*println.*").lineNumber(50))
  }

  s"Collections$benchmarkNo line 51" should "report secure" taggedAs Collections in {
    assertIsSecure(cpg.literal(".*name.*"), cpg.call(".*println.*").lineNumber(51))
  }

}

class CollectionsBenchmark3 extends SecuribenchMicroCollectionsFixture(JVM_EXT, "3") {

  s"Collections$benchmarkNo line 49" should "report insecure" taggedAs Collections in {
    assertIsInsecure(cpg.literal(".*name.*"), cpg.call(".*println.*").lineNumber(49))
  }

  s"Collections$benchmarkNo line 51" should "report insecure" taggedAs Collections in {
    assertIsInsecure(cpg.literal(".*name.*"), cpg.call(".*println.*").lineNumber(51))
  }

}

class CollectionsBenchmark4 extends SecuribenchMicroCollectionsFixture(JVM_EXT, "4") {

  s"Collections$benchmarkNo" should "report insecure" taggedAs Collections in {
    assertIsInsecure(cpg.literal(".*name.*"), cpg.call(".*println.*"))
  }

}

class CollectionsBenchmark5 extends SecuribenchMicroCollectionsFixture(JVM_EXT, "5") {

  s"Collections$benchmarkNo" should "report secure" taggedAs Collections in {
    assertIsSecure(cpg.literal(".*name.*"), cpg.call(".*println.*"))
  }

}

class CollectionsBenchmark6 extends SecuribenchMicroCollectionsFixture(JVM_EXT, "6") {

  s"Collections$benchmarkNo line 47" should "report secure" taggedAs Collections in {
    assertIsSecure(cpg.literal(".*name.*"), cpg.call(".*println.*").lineNumber(47))
  }

  s"Collections$benchmarkNo line 48" should "report insecure" taggedAs Collections in {
    assertIsInsecure(cpg.literal(".*name.*"), cpg.call(".*println.*").lineNumber(48))
  }

}

class CollectionsBenchmark7 extends SecuribenchMicroCollectionsFixture(JVM_EXT, "7") {

  s"Collections$benchmarkNo line 49" should "report secure" taggedAs Collections in {
    assertIsSecure(cpg.literal(".*name.*"), cpg.call(".*println.*").lineNumber(49))
  }

  s"Collections$benchmarkNo line 50" should "report insecure" taggedAs Collections in {
    assertIsInsecure(cpg.literal(".*name.*"), cpg.call(".*println.*").lineNumber(50))
  }

}

class CollectionsBenchmark8 extends SecuribenchMicroCollectionsFixture(JVM_EXT, "8") {

  s"Collections$benchmarkNo" should "report insecure" taggedAs Collections in {
    assertIsInsecure(cpg.literal(".*name.*"), cpg.call(".*println.*"))
  }

}

class CollectionsBenchmark9 extends SecuribenchMicroCollectionsFixture(JVM_EXT, "9") {

  s"Collections$benchmarkNo" should "report secure" taggedAs Collections in {
    assertIsSecure(cpg.literal(".*name.*"), cpg.call(".*println.*"))
  }

}

class CollectionsBenchmark10 extends SecuribenchMicroCollectionsFixture(JVM_EXT, "10") {

  s"Collections$benchmarkNo line 54" should "report insecure" taggedAs Collections in {
    assertIsInsecure(cpg.literal(".*name.*"), cpg.call(".*println.*").lineNumber(54))
  }

  s"Collections$benchmarkNo line 61" should "report secure" taggedAs Collections in {
    assertIsSecure(cpg.literal(".*name.*"), cpg.call(".*println.*").lineNumber(61))
  }

}

class CollectionsBenchmark11 extends SecuribenchMicroCollectionsFixture(JVM_EXT, "11") {

  s"Collections$benchmarkNo" should "report insecure" taggedAs Collections in {
    assertIsInsecure(cpg.literal(".*name.*"), cpg.call(".*foo.*"))
  }

}

class CollectionsBenchmark11b extends SecuribenchMicroCollectionsFixture(JVM_EXT, "11b") {

  s"Collections$benchmarkNo" should "report insecure" taggedAs Collections in {
    assertIsInsecure(cpg.identifier("o"), cpg.call(".*println.*"))
  }

}

class CollectionsBenchmark12 extends SecuribenchMicroCollectionsFixture(JVM_EXT, "12") {

  s"Collections$benchmarkNo" should "report insecure" taggedAs Collections in {
    assertIsInsecure(cpg.literal(".*name.*"), cpg.call(".*println.*"))
  }

}

class CollectionsBenchmark13 extends SecuribenchMicroCollectionsFixture(JVM_EXT, "13") {

  s"Collections$benchmarkNo line 52" should "report insecure" taggedAs Collections in {
    assertIsInsecure(cpg.literal(".*name.*"), cpg.call(".*println.*").lineNumber(52))
  }

  s"Collections$benchmarkNo line 53" should "report secure" taggedAs Collections in {
    assertIsSecure(cpg.literal(".*name.*"), cpg.call(".*println.*").lineNumber(53))
  }

  s"Collections$benchmarkNo line 54" should "report secure" taggedAs Collections in {
    assertIsSecure(cpg.literal(".*name.*"), cpg.call(".*println.*").lineNumber(54))
  }

}

class CollectionsBenchmark14 extends SecuribenchMicroCollectionsFixture(JVM_EXT, "14") {

  s"Collections$benchmarkNo" should "report insecure" taggedAs Collections in {
    assertIsInsecure(cpg.literal(".*name.*"), cpg.call(".*println.*"))
  }

}
