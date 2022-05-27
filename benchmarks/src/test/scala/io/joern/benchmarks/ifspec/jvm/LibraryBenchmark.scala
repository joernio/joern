package io.joern.benchmarks.ifspec.jvm

import io.joern.benchmarks.BenchmarkTags.{ExplicitFlows, ImplicitFlows, Library}
import io.joern.benchmarks.testfixtures.BenchmarkFixture._
import io.joern.benchmarks.testfixtures.IfspecLibraryFixture
import io.shiftleft.semanticcpg.language._

// ImplicitListSizeLeak
class LibraryBenchmark1 extends IfspecLibraryFixture(JVM_EXT, 1) {

  s"Library$benchmarkNo" should "report insecure" taggedAs (Library, ImplicitFlows) in {
    assertIsInsecure(cpg.call(".*listSizeLeak.*").argument(1), cpg.method("listSizeLeak").methodReturn)
  }

}

// ImplicitListSizeNoLeak
class LibraryBenchmark2 extends IfspecLibraryFixture(JVM_EXT, 2) {

  s"Library$benchmarkNo" should "report secure" taggedAs (Library, ImplicitFlows) in {
    assertIsSecure(cpg.call(".*listSizeLeak.*").argument(1), cpg.method("listSizeLeak").methodReturn)
  }

}

// PasswordChecker
class LibraryBenchmark3 extends IfspecLibraryFixture(JVM_EXT, 3) {

  s"Library$benchmarkNo" should "report insecure" taggedAs (Library, ImplicitFlows) in {
    assertIsInsecure(cpg.call(".*nextLine.*").receiver, cpg.call(".*println.*").argument(0))
  }

}

// SimpleListSize
class LibraryBenchmark4 extends IfspecLibraryFixture(JVM_EXT, 4) {

  s"Library$benchmarkNo" should "report insecure" taggedAs (Library, ImplicitFlows) in {
    assertIsInsecure(cpg.call(".*listSizeLeak.*").argument(1), cpg.method("listSizeLeak").methodReturn)
  }

}

// SimpleListToArraySize
class LibraryBenchmark5 extends IfspecLibraryFixture(JVM_EXT, 5) {

  s"Library$benchmarkNo" should "report insecure" taggedAs (Library, ImplicitFlows) in {
    assertIsInsecure(cpg.call(".*listArraySizeLeak.*").argument, cpg.method("listArraySizeLeak").methodReturn)
  }

}

// StringIntern
class LibraryBenchmark6 extends IfspecLibraryFixture(JVM_EXT, 6) {

  s"Library$benchmarkNo" should "report insecure" taggedAs (Library, ExplicitFlows) in {
    assertIsInsecure(cpg.call(".*foo.*").argument, cpg.method("foo").methodReturn)
  }

}

// TimeBomb
class LibraryBenchmark7 extends IfspecLibraryFixture(JVM_EXT, 7) {

  s"Library$benchmarkNo" should "report secure" taggedAs (Library, ExplicitFlows) in {
    assertIsSecure(cpg.call(".*noLeak.*").argument, cpg.method("noLeak").methodReturn)
  }

}
