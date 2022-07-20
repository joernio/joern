package io.joern.benchmarks.ifspec.jvm

import io.joern.benchmarks.BenchmarkTags.{Exceptions, ImplicitFlows}
import io.joern.benchmarks.testfixtures.BenchmarkFixture._
import io.joern.benchmarks.testfixtures.IfspecExceptionsFixture
import io.shiftleft.semanticcpg.language._

// ConditionalLeakage
class ExceptionsBenchmark1 extends IfspecExceptionsFixture(JVM_EXT, 1) {

  s"Exceptions$benchmarkNo" should "report insecure" taggedAs (Exceptions, ImplicitFlows) in {
    assertIsInsecure(cpg.call(".*divide.*").argument(2), cpg.method(".*divide.*").call(".*println.*").argument(1))
  }

}

// ExceptionalControlFlow1Insecure
class ExceptionsBenchmark2 extends IfspecExceptionsFixture(JVM_EXT, 2) {

  s"Exceptions$benchmarkNo" should "report insecure" taggedAs (Exceptions, ImplicitFlows) in {
    assertIsInsecure(cpg.call(".*foo.*").argument(1), cpg.method("foo").methodReturn)
  }

}

// ExceptionalControlFlow1Secure
class ExceptionsBenchmark3 extends IfspecExceptionsFixture(JVM_EXT, 3) {

  s"Exceptions$benchmarkNo" should "report secure" taggedAs (Exceptions, ImplicitFlows) in {
    assertIsSecure(cpg.call(".*foo.*").argument(1), cpg.method("foo").methodReturn)
  }

}

// ExceptionalControlFlow2Secure
class ExceptionsBenchmark4 extends IfspecExceptionsFixture(JVM_EXT, 4) {

  s"Exceptions$benchmarkNo" should "report secure" taggedAs (Exceptions, ImplicitFlows) in {
    assertIsSecure(cpg.call(".*foo.*").argument(1), cpg.method("foo").methodReturn)
  }

}

// ExceptionDivZero
class ExceptionsBenchmark5 extends IfspecExceptionsFixture(JVM_EXT, 5) {

  s"Exceptions$benchmarkNo" should "report insecure" taggedAs (Exceptions, ImplicitFlows) in {
    assertIsInsecure(
      cpg.call(".*nextInt.*").receiver,
      cpg.call.methodFullName(".*writeToDisk.*", ".*writeToDB.*").argument(1)
    )
  }

}

// ExceptionHandling
class ExceptionsBenchmark6 extends IfspecExceptionsFixture(JVM_EXT, 6) {

  s"Exceptions$benchmarkNo" should "report insecure" taggedAs (Exceptions, ImplicitFlows) in {
    assertIsInsecure(cpg.call(".*f.*").argument(1), cpg.method("f").methodReturn)
  }

}
