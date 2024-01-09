package io.joern.x2cpg.utils

import org.scalatest.Assertions
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.util.Success

class ConcurrentTaskUtilTests extends AnyWordSpec with Matchers {

  private def assumeMultipleProcessors =
    assume(Runtime.getRuntime.availableProcessors() > 1, "!!! Number of available processors not larger than 1 !!!")

  "compared to serial execution, concurrent execution" should {

    "perform better against a large number of 'expensive' operations using a spliterator" in {
      assumeMultipleProcessors
      def problem = Iterator.fill(500)(() => Thread.sleep(10))

      val parStart = System.nanoTime()
      ConcurrentTaskUtil.runUsingSpliterator(problem)
      val parTotal = System.nanoTime() - parStart

      val serStart = System.nanoTime()
      problem.foreach(x => x())
      val serTotal = System.nanoTime() - serStart

      parTotal should be < serTotal
    }

    "perform better against a large number of 'cheap' operations using a thread pool" in {
      assumeMultipleProcessors
      def problem = Iterator.fill(500)(() => Thread.sleep(1))

      val parStart = System.nanoTime()
      ConcurrentTaskUtil.runUsingThreadPool(problem)
      val parTotal = System.nanoTime() - parStart

      val serStart = System.nanoTime()
      problem.foreach(x => x())
      val serTotal = System.nanoTime() - serStart

      parTotal should be < serTotal
    }
  }

  "provide the means to let the caller handle unsuccessful operations without propagating an exception" in {
    val problem = Iterator(() => "Success!", () => "Success!", () => throw new RuntimeException("Failure!"))
    val result  = ConcurrentTaskUtil.runUsingThreadPool(problem)
    result.count {
      case Success(_) => true
      case _          => false
    } shouldBe 2
  }

}
