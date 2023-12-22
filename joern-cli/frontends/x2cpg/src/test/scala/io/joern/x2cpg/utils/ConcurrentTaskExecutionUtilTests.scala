package io.joern.x2cpg.utils

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.util.Success

class ConcurrentTaskExecutionUtilTests extends AnyWordSpec with Matchers {

  "a large number of 'expensive' operations should perform faster in parallel" in {
    def problem = Iterator.fill(100)(() => Thread.sleep(10))

    val parStart = System.nanoTime()
    ConcurrentTaskExecutionUtil.runInParallel(problem)
    val parTotal = System.nanoTime() - parStart

    val serStart = System.nanoTime()
    problem.foreach(x => x())
    val serTotal = System.nanoTime() - serStart

    parTotal should be < serTotal
  }

  "provide the means to let the caller handle unsuccessful operations without propagating an exception" in {
    val problem = Iterator(() => "Success!", () => "Success!", () => throw new RuntimeException("Failure!"))
    val result  = ConcurrentTaskExecutionUtil.runInParallel(problem)
    result.count {
      case Success(_) => true
      case _          => false
    } shouldBe 2
  }

}
