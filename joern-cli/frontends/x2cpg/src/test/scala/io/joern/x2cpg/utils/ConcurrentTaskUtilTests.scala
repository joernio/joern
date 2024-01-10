package io.joern.x2cpg.utils

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.util.Success

class ConcurrentTaskUtilTests extends AnyWordSpec with Matchers {

  "provide the means to let the caller handle unsuccessful operations without propagating an exception" in {
    val problem = Iterator(() => "Success!", () => "Success!", () => throw new RuntimeException("Failure!"))
    val result  = ConcurrentTaskUtil.runUsingThreadPool(problem)
    result.count {
      case Success(_) => true
      case _          => false
    } shouldBe 2
  }

}
