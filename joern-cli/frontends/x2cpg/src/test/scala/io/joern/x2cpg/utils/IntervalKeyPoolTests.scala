package io.joern.x2cpg.utils

import org.scalatest.Inside
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class IntervalKeyPoolTests extends AnyWordSpec with Matchers with Inside {

  "IntervalKeyPool" should {
    "return [first, ..., last] and then raise" in {
      val keyPool = new IntervalKeyPool(10, 19)
      List.range(0, 10).map(_ => keyPool.next) shouldBe List.range(10, 20)

      val caughtException1 = intercept[RuntimeException] { keyPool.next }
      caughtException1.getMessage shouldBe s"Pool exhausted: next 20 > last 19"

      val caughtException2 = intercept[RuntimeException] { keyPool.next }
      caughtException2.getMessage shouldBe s"Pool exhausted: next 21 > last 19"
    }

  }

}
