package io.joern.x2cpg.utils

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import RetryableAssertion._
import scala.concurrent.duration._

class RetryableAssertionTests extends AnyWordSpec with Matchers {

  "Eventually assertion" should {

    // Example 1: Basic usage with default config
    "retry until condition is met" in {
      var counter = 0

      eventually {
        counter += 1
        counter should be > 2
      }
    }

    // Example 2: Custom configuration
    "work with custom retry config" in {
      var attempts = 0

      implicit val customConfig: RetryConfig =
        RetryConfig(maxRetries = 5, retryDelay = 50.millis, timeout = 2.seconds, backoffMultiplier = 1.5)

      eventually {
        attempts += 1
        attempts should be > 3
      }
    }

    // Example 3: Using shouldEventually syntax
    "work with shouldEventually syntax" in {
      var state = "initializing"

      // Simulate async state change
      new Thread(() => {
        Thread.sleep(300)
        state = "ready"
      }).start()

      implicit val config: RetryConfig = RetryConfig(maxRetries = 10, retryDelay = 100.millis, timeout = 3.seconds)

      state shouldEventually { s =>
        s should be("ready")
      }
    }

    // Example 4: Testing API responses with retry
    // intentionally ignored to avoid flaky test runs
    "retry flaky API calls" ignore {
      def flakyApiCall(): String = {
        // Simulate flaky API
        if (scala.util.Random.nextDouble() > 0.7) "success"
        else throw new RuntimeException("API temporarily unavailable")
      }

      implicit val apiConfig: RetryConfig =
        RetryConfig(maxRetries = 10, retryDelay = 200.millis, timeout = 5.seconds, backoffMultiplier = 1.2)

      eventually {
        flakyApiCall() shouldBe "success"
      }
    }

    // Example 5: Eventually with complex assertions
    "support complex ScalaTest matchers" in {
      var items = List.empty[Int]

      // Simulate async list population
      new Thread(() => {
        (1 to 5).foreach { i =>
          Thread.sleep(100)
          items = items :+ i
        }
      }).start()

      implicit val config: RetryConfig = RetryConfig(maxRetries = 20, retryDelay = 100.millis, timeout = 3.seconds)

      eventually {
        items should have length 5
        items should contain allOf (1, 2, 3, 4, 5)
      }
    }
  }
}
