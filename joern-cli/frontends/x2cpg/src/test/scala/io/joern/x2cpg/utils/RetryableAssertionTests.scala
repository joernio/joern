package io.joern.x2cpg.utils

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import RetryableAssertion.*
import org.scalatest.Assertion

import scala.concurrent.duration.*

class RetryableAssertionTests extends AnyWordSpec with Matchers {

  "Eventually assertion" should {

    "retry until condition is met" in {
      var counter = 0

      eventually[Assertion, Throwable] {
        counter += 1
        counter should be > 2
      }
    }

    "work with custom retry config" in {
      var attempts = 0

      implicit val customConfig: RetryConfig =
        RetryConfig(maxRetries = 5, retryDelay = 50.millis, timeout = 2.seconds, backoffMultiplier = 1.5)

      eventually[Assertion, Throwable] {
        attempts += 1
        attempts should be > 3
      }
    }

    // intentionally ignored to avoid flaky test runs
    "retry flaky API calls" ignore {
      def flakyApiCall(): String = {
        // Simulate flaky API
        if (scala.util.Random.nextDouble() > 0.7) "success"
        else throw new RuntimeException("API temporarily unavailable")
      }

      implicit val apiConfig: RetryConfig =
        RetryConfig(maxRetries = 10, retryDelay = 200.millis, timeout = 5.seconds, backoffMultiplier = 1.2)

      eventually[Assertion, RuntimeException] {
        flakyApiCall() shouldBe "success"
      }
    }

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

      eventually[Assertion, Throwable] {
        items should have length 5
        items should contain allOf (1, 2, 3, 4, 5)
      }
    }

    "only specified exception type triggers retries" in {
      // retry on IllegalStateException, shows that only specified exception type triggers retries
      var attempts = 0
      eventually[Assertion, IllegalStateException] {
        attempts += 1
        if (attempts < 3) throw new IllegalStateException("retry-me")
        attempts shouldBe 3
      }

      // other exception types should be rethrown immediately
      val ex = intercept[IllegalArgumentException] {
        eventually[Assertion, IllegalStateException] {
          throw new IllegalArgumentException("do-not-retry")
        }
      }
      ex.getMessage should include("do-not-retry")
    }
  }
}
