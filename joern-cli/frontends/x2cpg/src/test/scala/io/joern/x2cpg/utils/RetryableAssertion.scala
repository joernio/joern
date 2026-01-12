package io.joern.x2cpg.utils

import org.scalatest.Assertion

import scala.annotation.tailrec
import scala.concurrent.duration.*
import scala.util.{Failure, Success, Try}

/** Utility providing retrying assertions with timeout and optional exponential backoff.
  *
  * Use `eventually` to execute an assertion block repeatedly until it succeeds or a configured timeout / retry limit is
  * reached. Configuration is provided via `RetryConfig`. Also adds the `shouldEventually` syntax for concise assertions
  * against values inside tests.
  *
  * @example
  *
  * {{{
  *   // Quick usage with defaults
  *   eventually {
  *     someValue should be > 10
  *   }
  *
  *   // Custom configuration
  *   implicit val config = RetryConfig(
  *     maxRetries = 5,
  *     retryDelay = 100.millis,
  *     timeout = 3.seconds,
  *     backoffMultiplier = 1.5
  *   )
  *
  *   eventually {
  *     asyncOperation() shouldBe "expected"
  *   }
  *
  *   // Alternative syntax
  *   value shouldEventually { v =>
  *     v should be("ready")
  *   }
  * }}}
  */
object RetryableAssertion {

  /** Configuration for retry and timeout behavior
    *
    * @param maxRetries
    *   Maximum number of retry attempts (default: 3)
    * @param retryDelay
    *   Delay between retry attempts (default: 100ms)
    * @param timeout
    *   Maximum time to wait for assertion to pass (default: 5 seconds)
    * @param backoffMultiplier
    *   Multiplier for exponential backoff (default: 1.0 for no backoff)
    */
  case class RetryConfig(
    maxRetries: Int = 3,
    retryDelay: FiniteDuration = 100.millis,
    timeout: FiniteDuration = 5.seconds,
    backoffMultiplier: Double = 1.0
  )

  /** Execute an assertion with retry and timeout support
    *
    * @param assertion
    *   The assertion block to execute
    * @param config
    *   Retry configuration
    * @return
    *   The assertion result if successful
    * @throws AssertionError
    *   if all retries are exhausted or timeout is reached
    */
  def eventually[T](assertion: => T)(implicit config: RetryConfig = RetryConfig()): T = {
    val startTime = System.currentTimeMillis()
    val timeoutMs = config.timeout.toMillis

    @tailrec
    def attempt(attemptNumber: Int, currentDelay: FiniteDuration): T = {
      val elapsed = System.currentTimeMillis() - startTime

      if (elapsed >= timeoutMs) {
        throw new AssertionError(s"Assertion timed out after ${config.timeout}. Attempted $attemptNumber times.")
      }

      Try(assertion) match {
        case Success(result) =>
          result
        case Failure(error) if attemptNumber < config.maxRetries =>
          if (elapsed + currentDelay.toMillis >= timeoutMs) {
            throw new AssertionError(
              s"Assertion failed after $attemptNumber attempts and ${elapsed}ms. Last error: ${error.getMessage}",
              error
            )
          }
          // Wait before retrying
          Thread.sleep(currentDelay.toMillis)
          // Calculate next delay with backoff
          val nextDelay = if (config.backoffMultiplier > 1.0) {
            (currentDelay.toMillis * config.backoffMultiplier).toLong.millis
          } else {
            currentDelay
          }
          attempt(attemptNumber + 1, nextDelay)
        case Failure(error) =>
          throw new AssertionError(
            s"Assertion failed after ${config.maxRetries} retries and ${elapsed}ms. Last error: ${error.getMessage}",
            error
          )
      }
    }
    attempt(1, config.retryDelay)
  }

  /** Implicit class to add retry syntax to any value
    */
  implicit class RetryableValue[T](value: => T) {
    def shouldEventually(assertion: T => Assertion)(implicit config: RetryConfig = RetryConfig()): Assertion = {
      eventually(assertion(value))(config)
    }
  }
}
