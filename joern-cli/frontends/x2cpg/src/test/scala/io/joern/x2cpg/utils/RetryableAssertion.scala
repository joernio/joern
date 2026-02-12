package io.joern.x2cpg.utils

import scala.annotation.tailrec
import scala.concurrent.duration.*
import scala.util.{Failure, Success, Try}

/** Provides a test-focused retry helper for flaky or asynchronous assertions.
  *
  * `eventually` Repeatedly evaluates the `assertion` until it returns successfully, a non-retryable throwable is thrown
  * (rethrown immediately), or the configured timeout / retry limit is reached. The type parameter `E` controls which
  * throwable types are considered retryable (checked at runtime via the provided `ClassTag[E]`).
  *
  * Semantics:
  *   - Retryable exceptions (instances of `E`) cause the helper to wait `config.retryDelay` between attempts and
  *     optionally apply `config.backoffMultiplier` to the delay. Retries stop when `config.maxRetries` is exceeded or
  *     when `config.timeout` elapses.
  *   - Non-retryable throwable is rethrown immediately.
  *   - When retries are exhausted or a timeout occurs, an `AssertionError` is thrown that includes elapsed time in
  *     milliseconds and the last error message.
  *
  * Typical use:
  *   - When using ScalaTest matchers inside the assertion block, use `org.scalatest.Assertion` as the `T` result type.
  *
  * @example
  *
  * {{{
  *   // Quick usage with defaults
  *   eventually[Assertion, Throwable] {
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
  *   eventually[Assertion, Throwable] {
  *     flakyOperation() shouldBe "expected"
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

  /** Repeatedly evaluates the given `assertion` until it succeeds or the configured timeout / retry limit is reached.
    *
    * The method will retry on exceptions of type `E` up to `config.maxRetries` times, waiting `config.retryDelay`
    * between attempts and applying `config.backoffMultiplier` for exponential backoff if > 1.0. If a non-retryable
    * throwable is thrown, it is immediately rethrown.
    *
    * @tparam T
    *   result type of the assertion block
    * @tparam E
    *   throwable type that should be treated as retryable
    * @param assertion
    *   by-name assertion to execute
    * @param ct
    *   implicit ClassTag for `E` used to check retryable exceptions
    * @param config
    *   implicit RetryConfig controlling delays, timeout and retry behavior (default: `RetryConfig()`)
    * @return
    *   the successful result of `assertion`
    * @throws AssertionError
    *   if the assertion does not succeed within the configured timeout or retry limit
    * @throws Throwable
    *   any non-retryable exception thrown by `assertion` is rethrown unchanged
    */
  def eventually[T, E <: Throwable](
    assertion: => T
  )(implicit ct: scala.reflect.ClassTag[E], config: RetryConfig = RetryConfig()): T = {
    val startTime = System.nanoTime()
    val timeoutNs = config.timeout.toNanos

    @tailrec
    def attempt(attemptNumber: Int, currentDelay: FiniteDuration): T = {
      Try(assertion) match {
        case Success(result) =>
          result
        // If the Failure contains an exception of type E, treat it as retryable
        case Failure(error) if ct.runtimeClass.isInstance(error) && attemptNumber <= config.maxRetries =>
          val typed = error.asInstanceOf[E]

          val elapsed   = System.nanoTime() - startTime
          val elapsedMs = (currentDelay.toNanos + elapsed) / 1000000

          if (elapsed + currentDelay.toNanos >= timeoutNs) {
            throw new AssertionError(
              s"Assertion failed after $attemptNumber attempts and ${elapsedMs}ms. Last error: ${typed.getMessage}",
              typed
            )
          }

          // Wait before retrying
          Thread.sleep(currentDelay.toMillis)

          // Calculate next delay with backoff
          val nextDelay = if (config.backoffMultiplier > 1.0) {
            (currentDelay.toNanos * config.backoffMultiplier).toLong.nanos
          } else {
            currentDelay
          }
          attempt(attemptNumber + 1, nextDelay)
        // If it's a Failure with an exception of type E but no retries left, wrap in AssertionError
        case Failure(error) if ct.runtimeClass.isInstance(error) =>
          val typed = error.asInstanceOf[E]

          val elapsed   = System.nanoTime() - startTime
          val elapsedMs = (currentDelay.toNanos + elapsed) / 1000000

          throw new AssertionError(
            s"Assertion failed after ${config.maxRetries} retries and ${elapsedMs}ms. Last error: ${typed.getMessage}",
            typed
          )
        // Any other throwable type should be rethrown immediately
        case Failure(other) =>
          throw other
      }
    }
    attempt(1, config.retryDelay)
  }

}
