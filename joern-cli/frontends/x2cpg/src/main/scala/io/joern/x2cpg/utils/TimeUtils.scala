package io.joern.x2cpg.utils

import java.util.Locale
import scala.concurrent.*
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.*
import scala.language.postfixOps
import scala.util.Try

object TimeUtils {

  /** Measures elapsed time for executing a block in nanoseconds */
  def time[R](block: => R): (R, Long) = {
    val t0      = System.nanoTime()
    val result  = block
    val t1      = System.nanoTime()
    val elapsed = t1 - t0
    (result, elapsed)
  }

  /** Selects most appropriate TimeUnit for given duration and formats it accordingly */
  def pretty(duration: Long): String = pretty(Duration.fromNanos(duration))

  def runWithTimeout[T](timeoutMs: Long)(f: => T): Try[T] = {
    Try(Await.result(Future(f), timeoutMs milliseconds))
  }

  private def pretty(duration: Duration): String =
    duration match {
      case d: FiniteDuration =>
        val nanos = d.toNanos
        val unit  = chooseUnit(nanos)
        val value = nanos.toDouble / NANOSECONDS.convert(1, unit)

        s"%.4g %s".formatLocal(Locale.ROOT, value, abbreviate(unit))

      case Duration.MinusInf => s"-∞ (minus infinity)"
      case Duration.Inf      => s"∞ (infinity)"
      case _                 => "undefined"
    }

  private def chooseUnit(nanos: Long): TimeUnit = {
    val d = nanos.nanos

    if (d.toDays > 0) DAYS
    else if (d.toHours > 0) HOURS
    else if (d.toMinutes > 0) MINUTES
    else if (d.toSeconds > 0) SECONDS
    else if (d.toMillis > 0) MILLISECONDS
    else if (d.toMicros > 0) MICROSECONDS
    else NANOSECONDS
  }

  private def abbreviate(unit: TimeUnit): String =
    unit match {
      case NANOSECONDS  => "ns"
      case MICROSECONDS => "μs"
      case MILLISECONDS => "ms"
      case SECONDS      => "s"
      case MINUTES      => "min"
      case HOURS        => "h"
      case DAYS         => "d"
    }

}
