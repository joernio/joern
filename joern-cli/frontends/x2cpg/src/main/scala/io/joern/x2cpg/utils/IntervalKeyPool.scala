package io.joern.x2cpg.utils

import java.util.concurrent.atomic.AtomicLong

/** A key pool that returns the integers of the interval [first, last] in a thread-safe manner. */
class IntervalKeyPool(val first: Long, val last: Long) {
  private val cur: AtomicLong = new AtomicLong(first - 1)

  /** Get next number in interval or raise if number is larger than `last` */
  def next: Long = {
    val nextNumber = cur.incrementAndGet()
    if (nextNumber > last) {
      throw new RuntimeException(s"Pool exhausted: next ${nextNumber} > last ${last}")
    } else {
      nextNumber
    }
  }

}
