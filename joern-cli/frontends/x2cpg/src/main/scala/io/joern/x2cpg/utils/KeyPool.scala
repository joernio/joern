package io.joern.x2cpg.utils

import java.util.concurrent.atomic.{AtomicInteger, AtomicLong}

/** A pool of long integers. Using the method `next`, the pool provides the next id in a thread-safe manner. */
trait KeyPool {
  def next: Long
}

/** A key pool that returns the integers of the interval [first, last] in a thread-safe manner.
  */
class IntervalKeyPool(val first: Long, val last: Long) extends KeyPool {

  /** Get next number in interval or raise if number is larger than `last`
    */
  def next: Long = {
    if (!valid) {
      throw new IllegalStateException("Call to `next` on invalidated IntervalKeyPool.")
    }
    val n = cur.incrementAndGet()
    if (n > last) {
      throw new RuntimeException("Pool exhausted")
    } else {
      n
    }
  }

  /** Split key pool into `numberOfPartitions` partitions of mostly equal size. Invalidates the current pool to ensure
    * that the user does not continue to use both the original pool and pools derived from it via `split`.
    */
  def split(numberOfPartitions: Int): Iterator[IntervalKeyPool] = {
    valid = false
    if (numberOfPartitions == 0) {
      Iterator()
    } else {
      val curFirst = cur.get()
      val k        = (last - curFirst) / numberOfPartitions
      (1 to numberOfPartitions).map { i =>
        val poolFirst = curFirst + (i - 1) * k
        new IntervalKeyPool(poolFirst, poolFirst + k - 1)
      }.iterator
    }
  }

  private val cur: AtomicLong = new AtomicLong(first - 1)
  private var valid: Boolean  = true
}

/** A key pool that returns elements of `seq` in order in a thread-safe manner.
  */
class SequenceKeyPool(seq: Seq[Long]) extends KeyPool {

  val seqLen: Int = seq.size
  var cur         = new AtomicInteger(-1)

  override def next: Long = {
    val i = cur.incrementAndGet()
    if (i >= seqLen) {
      throw new RuntimeException("Pool exhausted")
    } else {
      seq(i)
    }
  }
}

object KeyPoolCreator {

  /** Divide the keyspace into n intervals and return a list of corresponding key pools.
    */
  def obtain(n: Long, minValue: Long = 0, maxValue: Long = Long.MaxValue): List[IntervalKeyPool] = {
    val nIntervals        = Math.max(n, 1)
    val intervalLen: Long = (maxValue - minValue) / nIntervals
    List.range(0L, nIntervals).map { i =>
      val first = i * intervalLen + minValue
      val last  = first + intervalLen - 1
      new IntervalKeyPool(first, last)
    }
  }

}
