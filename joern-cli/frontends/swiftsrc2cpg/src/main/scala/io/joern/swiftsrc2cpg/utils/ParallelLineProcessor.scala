package io.joern.swiftsrc2cpg.utils

import org.slf4j.LoggerFactory

import java.io.BufferedReader
import java.util.concurrent.*
import java.util.concurrent.atomic.AtomicInteger

object ParallelLineProcessor {

  private val logger = LoggerFactory.getLogger(getClass)

  /** Process every line obtained from `reader.readLine()` (until it returns null), invoking `callback` on each line in
    * parallel using the given ExecutorService. Every line is filtered using the provided lineFilter function.
    *
    * Why a manually provided pool and manual task submission?
    *   - Pool ownership: an externally provided `ExecutorService` lets callers control parallelism, thread naming,
    *     instrumentation, and lifecycle, and enables reuse across multiple invocations without creating/tearing down
    *     threads per call.
    *   - Isolation: avoids relying on a global `ExecutionContext`/common pool, which can behave differently in IDEs,
    *     build tools, or native‑image.
    *   - Reader safety: a single producer thread performs all `readLine()` calls; tasks only run the callback, keeping
    *     the non‑thread‑safe reader untouched from worker threads.
    *   - Bounded state: does not build a list of `Future`s or buffer all lines; memory use is essentially the pool’s
    *     work queue plus in‑flight tasks. This helps to reduce the overall memory consumption for very large Json
    *     strings quite dramatically as we do not need to buffer all lines.
    *   - Deterministic completion: a latch and an in‑flight counter allow waiting for just the submitted work without
    *     shutting down the pool or depending on `awaitTermination`, leaving pool management to the caller.
    *   - Robustness: exceptions in callbacks are caught and logged so the producer continues and the method still waits
    *     for all scheduled work.
    *
    * Guarantees:
    *   - The underlying reader is only accessed by the calling thread (so a non‑thread‑safe reader is fine).
    *   - The callback for different lines may run concurrently.
    *   - The method returns only after all callbacks have completed (successfully or with exceptions).
    *
    * Error handling:
    *   - logging as warn
    */
  def processLinesParallel(reader: BufferedReader, pool: ExecutorService, lineFilter: String => Boolean)(
    callback: String => Unit
  ): Unit = {
    // Track outstanding tasks
    val activeTasks           = new AtomicInteger(0)
    val doneLatch             = new CountDownLatch(1)
    @volatile var readingDone = false

    def submit(line: String): Unit = {
      activeTasks.incrementAndGet()
      pool.execute(() => {
        try {
          callback(line)
        } catch {
          case t: Throwable => logger.warn("Reading Json failed", t)
        } finally {
          if (activeTasks.decrementAndGet() == 0 && readingDone) {
            doneLatch.countDown()
          }
        }
      })
    }

    // Producer loop: read lines sequentially, submit work
    Iterator
      .continually(reader.readLine())
      .takeWhile(_ != null)
      .filter(lineFilter)
      .foreach(submit)

    // Signal no more submissions
    readingDone = true
    if (activeTasks.get() == 0) {
      // Edge case: empty input or all tasks already finished
      doneLatch.countDown()
    }

    // Wait for all tasks
    doneLatch.await()
  }
}
