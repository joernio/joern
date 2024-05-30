package io.joern.x2cpg.utils

import java.util
import java.util.concurrent.{Callable, Executors}
import java.util.stream.{Collectors, StreamSupport}
import java.util.{Collections, Spliterator, Spliterators}
import scala.jdk.CollectionConverters.*
import scala.util.Try

/** A utility for providing out-of-the-box basic concurrent execution for a collection of Scala functions.
  */
object ConcurrentTaskUtil {

  private val MAX_POOL_SIZE = Runtime.getRuntime.availableProcessors()

  /** Uses a thread pool with a limited number of active threads executing a task at any given point. This is effective
    * when tasks may require large amounts of memory, or single tasks are too short lived.
    *
    * @param tasks
    *   the tasks to parallelize.
    * @param maxPoolSize
    *   the max pool size to allow for active threads.
    * @tparam V
    *   the output type of each task.
    * @return
    *   an array of the executed tasks as either a success or failure.
    */
  def runUsingThreadPool[V](tasks: Iterator[() => V], maxPoolSize: Int = MAX_POOL_SIZE): List[Try[V]] = {
    val ex = Executors.newFixedThreadPool(maxPoolSize)
    try {
      val callables = Collections.list(tasks.map { x =>
        new Callable[V] {
          override def call(): V = x.apply()
        }
      }.asJavaEnumeration)
      ex.invokeAll(callables).asScala.map(x => Try(x.get())).toList
    } finally {
      ex.shutdown()
    }
  }

  /** Uses a Spliterator to run a number of tasks in parallel, where any number of threads may be alive at any point.
    * This is useful for running a large number of tasks with low memory consumption. Spliterator's default thread pool
    * is ForkJoinPool.commonPool().
    *
    * @param tasks
    *   the tasks to parallelize.
    * @tparam V
    *   the output type of each task.
    * @return
    *   an array of the executed tasks as either a success or failure.
    */
  def runUsingSpliterator[V](tasks: Iterator[() => V]): Seq[Try[V]] = {
    scala.collection.immutable.ArraySeq
      .ofRef(
        java.util.Arrays
          .stream(tasks.toArray)
          .parallel()
          .map(task => Try(task.apply()))
          .toArray
      )
      .asInstanceOf[scala.collection.immutable.ArraySeq[Try[V]]]
  }

}
