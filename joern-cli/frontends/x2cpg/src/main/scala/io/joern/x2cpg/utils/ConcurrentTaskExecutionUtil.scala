package io.joern.x2cpg.utils

import io.shiftleft.utils.ExecutionContextProvider

import scala.collection.mutable
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.Try

/** Following ConcurrentWriterCpgPass, this creates a generic re-usable utility for bootstrapping basic non-CPG pass
  * concurrency.
  */
object ConcurrentTaskExecutionUtil {

  private val MAX_QUEUE_SIZE_DEFAULT        = 2 + 4 * Runtime.getRuntime.availableProcessors()
  private implicit val ec: ExecutionContext = ExecutionContextProvider.getExecutionContext

  /** Uses the parallel queue strategy from [[io.shiftleft.passes.ConcurrentWriterCpgPass]] to offer a generic means of
    * executing an iterator of tasks that share an output type in parallel.
    *
    * @param tasks
    *   the tasks to parallelize.
    * @param maxQueueSize
    *   the max number of tasks to queue for parallel execution.
    * @tparam V
    *   the output type of each task.
    * @return
    *   an array of the executed tasks as either a success or failure.
    * @see
    *   [[io.shiftleft.passes.ConcurrentWriterCpgPass]]
    */
  def runInParallel[V](tasks: Iterator[() => V], maxQueueSize: Int = MAX_QUEUE_SIZE_DEFAULT): List[Try[V]] = {
    val completionQueue = mutable.ArrayDeque.empty[Future[V]]
    val results         = mutable.ArrayBuffer.empty[Try[V]]

    var done = false
    while (!done) {
      if (completionQueue.size < maxQueueSize && tasks.hasNext) {
        val nextTask = tasks.next()
        completionQueue.append(Future.apply(nextTask()))
      } else if (completionQueue.nonEmpty) {
        val future = completionQueue.removeHead()
        val res    = Try(Await.result(future, Duration.Inf))
        results.append(res)
      } else {
        done = true
      }
    }

    results.toList
  }

}
