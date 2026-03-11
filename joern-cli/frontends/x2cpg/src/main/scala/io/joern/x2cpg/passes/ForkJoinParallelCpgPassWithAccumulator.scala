package io.joern.x2cpg.passes

import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.passes.ForkJoinParallelCpgPass

import java.util.concurrent.ConcurrentLinkedQueue
import scala.jdk.CollectionConverters.*

/** A [[ForkJoinParallelCpgPass]] that additionally maintains a thread-local accumulator of type [[R]] which is merged
  * across all threads after processing completes. This enables map-reduce style aggregation alongside the usual
  * DiffGraph-based graph modifications.
  *
  * Each thread gets its own accumulator instance (via [[newAccumulator]]). After all parts are processed, the
  * accumulators are merged using [[mergeAccumulators]] and the result is passed to [[onAccumulatorComplete]].
  *
  * @tparam T
  *   the part type (same as in [[ForkJoinParallelCpgPass]])
  * @tparam R
  *   the accumulator type
  */
abstract class ForkJoinParallelCpgPassWithAccumulator[T <: AnyRef, R](cpg: Cpg, outName: String = "")
    extends ForkJoinParallelCpgPass[T](cpg, outName) {

  /** Create a fresh, empty accumulator. Called once per thread. */
  protected def newAccumulator(): R

  /** Merge two accumulators. Must be associative. The result may reuse either argument. */
  protected def mergeAccumulators(left: R, right: R): R

  /** Process a single part, writing graph changes to `builder` and aggregated data to `acc`. */
  protected def runOnPartWithAccumulator(builder: DiffGraphBuilder, acc: R, part: T): Unit

  /** Called after all parts are processed with the fully merged accumulator. */
  protected def onAccumulatorComplete(acc: R): Unit = {}

  private val accumulators = new ConcurrentLinkedQueue[R]()

  private val threadLocalAcc: ThreadLocal[R] = new ThreadLocal[R]()

  // Guard against double-finish: ForkJoinParallelCpgPass.createAndApply() calls finish() in its
  // finally block, AND runWithBuilder() (called from createAndApply) also calls finish() in its
  // own finally block. Without this guard the second call would invoke onAccumulatorComplete with
  // an empty accumulator, overwriting the result from the first call.
  //
  // @note: This is a hack to prevent double-finish.
  // TODO: We should find a better way to handle this in ForkJoinParallelCpgPass directly.
  private var _finishCompleted = false

  final override def runOnPart(builder: DiffGraphBuilder, part: T): Unit = {
    var acc = threadLocalAcc.get()
    if (acc == null) {
      acc = newAccumulator()
      threadLocalAcc.set(acc)
      accumulators.add(acc)
    }
    runOnPartWithAccumulator(builder, acc, part)
  }

  override def init(): Unit = {
    _finishCompleted = false
    accumulators.clear()
    threadLocalAcc.remove()
    super.init()
  }

  override def finish(): Unit = {
    if (!_finishCompleted) {
      _finishCompleted = true
      val merged = accumulators.asScala.reduceOption(mergeAccumulators).getOrElse(newAccumulator())
      onAccumulatorComplete(merged)
      accumulators.clear()
      threadLocalAcc.remove()
    }
    super.finish()
  }
}
