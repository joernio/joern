package io.joern.dataflowengineoss

import io.shiftleft.codepropertygraph.generated.nodes.{AstNode, Call, CfgNode}

package object queryengine {

  /** The TaskFingerprint uniquely identifies a task.
    */
  case class TaskFingerprint(sink: CfgNode, callSiteStack: List[Call], callDepth: Int)

  /** A (partial) result, informing about a path that exists from a source to another node in the graph.
    *
    * @param taskStack
    *   The list of tasks that was solved to arrive at this task
    *
    * @param path
    *   A path to the sink.
    *
    * @param partial
    *   indicate whether this result stands on its own or requires further analysis, e.g., by expanding output arguments
    *   backwards into method output parameters.
    */
  case class ReachableByResult(taskStack: List[TaskFingerprint], path: Vector[PathElement], partial: Boolean = false) {

    def fingerprint: TaskFingerprint = taskStack.last
    def sink: CfgNode                = fingerprint.sink
    def callSiteStack: List[Call]    = fingerprint.callSiteStack

    def callDepth: Int = fingerprint.callDepth

    def startingPoint: CfgNode = path.head.node.asInstanceOf[CfgNode]

    /** If the result begins in an output argument, return it.
      */
    def outputArgument: Option[CfgNode] = {
      path.headOption.collect {
        case elem: PathElement if elem.isOutputArg =>
          elem.node.asInstanceOf[CfgNode]
      }
    }
  }

  /** We represent data flows as sequences of path elements, where each path element consists of a node, flags and the
    * label of its outgoing edge.
    *
    * @param node
    *   The parent node. This is actually always a CfgNode during data flow computation, however, since the source may
    *   be an arbitrary AST node, we may add an AST node to the start of the flow right before returning flows to the
    *   user.
    *
    * @param callSiteStack
    *   The call stack when this path element was created. Since we may enter the same function via two different call
    *   sites, path elements should only be treated as the same if they are the same node and we've reached them via the
    *   same call sequence.
    *
    * @param visible
    *   whether this path element should be shown in the flow
    * @param isOutputArg
    *   input and output arguments are the same node in the CPG, so, we need this additional flag to determine whether
    *   we are on an input or output argument. By default, we consider arguments to be input arguments, meaning that
    *   when tracking `x` at `f(x)`, we do not expand into `f` but rather upwards to producers of `x`.
    * @param outEdgeLabel
    *   label of the outgoing DDG edge
    */
  case class PathElement(
    node: AstNode,
    callSiteStack: List[Call] = List(),
    visible: Boolean = true,
    isOutputArg: Boolean = false,
    outEdgeLabel: String = ""
  )

  /** @param taskStack
    *   The list of tasks that was solved to arrive at this task, including the current task, which is to be solved. The
    *   current task is the last element of the list.
    *
    * @param initialPath
    *   The path from the current sink downwards to previous sinks.
    */
  case class ReachableByTask(taskStack: List[TaskFingerprint], initialPath: Vector[PathElement]) {

    /** This tasks fingerprint: if two tasks have the same fingerprint, then the TaskSolver MUST return the same result
      * for them. This is the basis of our caching scheme.
      */
    def fingerprint: TaskFingerprint = taskStack.last

    /** The sink at which we start the analysis (upwards)
      */
    def sink: CfgNode = fingerprint.sink

    /** The call sites we have expanded downwards during this analysis. We need to keep track of this so that we do not
      * end up expanding one call site and then returning to a different call site, which would produce an unreachable
      * path.
      */
    def callSiteStack: List[Call] = fingerprint.callSiteStack

    /** The call depth at which this task was created.
      */
    def callDepth: Int = fingerprint.callDepth

  }

  case class TaskSummary(tableEntries: Vector[(TaskFingerprint, TableEntry)], followupTasks: Vector[ReachableByTask])
  case class TableEntry(path: Vector[PathElement])

}
