package io.joern.dataflowengineoss.slicing

import io.joern.dataflowengineoss.language.*
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.PropertyNames
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*

import java.util.concurrent.{Callable, Executors}

object DataFlowSlicing {

  implicit val resolver: ICallResolver = NoResolve

  def calculateDataFlowSlice(cpg: Cpg, config: DataFlowConfig): Option[DataFlowSlice] = {
    implicit val implicitConfig: DataFlowConfig = config

    val exec = config.parallelism match
      case Some(parallelism) if parallelism == 1 => Executors.newSingleThreadExecutor()
      case Some(parallelism) if parallelism > 1  => Executors.newWorkStealingPool(parallelism)
      case _                                     => Executors.newWorkStealingPool()

    (config.fileFilter match {
      case Some(fileName) => cpg.file.nameExact(fileName).method.call
      case None           => cpg.call
    }).method.withMethodNameFilter.withMethodParameterFilter.withMethodAnnotationFilter.call.withExternalCalleeFilter
      .map(c => exec.submit(new TrackDataFlowTask(config, c)))
      .flatMap(_.get())
      .reduceOption { (a, b) => DataFlowSlice(a.nodes ++ b.nodes, a.edges ++ b.edges) }
  }

  private class TrackDataFlowTask(config: DataFlowConfig, c: Call) extends Callable[Option[DataFlowSlice]] {

    override def call(): Option[DataFlowSlice] = {
      val sinks           = config.sinkPatternFilter.map(filter => c.argument.code(filter).l).getOrElse(c.argument.l)
      val sliceNodes      = sinks.iterator.repeat(_.ddgIn)(_.maxDepth(config.sliceDepth).emit).dedup.l
      val sliceNodesIdSet = sliceNodes.id.toSet
      // Lazily set up the rest if the filters are satisfied
      lazy val sliceEdges = sliceNodes
        .flatMap(_.outE)
        .filter(x => sliceNodesIdSet.contains(x.inNode().id()))
        .map { e => SliceEdge(e.outNode().id(), e.inNode().id(), e.label()) }
        .toSet
      lazy val slice = Option(DataFlowSlice(sliceNodes.map(cfgNodeToSliceNode).toSet, sliceEdges))

      if (sliceNodes.isEmpty) None
      else slice
    }

  }

  /** True if the sinks are either calls to external methods or are in external method stubs.
    */
  private def sinksEndAtExternalMethod(sinks: List[Expression]) =
    sinks.isCall.callee.isExternal.nonEmpty || sinks.method.isExternal.nonEmpty

  private def cfgNodeToSliceNode(cfgNode: CfgNode): SliceNode = {
    val sliceNode = SliceNode(
      cfgNode.id(),
      cfgNode.label,
      code = cfgNode.code,
      parentMethod = cfgNode.method.fullName,
      parentFile = cfgNode.file.name.headOption.getOrElse(""),
      lineNumber = cfgNode.lineNumber,
      columnNumber = cfgNode.columnNumber
    )
    cfgNode match {
      case n: Method    => sliceNode.copy(name = n.name, typeFullName = n.methodReturn.typeFullName)
      case n: Return    => sliceNode.copy(name = "RET", typeFullName = n.method.methodReturn.typeFullName)
      case n: MethodRef => sliceNode.copy(name = n.methodFullName, code = n.code)
      case n: TypeRef   => sliceNode.copy(name = n.typeFullName, code = n.code)
      case n =>
        sliceNode.copy(
          name = n.property(PropertyNames.NAME, ""),
          typeFullName = n.property(PropertyNames.TYPE_FULL_NAME, "")
        )
    }
  }

}
