package io.joern.joerncli.slicing

import io.joern.dataflowengineoss.language._
import io.joern.joerncli.JoernSlice.Config
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._
import overflowdb.traversal.Traversal

object DataFlowSlicing {

  def calculateDataFlowSlice(cpg: Cpg, config: Config): ProgramDataFlowSlice = {
    val sliceMapping = (config.sourceFile match {
      case Some(fileName) => cpg.file.nameExact(fileName).ast.isCall
      case None           => cpg.call
    }).toSeq.groupBy(_.method).map { case (m: Method, calls: Traversal[Call]) =>
      m.fullName -> calls.map { c =>
        val sinks = c.argument.l

        val sliceNodes = sinks.iterator.repeat(_.ddgIn)(_.maxDepth(config.sliceDepth).emit).dedup.l
        val sliceEdges = sliceNodes
          .flatMap(_.outE)
          .filter(x => sliceNodes.contains(x.inNode()))
          .groupBy(_.outNode().asInstanceOf[CfgNode])
        DataFlowSlice(sliceNodes, sliceEdges)
      }.toSet
    }
    ProgramDataFlowSlice(sliceMapping)
  }

}
