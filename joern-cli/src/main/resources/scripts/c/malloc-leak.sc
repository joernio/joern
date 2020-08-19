import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.dataflowengineoss.language._
import io.shiftleft.semanticcpg.language._
import io.shiftleft.semanticcpg.language.operatorextension.opnodes.Assignment

@main def main() = {
  val allocated = cpg.call("malloc").inAssignment.target.toSet
  val freed = cpg.call("free").argument(1).l
  val flowsFromAllocToFree = freed.start.reachableBy(allocated.start).toSet
  allocated.map(_.code).toSet.diff(flowsFromAllocToFree.map(_.code))
}
