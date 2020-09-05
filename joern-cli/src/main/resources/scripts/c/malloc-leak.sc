import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.dataflowengineoss.language._
import io.shiftleft.semanticcpg.language._
import io.shiftleft.semanticcpg.language.operatorextension.opnodes.Assignment
import overflowdb.traversal._

@main def main() = {
  def allocated = cpg.call("malloc").inAssignment.target.dedup
  def freed = cpg.call("free").argument(1)
  def flowsFromAllocToFree = freed.reachableBy(allocated).toSet
  allocated.map(_.code).toSet.diff(flowsFromAllocToFree.map(_.code))
}
