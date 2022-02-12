import io.shiftleft.codepropertygraph.Cpg
import io.joern.dataflowengineoss.language._
import io.shiftleft.semanticcpg.language._
import io.shiftleft.semanticcpg.language.operatorextension.OpNodes.Assignment
import overflowdb.traversal._

@main def main() = {
  def allocated            = cpg.call("malloc").inAssignment.target.dedup
  def freed                = cpg.call("free").argument(1)
  def flowsFromAllocToFree = freed.reachableBy(allocated).toSetImmutable
  allocated.map(_.code).toSetImmutable.diff(flowsFromAllocToFree.map(_.code))
}
