@main def main() = {
  def allocated            = cpg.call("malloc").inAssignment.target.dedup
  def freed                = cpg.call("free").argument(1)
  def flowsFromAllocToFree = freed.reachableBy(allocated).toSetImmutable
  allocated.map(_.code).toSetImmutable.diff(flowsFromAllocToFree.map(_.code))
}
