//> using file assertions.sc

@main def main(inputPath: String) = {
  importCode(inputPath)
  def allocated            = cpg.call("malloc").inAssignment.target.dedup
  def freed                = cpg.call("free").argument(1)
  def flowsFromAllocToFree = freed.reachableBy(allocated).toSetImmutable
  val leaks                = allocated.map(_.code).toSetImmutable.diff(flowsFromAllocToFree.map(_.code))

  val expected = Set("leak")
  assertContains("leaks", leaks, expected)

}
