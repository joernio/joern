package io.joern.x2cpg.passes.controlflow.cfgdominator

trait CfgAdapter[Node] {
  def successors(node: Node): IterableOnce[Node]
  def predecessors(node: Node): IterableOnce[Node]
}
