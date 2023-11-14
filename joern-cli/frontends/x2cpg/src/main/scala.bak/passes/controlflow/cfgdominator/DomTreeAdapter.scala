package io.joern.x2cpg.passes.controlflow.cfgdominator

trait DomTreeAdapter[Node] {

  /** Returns the immediate dominator of a cfgNode. The returned value can be None if cfgNode was the cfg entry node
    * while calculating the dominator relation or if cfgNode is dead code. In the post dominator case "dead code" means
    * code which does lead to the normal method exit. An example would be a thrown excpetion.
    */
  def immediateDominator(cfgNode: Node): Option[Node]
}
