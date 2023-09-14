package io.shiftleft.semanticcpg.language

import scala.collection.mutable

object NodeOrdering {

  /** For a given CFG with the entry node `cfgEntry` and an expansion function `expand`, return a map that associates
    * each node with an index such that nodes are numbered in post order.
    */
  def postOrderNumbering[NodeType](
    cfgEntry: NodeType,
    expand: NodeType => Iterator[NodeType]
  ): mutable.LinkedHashMap[NodeType, Int] = {
    var stack      = (cfgEntry, expand(cfgEntry)) :: Nil
    val visited    = mutable.Set.empty[NodeType]
    val numbering  = mutable.LinkedHashMap.empty[NodeType, Int]
    var nextNumber = 0

    while (stack.nonEmpty) {
      val (node, successors) = stack.head
      visited += node

      if (successors.hasNext) {
        val successor = successors.next()
        if (!visited.contains(successor)) {
          stack = (successor, expand(successor)) :: stack
        }
      } else {
        stack = stack.tail
        numbering.put(node, nextNumber)
        nextNumber += 1
      }
    }
    numbering
  }

  /** For a list of (node, number) pairs, return the list of nodes obtained by sorting nodes according to number in
    * reverse order.
    */
  def reverseNodeList[NodeType](nodeNumberPairs: List[(NodeType, Int)]): List[NodeType] = {
    nodeNumberPairs
      .sortBy { case (_, num) => -num }
      .map { case (node, _) => node }
  }

  /** For a list of (node, number) pairs, return the list of nodes obtained by sorting nodes according to number.
    */
  def nodeList[NodeType](nodeNumberPairs: List[(NodeType, Int)]): List[NodeType] = {
    nodeNumberPairs
      .sortBy { case (_, num) => num }
      .map { case (node, _) => node }
  }

}
