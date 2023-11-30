package io.shiftleft.semanticcpg.dotgenerator

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.v2.nodes.{Method, StoredNode}
// TODO cleanup imports
import io.shiftleft.codepropertygraph.generated.v2.accessors.Lang.*
import io.shiftleft.codepropertygraph.generated.v2.neighboraccessors.Lang.*

import io.shiftleft.semanticcpg.dotgenerator.DotSerializer.{Edge, Graph}
import io.shiftleft.semanticcpg.language.*

import scala.collection.mutable

class CallGraphGenerator {

  def generate(cpg: Cpg): Graph = {
    val subgraph = mutable.HashMap.empty[String, Seq[StoredNode]]
    val vertices = cpg.method.l
    val edges = for {
      srcMethod <- vertices
      _ = storeInSubgraph(srcMethod, subgraph)
      child <- srcMethod.call
      tgt   <- child._callOut.collectAll[Method] // TODO define as named step in graph
    } yield {
      storeInSubgraph(tgt, subgraph)
      Edge(srcMethod, tgt, label = child.dispatchType.stripSuffix("_DISPATCH"))
    }
    Graph(vertices, edges.distinct, subgraph.toMap)
  }

  def storeInSubgraph(method: Method, subgraph: mutable.Map[String, Seq[StoredNode]]): Unit = {
    method.typeDeclViaAstIn match {
      case Some(typeDeclName) =>
        subgraph.put(typeDeclName.fullName, subgraph.getOrElse(typeDeclName.fullName, Seq()) ++ Seq(method))
      case None =>
        subgraph.put(method.astParentFullName, subgraph.getOrElse(method.astParentFullName, Seq()) ++ Seq(method))
    }
  }

}
