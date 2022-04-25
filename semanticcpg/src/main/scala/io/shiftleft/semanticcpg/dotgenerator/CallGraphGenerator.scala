package io.shiftleft.semanticcpg.dotgenerator

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{Method, StoredNode}
import io.shiftleft.semanticcpg.dotgenerator.DotSerializer.{Edge, Graph}
import io.shiftleft.semanticcpg.language._

import scala.collection.mutable

class CallGraphGenerator {

  def generate(cpg: Cpg): Graph = {
    val subgraph = mutable.HashMap.empty[String, Seq[StoredNode]]
    val vertices = cpg.all.collect { case m: Method => m }.l
    val edges = vertices.flatMap { srcMethod =>
      storeInSubgraph(srcMethod, subgraph)
      srcMethod.call.flatMap { child =>
        child.callOut.map { tgt =>
          storeInSubgraph(tgt, subgraph)
          Edge(srcMethod, tgt, label = child.dispatchType.stripSuffix("_DISPATCH"))
        }
      }
    }.distinct
    Graph(vertices, edges, subgraph.toMap)
  }

  def storeInSubgraph(method: Method, subgraph: mutable.Map[String, Seq[StoredNode]]): Unit = {
    method._typeDeclViaAstIn match {
      case Some(typeDeclName) =>
        subgraph.put(typeDeclName.fullName, subgraph.getOrElse(typeDeclName.fullName, Seq()) ++ Seq(method))
      case None =>
        subgraph.put(method.astParentFullName, subgraph.getOrElse(method.astParentFullName, Seq()) ++ Seq(method))
    }
  }

}
