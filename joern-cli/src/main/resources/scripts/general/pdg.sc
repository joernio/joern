/* pdg.sc

   This script returns a complete PDG for functions matching a regex, or the whole CPG if no regex is specified. The PDG
   is represented as two lists, one for the edges and another for the vertices.

   The first list contains all of the edges in the PDG. The first entry in each tuple contains the ID of the incoming
   vertex. The second entry in the tuple contains the ID of the outgoing vertex.

   The second list contains all the vertices in the PDG. The first entry in each tuple contains the ID of the vertex
   and the second entry contains the code stored in the vertex.
*/

import gremlin.scala.{Edge, GremlinScala}

import io.shiftleft.codepropertygraph.generated.EdgeTypes

import scala.collection.mutable

type EdgeEntry = (AnyRef, AnyRef)
type VertexEntry = (AnyRef, String)
type Pdg = (Option[String], List[EdgeEntry], List[VertexEntry])


private def pdgFromEdges(edges: GremlinScala[Edge]): (List[EdgeEntry], List[VertexEntry]) = {
  val filteredEdges = edges.filter(edge => edge.hasLabel(EdgeTypes.REACHING_DEF, EdgeTypes.CDG)).dedup.l

  val (edgeResult, vertexResult) =
    filteredEdges.foldLeft((mutable.Set.empty[EdgeEntry], mutable.Set.empty[VertexEntry])) {
      case ((edgeList, vertexList), edge) =>
        val edgeEntry = (edge.inVertex().id, edge.outVertex().id)
        val inVertexEntry = (edge.inVertex().id, edge.inVertex().property("CODE").orElse(""))
        val outVertexEntry = (edge.outVertex().id, edge.outVertex().property("CODE").orElse(""))

        (edgeList += edgeEntry, vertexList ++= Set(inVertexEntry, outVertexEntry))
    }

  (edgeResult.toList, vertexResult.toList)
}

@main def main(methodRegex: String = ""): List[Pdg] = {
  if (methodRegex.isEmpty) {
    val (edgeEntries, vertexEntries) = pdgFromEdges(cpg.scalaGraph.E())
    List((None, edgeEntries, vertexEntries))
  } else {
    cpg.method(methodRegex).l.map { method =>
      val methodFile = method.location.filename
      val (edgeEntries, vertexEntries) = pdgFromEdges(method.asScala.out().flatMap(_.asScala.outE()))

      (Some(methodFile), edgeEntries, vertexEntries)
    }
  }
}
