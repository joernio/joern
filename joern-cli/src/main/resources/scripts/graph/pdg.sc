/* pdg.sc

   This script returns a complete PDG for functions matching a regex, or the whole CPG if no regex is specified. The PDG
   is represented as two lists, one for the edges and another for the vertices.

   The first list contains all of the edges in the PDG. The first entry in each tuple contains the ID of the incoming
   vertex. The second entry in the tuple contains the ID of the outgoing vertex.

   The second list contains all the vertices in the PDG. The first entry in each tuple contains the ID of the vertex
   and the second entry contains the code stored in the vertex.
*/

import io.shiftleft.codepropertygraph.generated.EdgeTypes
import overflowdb._
import overflowdb.traversal._
import scala.collection.mutable

type EdgeEntry = (Long, Long)
type VertexEntry = (Long, String)
type Pdg = (Option[String], List[EdgeEntry], List[VertexEntry])


private def pdgFromEdges(edges: Traversal[OdbEdge]): (List[EdgeEntry], List[VertexEntry]) = {
  val filteredEdges = edges.hasLabel(EdgeTypes.REACHING_DEF, EdgeTypes.CDG).dedup.l

  val (edgeResult, vertexResult) =
    filteredEdges.foldLeft((mutable.Set.empty[EdgeEntry], mutable.Set.empty[VertexEntry])) {
      case ((edgeList, vertexList), edge) =>
        val edgeEntry = (edge.inNode.id, edge.outNode.id)
        val inVertexEntry = (edge.inNode.id, edge.inNode.propertyOption(NodeKeysOdb.CODE).getOrElse(""))
        val outVertexEntry = (edge.outNode.id, edge.outNode.propertyOption(NodeKeysOdb.CODE).getOrElse(""))

        (edgeList += edgeEntry, vertexList ++= Set(inVertexEntry, outVertexEntry))
    }

  (edgeResult.toList, vertexResult.toList)
}

@main def main(methodRegex: String = ""): List[Pdg] = {
  if (methodRegex.isEmpty) {
    val (edgeEntries, vertexEntries) = pdgFromEdges(cpg.graph.E())
    List((None, edgeEntries, vertexEntries))
  } else {
    cpg.method(methodRegex).l.map { method =>
      val methodFile = method.location.filename
      val (edgeEntries, vertexEntries) = pdgFromEdges(method.out.outE)

      (Some(methodFile), edgeEntries, vertexEntries)
    }
  }
}
