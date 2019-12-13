/* pdg.sc

   This script returns a complete PDG for the whole CPG, separated into two lists, once for the edges and another for the
   vertices.

   The first list contains all of the edges in the PDG. The first entry in each tuple contains the ID of the incoming
   vertex. The second entry in the tuple contains the ID of the outgoing vertex.

   The second list contains all the vertices in the PDG. The first entry in each tuple contains the ID of the vertex
   and the second entry contains the code stored in the vertex.
*/

import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.joern.console.Console.cpg

import scala.collection.mutable

type EdgeEntry = (AnyRef, AnyRef)
type VertexEntry = (AnyRef, String)

val edges = cpg.scalaGraph.E().filter(edge => edge.hasLabel(EdgeTypes.REACHING_DEF, EdgeTypes.CDG)).dedup.l

val (edgeResult, vertexResult) =
  edges.foldLeft((mutable.Set.empty[EdgeEntry], mutable.Set.empty[VertexEntry])) {
    case ((edgeList, vertexList), edge) =>
      val edgeEntry = (edge.inVertex().id, edge.outVertex().id)
      val inVertexEntry = (edge.inVertex().id, edge.inVertex().property("CODE").orElse(""))
      val outVertexEntry = (edge.inVertex().id, edge.inVertex().property("CODE").orElse(""))

      (edgeList += edgeEntry, vertexList ++= Set(inVertexEntry, outVertexEntry))
  }

(edgeResult.toList, vertexResult.toList)
