/* cfgToDot.scala

   This script generates a Dot representation for the CFG of the currently loaded CPG.

   Input: A valid CPG
   Output: String

   Running the Script
   ------------------
   see: README.md

   Sample Output
   -------------
   digraph g {
     node[shape=plaintext];
      "free.c: 11 p" -> "free.c: 11 free(p)";
      "free.c: 11 free(p)" -> "free.c: 9 p";
      "free.c: 10 next" -> "free.c: 10 p->next";
      "free.c: 10 p" -> "free.c: 10 next";
      "free.c: 10 p->next" -> "free.c: 10 q = p->next";
      "free.c: 10 q" -> "free.c: 10 p";
      "free.c: 10 q = p->next" -> "free.c: 11 p";
      "free.c: 9 q" -> "free.c: 9 p = q";
      "free.c: 9 p" -> "free.c: 9 q";
      "free.c: 9 p = q" -> "free.c: 9 p";
      "free.c: 9 NULL" -> "free.c: 9 p != NULL";
      "free.c: 9 p" -> "free.c: 9 NULL";
      "free.c: 9 p != NULL" -> "free.c: 10 q";
      "free.c: 9 p != NULL" -> "";
      "free.c: 9 head" -> "free.c: 9 *p = head";
      "free.c: 9 p" -> "free.c: 9 head";
      "free.c: 9 *p = head" -> "free.c: 9 p";
      "" -> "free.c: 9 p";
   }
 */

import io.shiftleft.Implicits.JavaIteratorDeco
import io.shiftleft.codepropertygraph.generated._
import overflowdb._
import overflowdb.traversal._

import java.nio.file.Paths

/** Some helper functions: adapted from ReachingDefPass.scala in codeproperty graph repo */
def vertexToStr(vertex: Node): String = {
  try {
    val methodVertex = vertex.in("CONTAINS").next
    val fileName = methodVertex.in("CONTAINS").next match {
      case file: nodes.File => file.asInstanceOf[nodes.File].name
      case _ => "NA"
    }

    s"${Paths.get(fileName).getFileName.toString}: ${vertex.property(NodeKeysOdb.LINE_NUMBER)} ${vertex.property(NodeKeysOdb.CODE)}"
  } catch { case _: Exception => "" }
}

def toDot(graph: OdbGraph): String = {
  val buf = new StringBuffer()

  buf.append("digraph g {\n node[shape=plaintext];\n")

  graph.edges("CFG").l.foreach { e =>
    val inV = vertexToStr(e.inNode).replace("\"", "\'")
    val outV = vertexToStr(e.outNode).replace("\"", "\'")
    buf.append(s""" "$outV" -> "$inV";\n """)
  }
  buf.append { "}" }
  buf.toString
}

@main def main(): String = {
  toDot(cpg.graph)
}
