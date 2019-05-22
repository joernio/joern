import gremlin.scala._
import io.shiftleft.codepropertygraph.generated._
import java.nio.file.Paths
import io.shiftleft.queryprimitives.utils.ExpandTo
import org.apache.tinkerpop.gremlin.structure.Direction
import io.shiftleft.queryprimitives.steps.Implicits.JavaIteratorDeco
import javax.script.ScriptEngineManager

/** Some helper functions: adapted from ReachingDefPass.scala in codeproperty graph repo */
def vertexToStr(vertex: Vertex): String = {
  try {
    val methodVertex = vertex.vertices(Direction.IN, "CONTAINS").nextChecked
    val fileName = methodVertex.vertices(Direction.IN, "CONTAINS").nextChecked match {
      case file: nodes.File => file.asInstanceOf[nodes.File].name
      case _ => "NA"
    }

    s"${Paths.get(fileName).getFileName.toString}: ${vertex.value2(NodeKeys.LINE_NUMBER).toString} ${vertex.value2(NodeKeys.CODE)}"
  } catch { case _: Exception => "" }
}

def toDot(graph: ScalaGraph): String = {
  val buf = new StringBuffer()

  buf.append("digraph g {\n node[shape=plaintext];\n")

  graph.E.hasLabel("CFG").l.foreach { e =>
    val inV = vertexToStr(e.inVertex).replace("\"", "\'")
    val outV = vertexToStr(e.outVertex).replace("\"", "\'")
    buf.append(s""" "$outV" -> "$inV";\n """)
  }
  buf.append { "}" }
  buf.toString
}

toDot(cpg.graph)
