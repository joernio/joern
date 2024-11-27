import upickle.default.*
import io.shiftleft.utils.IOUtils
import java.nio.file.Path
import io.joern.dataflowengineoss.slicing.{DataFlowSlice, SliceEdge}

@main def exec(sliceFile: String) = {
  val jsonContent       = IOUtils.readLinesInFile(Path.of(sliceFile)).mkString
  val dataFlowSlice = read[DataFlowSlice](jsonContent)
  val nodeMap = dataFlowSlice.nodes.map(n => n.id -> n).toMap
  val edges = dataFlowSlice.edges.toList
    .map { case SliceEdge(src, dst, _) =>
      (nodeMap(src).lineNumber, nodeMap(dst).lineNumber) -> List(nodeMap(src).code, nodeMap(dst).code).distinct
    }
    .sortBy(_._1)
    .flatMap(_._2)
  println(edges)
}
