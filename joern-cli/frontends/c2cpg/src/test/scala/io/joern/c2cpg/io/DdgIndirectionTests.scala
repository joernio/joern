package io.joern.c2cpg.io

import io.joern.c2cpg.testfixtures.DataFlowCodeToCpgSuite
import io.joern.dataflowengineoss.layers.dataflows.{DdgDumpOptions, DumpDdg}
import io.shiftleft.semanticcpg.layers.LayerCreatorContext
import io.shiftleft.semanticcpg.utils.FileUtil
import io.shiftleft.semanticcpg.utils.FileUtil.*

import java.nio.file.Files
import scala.io.Source

class DdgIndirectionTests extends DataFlowCodeToCpgSuite {

  "DumpDdg" should {
    val cpg = code("""
        |int * glob;
        |
        |int main(int argc, char **argv) {
        |  int x = 0;
        |  int * xx = &x;
        |  while (((x) && *xx) || *glob){
        |    x++;
        |  }
        |  return 0;
        |}
        |""".stripMargin)

    "include indirection operators in DDG when they are part of conditional expressions" in {
      FileUtil.usingTemporaryDirectory("dumpddg") { tmpDir =>
        val opts         = DdgDumpOptions(tmpDir.toString)
        val layerContext = new LayerCreatorContext(cpg)
        new DumpDdg(opts).run(layerContext)

        val dotFile    = tmpDir / "0-ddg.dot"
        val dotContent = Source.fromFile(dotFile.toFile).getLines().mkString("\n")

        // The *glob indirection operator should be present as a node (HTML-escaped in DOT format)
        dotContent should include("&lt;operator&gt;.indirection")
        dotContent should include("*glob")

        // There should be an edge from *glob to the logicalOr operator (condition)
        dotContent should include regex """".*" -> ".*"\s+\[\s+label = "\*glob"\]"""
      }
    }
  }

  "DumpDdg" should {
    val cpg2 = code("""
        |int * glob;
        |
        |int main(int argc, char **argv) {
        |  int x = 0;
        |  int y = *glob;
        |  return y;
        |}
        |""".stripMargin)

    "filter out indirection operators that are not in control structures" in {
      FileUtil.usingTemporaryDirectory("dumpddg") { tmpDir =>
        val opts         = DdgDumpOptions(tmpDir.toString)
        val layerContext = new LayerCreatorContext(cpg2)
        new DumpDdg(opts).run(layerContext)

        val dotFile    = tmpDir / "0-ddg.dot"
        val dotContent = Source.fromFile(dotFile.toFile).getLines().mkString("\n")

        // The simple *glob in assignment should be filtered out (not in a condition)
        // We should NOT see indirection operator nodes
        dotContent should not include "&lt;operator&gt;.indirection"
      }
    }
  }

}
