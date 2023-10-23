package io.joern.javasrc2cpg.jpastprinter

import com.github.javaparser.printer.YamlPrinter
import com.github.javaparser.printer.DotPrinter
import io.shiftleft.semanticcpg.language.dotextension.Shared
import io.joern.javasrc2cpg.Config
import io.joern.javasrc2cpg.util.SourceParser
import java.nio.file.Path

object JavaParserAstPrinter {
  def printJpAsts(config: Config): Unit = {

    val sourceParser = SourceParser(config, false)
    val printer      = new YamlPrinter(true)

    SourceParser.getSourceFilenames(config).foreach { filename =>
      val relativeFilename = Path.of(config.inputPath).relativize(Path.of(filename)).toString
      sourceParser.parseAnalysisFile(relativeFilename, saveFileContent = false).foreach { case (compilationUnit, _) =>
        println(relativeFilename)
        println(printer.output(compilationUnit))
      }
    }
  }
}
