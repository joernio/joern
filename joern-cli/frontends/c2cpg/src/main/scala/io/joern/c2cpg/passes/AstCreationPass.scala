package io.joern.c2cpg.passes

import better.files.File
import io.joern.c2cpg.C2Cpg
import io.joern.c2cpg.astcreation.{AstCreator, Defines}
import io.joern.c2cpg.datastructures.Global
import io.joern.c2cpg.parser.{CdtParser, FileDefaults, HeaderFileFinder, ParserConfig}
import io.joern.c2cpg.passes.AstCreationPass.InputFiles
import io.joern.c2cpg.utils.{Report, TimeUtils}
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.passes.{ConcurrentWriterCpgPass, IntervalKeyPool}
import io.shiftleft.x2cpg.SourceFiles

import java.nio.file.Paths
import scala.jdk.CollectionConverters._

object AstCreationPass {
  sealed trait InputFiles
  object HeaderFiles extends InputFiles
  object SourceFiles extends InputFiles
}

class AstCreationPass(
    cpg: Cpg,
    forFiles: InputFiles,
    keyPool: Option[IntervalKeyPool],
    config: C2Cpg.Config,
    report: Report = new Report()
) extends ConcurrentWriterCpgPass[String](cpg, keyPool = keyPool) {

  private val global: Global                     = new Global()
  private val parserConfig: ParserConfig         = ParserConfig.fromConfig(config)
  private val headerFileFinder: HeaderFileFinder = new HeaderFileFinder(config.inputPaths)

  private def sourceFiles: Set[String] =
    SourceFiles.determine(config.inputPaths, FileDefaults.SOURCE_FILE_EXTENSIONS).toSet

  private def headerFiles: Set[String] = {
    val allHeaderFiles         = SourceFiles.determine(config.inputPaths, FileDefaults.HEADER_FILE_EXTENSIONS).toSet
    val alreadySeenHeaderFiles = Global.headerFiles
    allHeaderFiles -- alreadySeenHeaderFiles
  }

  def usedTypes(): List[String] =
    global.usedTypes.keys().asScala.filterNot(_ == Defines.anyTypeName).toList

  override def generateParts(): Array[String] = forFiles match {
    case AstCreationPass.HeaderFiles => headerFiles.toArray
    case AstCreationPass.SourceFiles => sourceFiles.toArray
  }

  override def runOnPart(diffGraph: DiffGraphBuilder, filename: String): Unit = {
    val parser = new CdtParser(parserConfig, headerFileFinder)
    val (gotCpg, duration) = TimeUtils.time {
      val parseResult = parser.parse(Paths.get(filename))
      parseResult match {
        case Some(translationUnit) =>
          report.addReportInfo(filename, File(filename).lineCount, parsed = true)
          val localDiff = new DiffGraphBuilder
          new AstCreator(filename, config, global, localDiff, translationUnit).createAst()
          diffGraph.absorb(localDiff)
          true
        case None =>
          report.addReportInfo(filename, File(filename).lineCount)
          false
      }
    }
    report.updateReport(filename, gotCpg, duration)
  }

}
