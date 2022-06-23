package io.joern.c2cpg.passes

import io.joern.c2cpg.Config
import io.joern.c2cpg.astcreation.{AstCreator, Defines}
import io.joern.c2cpg.datastructures.CGlobal
import io.joern.c2cpg.parser.{CdtParser, FileDefaults}
import io.joern.c2cpg.passes.AstCreationPass.InputFiles
import io.joern.c2cpg.utils.{Report, TimeUtils}
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.passes.ConcurrentWriterCpgPass
import io.shiftleft.utils.IOUtils
import io.joern.x2cpg.SourceFiles

import java.nio.file.Paths
import scala.jdk.CollectionConverters._

object AstCreationPass {
  sealed trait InputFiles
  object HeaderFiles extends InputFiles
  object SourceFiles extends InputFiles
}

class AstCreationPass(cpg: Cpg, forFiles: InputFiles, config: Config, report: Report = new Report())
    extends ConcurrentWriterCpgPass[String](cpg) {

  private val global: CGlobal   = new CGlobal()
  private val parser: CdtParser = new CdtParser(config)

  private def sourceFiles: Set[String] =
    SourceFiles.determine(config.inputPath, FileDefaults.SOURCE_FILE_EXTENSIONS).toSet

  private def headerFiles: Set[String] = {
    val allHeaderFiles         = SourceFiles.determine(config.inputPath, FileDefaults.HEADER_FILE_EXTENSIONS).toSet
    val alreadySeenHeaderFiles = CGlobal.headerFiles
    allHeaderFiles -- alreadySeenHeaderFiles
  }

  def usedTypes(): List[String] =
    global.usedTypes.keys().asScala.filterNot(_ == Defines.anyTypeName).toList

  override def generateParts(): Array[String] = forFiles match {
    case AstCreationPass.HeaderFiles => headerFiles.toArray
    case AstCreationPass.SourceFiles => sourceFiles.toArray
  }

  override def runOnPart(diffGraph: DiffGraphBuilder, filename: String): Unit = {
    val path    = Paths.get(filename)
    val fileLOC = IOUtils.readLinesInFile(path).size
    val (gotCpg, duration) = TimeUtils.time {
      val parseResult = parser.parse(path)
      parseResult match {
        case Some(translationUnit) =>
          report.addReportInfo(filename, fileLOC, parsed = true)
          val localDiff = new AstCreator(filename, config, global, translationUnit).createAst()
          diffGraph.absorb(localDiff)
          true
        case None =>
          report.addReportInfo(filename, fileLOC)
          false
      }
    }
    report.updateReport(filename, gotCpg, duration)
  }

}
