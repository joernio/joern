package io.joern.c2cpg.passes

import better.files.File
import io.joern.c2cpg.Config
import io.joern.c2cpg.astcreation.AstCreator
import io.joern.c2cpg.datastructures.CGlobal
import io.joern.c2cpg.parser.{CdtParser, FileDefaults}
import io.joern.c2cpg.passes.AstCreationPass.InputFiles
import io.joern.c2cpg.utils.{IOUtils, Report, TimeUtils}
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.passes.ConcurrentWriterCpgPass
import io.joern.x2cpg.SourceFiles
import org.slf4j.LoggerFactory

import java.nio.file.Paths
import java.util.concurrent.ConcurrentHashMap

object AstCreationPass {

  private val logger = LoggerFactory.getLogger(getClass)

  sealed trait InputFiles
  object HeaderFiles extends InputFiles
  object SourceFiles extends InputFiles

}

class AstCreationPass(cpg: Cpg, forFiles: InputFiles, config: Config, report: Report = new Report())
    extends ConcurrentWriterCpgPass[String](cpg) {

  import io.joern.c2cpg.passes.AstCreationPass.logger

  private val file2OffsetTable: ConcurrentHashMap[String, Array[Int]] = new ConcurrentHashMap()
  private val parser: CdtParser                                       = new CdtParser(config)

  private def sourceFiles: Set[String] =
    SourceFiles.determine(config.inputPath, FileDefaults.SOURCE_FILE_EXTENSIONS).toSet

  private def headerFiles: Set[String] = {
    val allHeaderFiles         = SourceFiles.determine(config.inputPath, FileDefaults.HEADER_FILE_EXTENSIONS).toSet
    val alreadySeenHeaderFiles = CGlobal.headerFiles.map(IOUtils.toAbsolutePath(_, config))
    allHeaderFiles -- alreadySeenHeaderFiles
  }

  private def isIgnoredByUserConfig(filePath: String): Boolean = {
    lazy val isInIgnoredFiles = config.ignoredFiles.exists {
      case ignorePath if File(ignorePath).isDirectory => filePath.startsWith(ignorePath)
      case ignorePath                                 => filePath == ignorePath
    }
    lazy val isInIgnoredFileRegex = config.ignoredFilesRegex.matches(filePath)
    if (isInIgnoredFiles || isInIgnoredFileRegex) {
      logger.debug(s"'$filePath' ignored by user configuration")
      true
    } else {
      false
    }
  }

  private def isIgnoredByDefault(filePath: String): Boolean = {
    val relPath = File(config.inputPath).relativize(File(filePath))
    if (relPath.toString.startsWith(".")) {
      logger.debug(s"'$filePath' ignored by default")
      true
    } else {
      false
    }
  }

  private def filterFiles(files: Set[String]): Set[String] = files.filter {
    case filePath if isIgnoredByUserConfig(filePath) => false
    case filePath if isIgnoredByDefault(filePath)    => false
    case _                                           => true
  }

  override def generateParts(): Array[String] = filterFiles(forFiles match {
    case AstCreationPass.HeaderFiles => headerFiles
    case AstCreationPass.SourceFiles => sourceFiles
  }).toArray

  override def runOnPart(diffGraph: DiffGraphBuilder, filename: String): Unit = {
    val path    = Paths.get(filename).toAbsolutePath
    val relPath = IOUtils.toRelativePath(path.toString, config)
    val fileLOC = io.shiftleft.utils.IOUtils.readLinesInFile(path).size
    val (gotCpg, duration) = TimeUtils.time {
      val parseResult = parser.parse(path)
      parseResult match {
        case Some(translationUnit) =>
          report.addReportInfo(filename, fileLOC, parsed = true)
          val localDiff = new AstCreator(relPath, config, translationUnit, file2OffsetTable).createAst()
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
