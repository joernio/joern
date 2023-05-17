package io.joern.c2cpg.passes

import better.files.File
import io.joern.c2cpg.Config
import io.joern.c2cpg.astcreation.AstCreator
import io.joern.c2cpg.parser.{CdtParser, FileDefaults}
import io.joern.c2cpg.utils.{Report, TimeUtils}
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.passes.ConcurrentWriterCpgPass
import io.joern.x2cpg.SourceFiles
import org.slf4j.LoggerFactory

import java.nio.file.Paths
import java.util.concurrent.ConcurrentHashMap
import java.util.regex.Pattern
import scala.util.matching.Regex

class AstCreationPass(cpg: Cpg, config: Config, report: Report = new Report())
    extends ConcurrentWriterCpgPass[String](cpg) {

  private val logger                                                  = LoggerFactory.getLogger(getClass)
  private val file2OffsetTable: ConcurrentHashMap[String, Array[Int]] = new ConcurrentHashMap()
  private val parser: CdtParser                                       = new CdtParser(config)

  private val EscapedFileSeparator = Pattern.quote(java.io.File.separator)
  private val DefaultIgnoredFolders: List[Regex] = List(
    "\\..*".r,
    s"(.*[$EscapedFileSeparator])?tests?[$EscapedFileSeparator].*".r,
    s"(.*[$EscapedFileSeparator])?CMakeFiles[$EscapedFileSeparator].*".r
  )

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
    val relPath = File(config.inputPath).relativize(File(filePath)).toString
    if (DefaultIgnoredFolders.exists(_.matches(relPath))) {
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

  override def generateParts(): Array[String] = filterFiles(
    SourceFiles
      .determine(config.inputPath, FileDefaults.SOURCE_FILE_EXTENSIONS ++ FileDefaults.HEADER_FILE_EXTENSIONS)
      .toSet
  ).toArray

  override def runOnPart(diffGraph: DiffGraphBuilder, filename: String): Unit = {
    val path    = Paths.get(filename).toAbsolutePath
    val relPath = SourceFiles.toRelativePath(path.toString, config.inputPath)
    val fileLOC = io.shiftleft.utils.IOUtils.readLinesInFile(path).size
    val (gotCpg, duration) = TimeUtils.time {
      val parseResult = parser.parse(path)
      parseResult match {
        case Some(translationUnit) =>
          report.addReportInfo(relPath, fileLOC, parsed = true)
          val localDiff = new AstCreator(relPath, config, translationUnit, file2OffsetTable).createAst()
          diffGraph.absorb(localDiff)
          true
        case None =>
          report.addReportInfo(relPath, fileLOC)
          false
      }
    }
    report.updateReport(relPath, gotCpg, duration)
  }

}
