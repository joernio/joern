package io.joern.c2cpg.passes

import io.joern.c2cpg.C2Cpg.DefaultIgnoredFolders
import io.joern.c2cpg.Config
import io.joern.c2cpg.astcreation.AstCreator
import io.joern.c2cpg.astcreation.Defines
import io.joern.c2cpg.parser.{CdtParser, FileDefaults}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.passes.ConcurrentWriterCpgPass
import io.joern.x2cpg.SourceFiles
import io.joern.x2cpg.datastructures.Global
import io.joern.x2cpg.utils.Report
import io.joern.x2cpg.utils.TimeUtils

import java.nio.file.Paths
import java.util.concurrent.ConcurrentHashMap
import scala.util.matching.Regex
import scala.jdk.CollectionConverters.*

class AstCreationPass(cpg: Cpg, config: Config, report: Report = new Report())
    extends ConcurrentWriterCpgPass[String](cpg) {

  private val file2OffsetTable: ConcurrentHashMap[String, Array[Int]] = new ConcurrentHashMap()
  private val parser: CdtParser                                       = new CdtParser(config)

  private val global = new Global()

  def typesSeen(): List[String] = global.usedTypes.keys().asScala.filterNot(_ == Defines.anyTypeName).toList

  override def generateParts(): Array[String] = {
    val sourceFileExtensions = FileDefaults.SOURCE_FILE_EXTENSIONS
      ++ FileDefaults.HEADER_FILE_EXTENSIONS
      ++ Option.when(config.withPreprocessedFiles)(FileDefaults.PREPROCESSED_EXT).toList
    val allSourceFiles = SourceFiles
      .determine(
        config.inputPath,
        sourceFileExtensions,
        ignoredDefaultRegex = Option(DefaultIgnoredFolders),
        ignoredFilesRegex = Option(config.ignoredFilesRegex),
        ignoredFilesPath = Option(config.ignoredFiles)
      )
      .toArray
    if (config.withPreprocessedFiles) {
      allSourceFiles.filter {
        case f if !f.endsWith(FileDefaults.PREPROCESSED_EXT) =>
          val fAsPreprocessedFile = s"${f.substring(0, f.lastIndexOf("."))}${FileDefaults.PREPROCESSED_EXT}"
          !allSourceFiles.exists { sourceFile => f != sourceFile && sourceFile == fAsPreprocessedFile }
        case _ => true
      }
    } else {
      allSourceFiles
    }
  }

  override def runOnPart(diffGraph: DiffGraphBuilder, filename: String): Unit = {
    val path    = Paths.get(filename).toAbsolutePath
    val relPath = SourceFiles.toRelativePath(path.toString, config.inputPath)
    val fileLOC = io.shiftleft.utils.IOUtils.readLinesInFile(path).size
    val (gotCpg, duration) = TimeUtils.time {
      val parseResult = parser.parse(path)
      parseResult match {
        case Some(translationUnit) =>
          report.addReportInfo(relPath, fileLOC, parsed = true)
          val localDiff = new AstCreator(relPath, global, config, translationUnit, file2OffsetTable)(
            config.schemaValidation
          ).createAst()
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
