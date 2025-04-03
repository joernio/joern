package io.joern.c2cpg

import io.joern.c2cpg.astcreation.CGlobal
import io.joern.c2cpg.parser.FileDefaults
import io.joern.c2cpg.passes.{AstCreationPass, PreprocessorPass, TypeDeclNodePass}
import io.joern.c2cpg.passes.FunctionDeclNodePass
import io.joern.c2cpg.passes.FullNameUniquenessPass
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.Languages
import io.joern.x2cpg.passes.frontend.{MetaDataPass, TypeNodePass}
import io.joern.x2cpg.X2Cpg.withNewEmptyCpg
import io.joern.x2cpg.X2CpgFrontend
import io.joern.x2cpg.utils.Report
import io.joern.x2cpg.SourceFiles
import org.slf4j.LoggerFactory

import java.util.regex.Pattern
import scala.util.control.NonFatal
import scala.util.Try
import scala.util.matching.Regex

class C2Cpg extends X2CpgFrontend[Config] {

  private val logger = LoggerFactory.getLogger(classOf[C2Cpg])

  def createCpg(config: Config): Try[Cpg] = {
    withNewEmptyCpg(config.outputPath, config) { (cpg, config) =>
      val report = new Report()
      new MetaDataPass(cpg, Languages.NEWC, config.inputPath).createAndApply()

      val global            = new CGlobal()
      val preprocessedFiles = allPreprocessedFiles(config)

      new AstCreationPass(cpg, preprocessedFiles, gatherFileExtensions(config), config, global, report)
        .createAndApply()
      new AstCreationPass(cpg, preprocessedFiles, Set(FileDefaults.CHeaderFileExtension), config, global, report)
        .createAndApply()

      TypeNodePass.withRegisteredTypes(global.typesSeen(), cpg).createAndApply()
      new TypeDeclNodePass(cpg, config).createAndApply()
      new FunctionDeclNodePass(cpg, global.unhandledMethodDeclarations(), config).createAndApply()
      new FullNameUniquenessPass(cpg).createAndApply()
      report.print()
    }
  }

  private def gatherFileExtensions(config: Config): Set[String] = {
    FileDefaults.SourceFileExtensions ++
      FileDefaults.CppHeaderFileExtensions ++
      Option.when(config.withPreprocessedFiles)(FileDefaults.PreprocessedExt).toList
  }

  private def allPreprocessedFiles(config: Config): List[String] = {
    if (config.withPreprocessedFiles) {
      SourceFiles
        .determine(
          config.inputPath,
          Set(FileDefaults.PreprocessedExt),
          ignoredDefaultRegex = Option(C2Cpg.DefaultIgnoredFolders),
          ignoredFilesRegex = Option(config.ignoredFilesRegex),
          ignoredFilesPath = Option(config.ignoredFiles)
        )
    } else { List.empty }
  }

  def printIfDefsOnly(config: Config): Unit = {
    try {
      new PreprocessorPass(config).run().foreach(stmt => print(s"$stmt,"))
    } catch {
      case NonFatal(ex) =>
        logger.error("Failed to print preprocessor statements.", ex)
        throw ex
    }
  }

}

object C2Cpg {

  private val EscapedFileSeparator = Pattern.quote(java.io.File.separator)

  val DefaultIgnoredFolders: List[Regex] = List(
    s"(.*[$EscapedFileSeparator])?\\..*".r,
    s"(.*[$EscapedFileSeparator])?tests?[$EscapedFileSeparator].*".r,
    s"(.*[$EscapedFileSeparator])?CMakeFiles[$EscapedFileSeparator].*".r
  )

}
