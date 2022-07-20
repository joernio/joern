package io.joern.jssrc2cpg.utils

import better.files.File
import io.joern.jssrc2cpg.Config
import io.joern.x2cpg.SourceFiles
import io.joern.x2cpg.utils.ExternalCommand
import io.shiftleft.utils.ProjectRoot
import org.slf4j.LoggerFactory

import java.nio.file.Paths
import scala.util.Failure
import scala.util.Success
import scala.util.matching.Regex
import scala.util.Try

object AstGenRunner {

  private val logger = LoggerFactory.getLogger(getClass)

  private val EXECUTABLE_NAME = if (Environment.IS_MAC) {
    "astgen-macos"
  } else if (Environment.IS_LINUX) {
    "astgen-linux"
  } else {
    "astgen-win.exe"
  }

  private val EXECUTABLE_DIR: String =
    Paths.get(ProjectRoot.relativise("joern-cli/frontends/jssrc2cpg/bin/astgen")).toAbsolutePath.toString

  private val TYPE_DEFINITION_FILE_EXTENSIONS = List(".t.ts.json", ".d.ts.json")

  private val MINIFIED_PATH_REGEX: Regex = ".*([.-]min\\.js|bundle\\.js)".r

  private val IGNORED_FOLDERS_REGEX: Seq[Regex] =
    List("__.*__".r, "\\..*".r, "jest-cache".r, "codemods".r, "e2e".r, "e2e-beta".r, "eslint-rules".r, "flow-typed".r)

  private val IGNORED_FILES_REGEX: Seq[Regex] = List(
    ".*jest\\.config.*".r,
    ".*webpack\\..*\\.js".r,
    ".*vue\\.config\\.js".r,
    ".*babel\\.config\\.js".r,
    ".*chunk-vendors.*\\.js".r, // commonly found in webpack / vue.js projects
    ".*app~.*\\.js".r,          // commonly found in webpack / vue.js projects
    ".*\\.chunk\\.js".r,        // see: https://github.com/ShiftLeftSecurity/product/issues/8197
    ".*\\.babelrc.*".r,
    ".*\\.eslint.*".r,
    ".*\\.tslint.*".r,
    ".*\\.stylelintrc\\.js".r,
    ".*rollup\\.config.*".r,
    ".*\\.types\\.js".r,
    ".*\\.cjs\\.js".r,
    ".*eslint-local-rules\\.js".r
  )

  case class AstGenRunnerResult(
    parsedFiles: List[(String, String)] = List.empty,
    skippedFiles: List[(String, String)] = List.empty
  )

  private def skippedFiles(in: File, astgenOut: List[String]): List[String] = {
    val skipped = astgenOut.collect {
      case out if !out.startsWith("Converted") =>
        val filename = out.substring(0, out.indexOf(" "))
        val reason   = out.substring(out.indexOf(" ") + 1)
        logger.warn(s"\t- failed to parse '${in / filename}': '$reason'")
        Some(filename)
      case out =>
        logger.debug(s"\t+ $out")
        None
    }
    skipped.flatten
  }

  private def ignoredByUserConfig(filePath: String, config: Config, out: File): Boolean = {
    val resolvedFilePath = filePath.stripSuffix(".json").replace(out.pathAsString, config.inputPath)
    lazy val isInIgnoredFiles = config.ignoredFiles.exists {
      case ignorePath if File(ignorePath).isDirectory => resolvedFilePath.startsWith(ignorePath)
      case ignorePath                                 => resolvedFilePath == ignorePath
    }
    lazy val isInIgnoredFileRegex = config.ignoredFilesRegex.matches(resolvedFilePath)
    if (isInIgnoredFiles || isInIgnoredFileRegex) {
      logger.debug(s"'$resolvedFilePath' ignored by user configuration")
      true
    } else {
      false
    }
  }

  private def ignoredByDefault(filePath: String, config: Config, out: File): Boolean = {
    val resolvedFilePath       = filePath.stripSuffix(".json").replace(out.pathAsString, config.inputPath)
    lazy val isInIgnoredFolder = IGNORED_FOLDERS_REGEX.exists(_.matches(resolvedFilePath))
    lazy val isIgnoredFile     = IGNORED_FILES_REGEX.exists(_.matches(resolvedFilePath))
    lazy val isMinifiedFile    = MINIFIED_PATH_REGEX.matches(resolvedFilePath)
    if (isInIgnoredFolder || isIgnoredFile || isMinifiedFile) {
      logger.debug(s"'$resolvedFilePath' ignored by default")
      true
    } else {
      false
    }
  }

  private def filterFiles(files: List[String], config: Config, out: File): List[String] = {
    files.filter {
      // We are not interested in JS / TS type definition files at this stage.
      // TODO: maybe we can enable that later on and use the type definitions there
      //  for enhancing the CPG with additional type information for functions
      case filePath if TYPE_DEFINITION_FILE_EXTENSIONS.exists(filePath.endsWith) => false
      case filePath if ignoredByUserConfig(filePath, config, out)                => false
      case filePath if ignoredByDefault(filePath, config, out)                   => false
      case _                                                                     => true
    }
  }

  private def runAstGenNative(in: File, out: File): Try[Seq[String]] =
    ExternalCommand.run(s"$EXECUTABLE_DIR/$EXECUTABLE_NAME -t ts -o $out", in.toString())

  def execute(config: Config, out: File): AstGenRunnerResult = {
    val in = File(config.inputPath)
    logger.debug(s"\t+ Running astgen in '$in' ...")
    runAstGenNative(in, out) match {
      case Success(result) =>
        val parsed  = filterFiles(SourceFiles.determine(out.toString(), Set(".json")), config, out)
        val skipped = skippedFiles(in, result.toList)
        AstGenRunnerResult(parsed.map((in.toString(), _)), skipped.map((in.toString(), _)))
      case Failure(f) =>
        logger.error("\t- running astgen failed!", f)
        AstGenRunnerResult()
    }
  }

}
