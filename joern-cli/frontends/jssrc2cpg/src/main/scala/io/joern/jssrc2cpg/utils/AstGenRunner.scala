package io.joern.jssrc2cpg.utils

import better.files.File
import io.joern.jssrc2cpg.Config
import io.joern.jssrc2cpg.preprocessing.EjsPreprocessor
import io.joern.x2cpg.SourceFiles
import io.joern.x2cpg.utils.ExternalCommand
import io.shiftleft.utils.IOUtils
import com.typesafe.config.ConfigFactory
import org.slf4j.LoggerFactory
import versionsort.VersionHelper

import java.nio.file.Paths
import scala.util.Failure
import scala.util.Success
import scala.util.matching.Regex
import scala.util.Try
import scala.sys.process.stringToProcess

object AstGenRunner {

  private val logger = LoggerFactory.getLogger(getClass)

  private val LineLengthThreshold: Int = 10000

  private val TypeDefinitionFileExtensions = List(".t.ts.json", ".d.ts.json")

  private val MinifiedPathRegex: Regex = ".*([.-]min\\..*js|bundle\\.js)".r

  private val IgnoredTestsRegex: Seq[Regex] =
    List(".*[.-]spec\\.js".r, ".*[.-]mock\\.js".r, ".*[.-]e2e\\.js".r, ".*[.-]test\\.js".r)

  private val IgnoredFilesRegex: Seq[Regex] = List(
    ".*jest\\.config.*".r,
    ".*webpack\\..*\\.js".r,
    ".*vue\\.config\\.js".r,
    ".*babel\\.config\\.js".r,
    ".*chunk-vendors.*\\.js".r, // commonly found in webpack / vue.js projects
    ".*app~.*\\.js".r,          // commonly found in webpack / vue.js projects
    ".*\\.chunk\\.js".r,
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

  lazy private val executableName = Environment.operatingSystem match {
    case Environment.OperatingSystemType.Windows => "astgen-win.exe"
    case Environment.OperatingSystemType.Linux   => "astgen-linux"
    case Environment.OperatingSystemType.Mac =>
      Environment.architecture match {
        case Environment.ArchitectureType.X86 => "astgen-macos"
        case Environment.ArchitectureType.ARM => "astgen-macos-arm"
      }
    case Environment.OperatingSystemType.Unknown =>
      logger.warn("Could not detect OS version! Defaulting to 'Linux'.")
      "astgen-linux"
  }

  lazy private val executableDir: String = {
    val dir        = getClass.getProtectionDomain.getCodeSource.getLocation.toString
    val indexOfLib = dir.lastIndexOf("lib")
    val fixedDir = if (indexOfLib != -1) {
      new java.io.File(dir.substring("file:".length, indexOfLib)).toString
    } else {
      val indexOfTarget = dir.lastIndexOf("target")
      if (indexOfTarget != -1) {
        new java.io.File(dir.substring("file:".length, indexOfTarget)).toString
      } else {
        "."
      }
    }
    Paths.get(fixedDir, "/bin/astgen").toAbsolutePath.toString
  }

  private def hasCompatibleAstGenVersion(astGenVersion: String): Boolean = {
    Try("astgen --version".!!).toOption.map(_.strip()) match {
      case Some(installedVersion) if VersionHelper.compare(installedVersion, astGenVersion) >= 0 =>
        logger.debug(s"Using local astgen v$installedVersion from systems PATH")
        true
      case Some(installedVersion) =>
        logger.debug(
          s"Found local astgen v$installedVersion in systems PATH but jssrc2cpg requires at least v$astGenVersion"
        )
        false
      case _ => false
    }
  }

  private lazy val astGenCommand = {
    val conf          = ConfigFactory.load
    val astGenVersion = conf.getString("jssrc2cpg.astgen_version")
    if (hasCompatibleAstGenVersion(astGenVersion)) {
      "astgen"
    } else {
      s"$executableDir/$executableName"
    }
  }
}

class AstGenRunner(config: Config) {

  import io.joern.jssrc2cpg.utils.AstGenRunner._

  private val executableArgs = if (!config.tsTypes) " --no-tsTypes" else ""

  private def skippedFiles(in: File, astgenOut: List[String]): List[String] = {
    val skipped = astgenOut.collect {
      case out if !out.startsWith("Converted") && !out.startsWith("Retrieving") =>
        val filename = out.substring(0, out.indexOf(" "))
        val reason   = out.substring(out.indexOf(" ") + 1)
        logger.warn(s"\t- failed to parse '${in / filename}': '$reason'")
        Option(filename)
      case out =>
        logger.debug(s"\t+ $out")
        None
    }
    skipped.flatten
  }

  private def isIgnoredByUserConfig(filePath: String, config: Config, out: File): Boolean = {
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

  private def isMinifiedFile(filePath: String): Boolean = filePath match {
    case p if MinifiedPathRegex.matches(p) => true
    case p if File(p).exists && p.endsWith(".js") =>
      val lines             = IOUtils.readLinesInFile(File(filePath).path)
      val linesOfCode       = lines.size
      val longestLineLength = if (lines.isEmpty) 0 else lines.map(_.length).max
      if (longestLineLength >= LineLengthThreshold && linesOfCode <= 50) {
        logger.debug(s"'$filePath' seems to be a minified file (contains a line with length $longestLineLength)")
        true
      } else false
    case _ => false
  }

  private def isIgnoredByDefault(filePath: String, config: Config, out: File): Boolean = {
    val resolvedFilePath   = filePath.stripSuffix(".json").replace(out.pathAsString, config.inputPath)
    lazy val isIgnored     = IgnoredFilesRegex.exists(_.matches(resolvedFilePath))
    lazy val isIgnoredTest = IgnoredTestsRegex.exists(_.matches(resolvedFilePath))
    lazy val isMinified    = isMinifiedFile(resolvedFilePath)
    if (isIgnored || isIgnoredTest || isMinified) {
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
      case filePath if TypeDefinitionFileExtensions.exists(filePath.endsWith) => false
      case filePath if isIgnoredByUserConfig(filePath, config, out)           => false
      case filePath if isIgnoredByDefault(filePath, config, out)              => false
      case _                                                                  => true
    }
  }

  private def processEjsFiles(in: File, out: File, ejsFiles: List[String]): Try[Seq[String]] = {
    val tmpJsFiles = ejsFiles.map { ejsFilePath =>
      val ejsFile           = File(ejsFilePath)
      val ls                = SourceFiles.retrieveLineSeparator(ejsFilePath)
      val sourceFileContent = IOUtils.readLinesInFile(ejsFile.path).mkString("", ls, ls)
      val preprocessContent = new EjsPreprocessor().preprocess(sourceFileContent)
      (out / in.relativize(ejsFile).toString).parent.createDirectoryIfNotExists(createParents = true)
      val newEjsFile = ejsFile.copyTo(out / in.relativize(ejsFile).toString)
      val jsFile     = newEjsFile.changeExtensionTo(".js").writeText(preprocessContent)
      newEjsFile.createFile().writeText(sourceFileContent)
      jsFile
    }

    val result = ExternalCommand.run(s"$astGenCommand$executableArgs -t ts -o $out", out.toString())

    val jsons = SourceFiles.determine(out.toString(), Set(".json"))
    jsons.foreach { jsonPath =>
      val jsonFile    = File(jsonPath)
      val jsonContent = IOUtils.readLinesInFile(jsonFile.path).mkString
      val json        = ujson.read(jsonContent)
      val fileName    = json("relativeName").str
      val newFileName = fileName.replace(".js", ".ejs")
      json("relativeName") = newFileName
      jsonFile.writeText(json.toString())
    }

    tmpJsFiles.foreach(_.delete())
    result
  }

  private def ejsFiles(in: File, out: File): Try[Seq[String]] = {
    val files = SourceFiles.determine(in.pathAsString, Set(".ejs"))
    if (files.nonEmpty) processEjsFiles(in, out, files)
    else Success(Seq.empty)
  }

  private def vueFiles(in: File, out: File): Try[Seq[String]] = {
    val files = SourceFiles.determine(in.pathAsString, Set(".vue"))
    if (files.nonEmpty)
      ExternalCommand.run(s"$astGenCommand$executableArgs -t vue -i ${in.toString()} -o $out", in.toString())
    else Success(Seq.empty)
  }

  private def jsFiles(in: File, out: File): Try[Seq[String]] =
    ExternalCommand.run(s"$astGenCommand$executableArgs -t ts -i ${in.toString()} -o $out", in.toString())

  private def runAstGenNative(in: File, out: File): Try[Seq[String]] = for {
    ejsResult <- ejsFiles(in, out)
    vueResult <- vueFiles(in, out)
    jsResult  <- jsFiles(in, out)
  } yield jsResult ++ vueResult ++ ejsResult

  def execute(out: File): AstGenRunnerResult = {
    val in = File(config.inputPath)
    logger.info(s"Running astgen in '$in' ...")
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
