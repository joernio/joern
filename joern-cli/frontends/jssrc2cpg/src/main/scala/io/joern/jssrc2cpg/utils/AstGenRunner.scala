package io.joern.jssrc2cpg.utils

import better.files.File
import io.joern.jssrc2cpg.Config
import io.joern.jssrc2cpg.preprocessing.EjsPreprocessor
import io.joern.x2cpg.SourceFiles
import io.joern.x2cpg.utils.{Environment, ExternalCommand}
import io.shiftleft.utils.IOUtils
import com.typesafe.config.ConfigFactory
import org.slf4j.LoggerFactory
import versionsort.VersionHelper

import java.nio.file.Paths
import java.util.regex.Pattern
import scala.util.Failure
import scala.util.Success
import scala.util.matching.Regex
import scala.util.Try

object AstGenRunner {

  private val logger = LoggerFactory.getLogger(getClass)

  private val LineLengthThreshold: Int = 10000

  private val NODE_OPTIONS: Map[String, String] = Map("NODE_OPTIONS" -> "--max-old-space-size=8192")

  private val TypeDefinitionFileExtensions = List(".t.ts", ".d.ts")

  private val MinifiedPathRegex: Regex = ".*([.-]min\\..*js|bundle\\.js)".r

  private val AstGenDefaultIgnoreRegex: Seq[Regex] =
    List(
      "(conf|test|spec|[.-]min|\\.d)\\.(js|ts|jsx|tsx)$".r,
      s"node_modules${Pattern.quote(java.io.File.separator)}.*".r,
      s"venv${Pattern.quote(java.io.File.separator)}.*".r,
      s"docs${Pattern.quote(java.io.File.separator)}.*".r,
      s"test${Pattern.quote(java.io.File.separator)}.*".r,
      s"tests${Pattern.quote(java.io.File.separator)}.*".r,
      s"e2e${Pattern.quote(java.io.File.separator)}.*".r,
      s"e2e-beta${Pattern.quote(java.io.File.separator)}.*".r,
      s"examples${Pattern.quote(java.io.File.separator)}.*".r,
      s"cypress${Pattern.quote(java.io.File.separator)}.*".r,
      s"jest-cache${Pattern.quote(java.io.File.separator)}.*".r,
      s"eslint-rules${Pattern.quote(java.io.File.separator)}.*".r,
      s"codemods${Pattern.quote(java.io.File.separator)}.*".r,
      s"flow-typed${Pattern.quote(java.io.File.separator)}.*".r,
      s"i18n${Pattern.quote(java.io.File.separator)}.*".r,
      s"vendor${Pattern.quote(java.io.File.separator)}.*".r,
      s"www${Pattern.quote(java.io.File.separator)}.*".r,
      s"dist${Pattern.quote(java.io.File.separator)}.*".r,
      s"build${Pattern.quote(java.io.File.separator)}.*".r
    )

  private val IgnoredTestsRegex: Seq[Regex] =
    List(
      ".*[.-]spec\\.js".r,
      ".*[.-]mock\\.js".r,
      ".*[.-]e2e\\.js".r,
      ".*[.-]test\\.js".r,
      ".*cypress\\.json".r,
      ".*test.*\\.json".r
    )

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
    ".*eslint-local-rules\\.js".r,
    ".*\\.devcontainer\\.json".r,
    ".*Gruntfile\\.js".r,
    ".*i18n.*\\.json".r
  )

  case class AstGenRunnerResult(
    parsedFiles: List[(String, String)] = List.empty,
    skippedFiles: List[(String, String)] = List.empty
  )

  // full path to the astgen binary from the env var ASTGEN_BIN
  private val AstGenBin: Option[String] = scala.util.Properties.envOrNone("ASTGEN_BIN").flatMap {
    case path if File(path).isDirectory => Some((File(path) / "astgen").pathAsString)
    case path if File(path).exists      => Some(File(path).pathAsString)
    case _                              => None
  }

  lazy private val executableName = (Environment.operatingSystem, Environment.architecture) match {
    case (Environment.OperatingSystemType.Windows, _)                                => "astgen-win.exe"
    case (Environment.OperatingSystemType.Linux, Environment.ArchitectureType.X86)   => "astgen-linux"
    case (Environment.OperatingSystemType.Linux, Environment.ArchitectureType.ARMv8) => "astgen-linux-arm"
    case (Environment.OperatingSystemType.Mac, Environment.ArchitectureType.X86)     => "astgen-macos"
    case (Environment.OperatingSystemType.Mac, Environment.ArchitectureType.ARMv8)   => "astgen-macos-arm"
    case _ =>
      logger.warn("Could not detect OS version! Defaulting to Linux/x86_64.")
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

  private def hasCompatibleAstGenVersionAtPath(astGenVersion: String, path: Option[String]): Boolean = {
    val astGenCommand = path.getOrElse("astgen")
    val localPath     = path.flatMap(File(_).parentOption.map(_.pathAsString)).getOrElse(".")
    val debugMsgPath  = path.getOrElse("PATH")
    ExternalCommand.run(s"$astGenCommand --version", localPath).toOption.map(_.mkString.strip()) match {
      case Some(installedVersion)
          if installedVersion != "unknown" &&
            Try(VersionHelper.compare(installedVersion, astGenVersion)).toOption.getOrElse(-1) >= 0 =>
        logger.debug(s"Found astgen v$installedVersion in '$debugMsgPath'")
        true
      case Some(installedVersion) =>
        logger.debug(
          s"Found astgen v$installedVersion in '$debugMsgPath' but jssrc2cpg requires at least v$astGenVersion"
        )
        false
      case _ =>
        false
    }
  }

  /** @return
    *   the full path to the astgen binary found on the system
    */
  private def compatibleAstGenPath(astGenVersion: String): String = {
    AstGenBin match
      // 1. case: we try it at env var ASTGEN_BIN
      case Some(path) if hasCompatibleAstGenVersionAtPath(astGenVersion, Some(path)) =>
        path
      // 2. case: we try it with the systems PATH
      case _ if hasCompatibleAstGenVersionAtPath(astGenVersion, None) =>
        "astgen"
      // otherwise: we use the default local astgen executable path
      case _ =>
        logger.debug(
          s"Did not find any astgen binary on this system (environment variable ASTGEN_BIN not set and no entry in the systems PATH)"
        )
        val localPath = s"$executableDir${java.io.File.separator}$executableName"
        localPath
  }

  private lazy val astGenCommand = {
    val conf          = ConfigFactory.load
    val astGenVersion = conf.getString("jssrc2cpg.astgen_version")
    val astGenPath    = compatibleAstGenPath(astGenVersion)
    logger.info(s"Using astgen from '$astGenPath'")
    astGenPath
  }
}

class AstGenRunner(config: Config) {

  import io.joern.jssrc2cpg.utils.AstGenRunner._

  private val executableArgs = if (!config.tsTypes) " --no-tsTypes" else ""

  private def skippedFiles(astGenOut: List[String]): List[String] = {
    val skipped = astGenOut.collect {
      case out if out.startsWith("Parsing") =>
        val filename = out.substring(out.indexOf(" ") + 1, out.indexOf(":") - 1)
        val reason   = out.substring(out.indexOf(":") + 2)
        logger.warn(s"\t- failed to parse '$filename': '$reason'")
        Option(filename)
      case out if !out.startsWith("Converted") && !out.startsWith("Retrieving") =>
        val filename = out.substring(0, out.indexOf(" "))
        val reason   = out.substring(out.indexOf(" ") + 1)
        logger.warn(s"\t- failed to parse '$filename': '$reason'")
        Option(filename)
      case out =>
        logger.debug(s"\t+ $out")
        None
    }
    skipped.flatten
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

  private def isIgnoredByDefault(filePath: String): Boolean = {
    lazy val isIgnored     = IgnoredFilesRegex.exists(_.matches(filePath))
    lazy val isIgnoredTest = IgnoredTestsRegex.exists(_.matches(filePath))
    lazy val isMinified    = isMinifiedFile(filePath)
    if (isIgnored || isIgnoredTest || isMinified) {
      logger.debug(s"'$filePath' ignored by default")
      true
    } else {
      false
    }
  }

  private def isTranspiledFile(filePath: String): Boolean = {
    val file = File(filePath)
    // We ignore files iff:
    // - they are *.js files and
    // - they contain a //sourceMappingURL comment or have an associated source map file and
    // - a file with the same name is located directly next to them
    lazy val isJsFile            = file.exists && file.extension.contains(".js")
    lazy val hasSourceMapComment = IOUtils.readLinesInFile(file.path).exists(_.contains("//sourceMappingURL"))
    lazy val hasSourceMapFile    = File(s"$filePath.map").exists
    lazy val hasSourceMap        = hasSourceMapComment || hasSourceMapFile
    lazy val hasFileWithSameName =
      file.siblings.exists(_.nameWithoutExtension(includeAll = false) == file.nameWithoutExtension)
    if (isJsFile && hasSourceMap && hasFileWithSameName) {
      logger.debug(s"'$filePath' ignored by default (seems to be the result of transpilation)")
      true
    } else {
      false
    }

  }

  private def filterFiles(files: List[String], out: File): List[String] = {
    files.filter { file =>
      file.stripSuffix(".json").replace(out.pathAsString, config.inputPath) match {
        // We are not interested in JS / TS type definition files at this stage.
        // TODO: maybe we can enable that later on and use the type definitions there
        //  for enhancing the CPG with additional type information for functions
        case filePath if TypeDefinitionFileExtensions.exists(filePath.endsWith) => false
        case filePath if isIgnoredByUserConfig(filePath)                        => false
        case filePath if isIgnoredByDefault(filePath)                           => false
        case filePath if isTranspiledFile(filePath)                             => false
        case _                                                                  => true
      }
    }
  }

  /** Changes the file-extension by renaming this file; if file does not have an extension, it adds the extension. If
    * file does not exist (or is a directory) no change is done and the current file is returned.
    */
  private def changeExtensionTo(file: File, extension: String): File = {
    val newName = s"${file.nameWithoutExtension(includeAll = false)}.${extension.stripPrefix(".")}"
    if (file.isRegularFile) file.renameTo(newName) else if (file.notExists) File(newName) else file
  }

  private def processEjsFiles(in: File, out: File, ejsFiles: List[String]): Try[Seq[String]] = {
    val tmpJsFiles = ejsFiles.map { ejsFilePath =>
      val ejsFile             = File(ejsFilePath)
      val maybeTranspiledFile = File(s"${ejsFilePath.stripSuffix(".ejs")}.js")
      if (isTranspiledFile(maybeTranspiledFile.pathAsString)) {
        maybeTranspiledFile
      } else {
        val sourceFileContent = IOUtils.readEntireFile(ejsFile.path)
        val preprocessContent = new EjsPreprocessor().preprocess(sourceFileContent)
        (out / in.relativize(ejsFile).toString).parent.createDirectoryIfNotExists(createParents = true)
        val newEjsFile = ejsFile.copyTo(out / in.relativize(ejsFile).toString)
        val jsFile     = changeExtensionTo(newEjsFile, ".js").writeText(preprocessContent)
        newEjsFile.createFile().writeText(sourceFileContent)
        jsFile
      }
    }

    val result =
      ExternalCommand.run(s"$astGenCommand$executableArgs -t ts -o $out", out.toString(), extraEnv = NODE_OPTIONS)

    val jsons = SourceFiles.determine(out.toString(), Set(".json"))
    jsons.foreach { jsonPath =>
      val jsonFile    = File(jsonPath)
      val jsonContent = IOUtils.readEntireFile(jsonFile.path)
      val json        = ujson.read(jsonContent)
      val fileName    = json("fullName").str
      val newFileName = fileName.patch(fileName.lastIndexOf(".js"), ".ejs", 3)
      json("relativeName") = newFileName
      json("fullName") = newFileName
      jsonFile.writeText(json.toString())
    }

    tmpJsFiles.foreach(_.delete())
    result
  }

  private def ejsFiles(in: File, out: File): Try[Seq[String]] = {
    val files =
      SourceFiles.determine(
        in.pathAsString,
        Set(".ejs"),
        ignoredDefaultRegex = Some(AstGenDefaultIgnoreRegex),
        ignoredFilesRegex = Some(config.ignoredFilesRegex),
        ignoredFilesPath = Some(config.ignoredFiles)
      )
    if (files.nonEmpty) processEjsFiles(in, out, files)
    else Success(Seq.empty)
  }

  private def vueFiles(in: File, out: File): Try[Seq[String]] = {
    val files = SourceFiles.determine(
      in.pathAsString,
      Set(".vue"),
      ignoredDefaultRegex = Some(AstGenDefaultIgnoreRegex),
      ignoredFilesRegex = Some(config.ignoredFilesRegex),
      ignoredFilesPath = Some(config.ignoredFiles)
    )
    if (files.nonEmpty)
      ExternalCommand.run(s"$astGenCommand$executableArgs -t vue -o $out", in.toString(), extraEnv = NODE_OPTIONS)
    else Success(Seq.empty)
  }

  private def jsFiles(in: File, out: File): Try[Seq[String]] =
    ExternalCommand.run(s"$astGenCommand$executableArgs -t ts -o $out", in.toString(), extraEnv = NODE_OPTIONS)

  private def runAstGenNative(in: File, out: File): Try[Seq[String]] = for {
    ejsResult <- ejsFiles(in, out)
    vueResult <- vueFiles(in, out)
    jsResult  <- jsFiles(in, out)
  } yield jsResult ++ vueResult ++ ejsResult

  private def checkParsedFiles(files: List[String], in: File): List[String] = {
    val numOfParsedFiles = files.size
    logger.info(s"Parsed $numOfParsedFiles files.")
    if (numOfParsedFiles == 0) {
      logger.warn("You may want to check the DEBUG logs for a list of files that are ignored by default.")
      SourceFiles.determine(
        in.pathAsString,
        Set(".js", ".ts", ".vue", ".ejs", ".jsx", ".cjs", ".mjs", ".tsx"),
        ignoredDefaultRegex = Option(AstGenDefaultIgnoreRegex)
      )
    }
    files
  }

  def execute(out: File): AstGenRunnerResult = {
    val in = File(config.inputPath)
    runAstGenNative(in, out) match {
      case Success(result) =>
        val parsed  = checkParsedFiles(filterFiles(SourceFiles.determine(out.toString(), Set(".json")), out), in)
        val skipped = skippedFiles(result.toList)
        AstGenRunnerResult(parsed.map((in.toString(), _)), skipped.map((in.toString(), _)))
      case Failure(f) =>
        logger.error("\t- running astgen failed!", f)
        AstGenRunnerResult()
    }
  }

}
