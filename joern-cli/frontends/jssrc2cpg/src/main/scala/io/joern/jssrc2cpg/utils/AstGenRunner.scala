package io.joern.jssrc2cpg.utils

import io.joern.jssrc2cpg.Config
import io.joern.jssrc2cpg.preprocessing.EjsPreprocessor
import io.joern.x2cpg.SourceFiles
import io.joern.x2cpg.astgen.AstGenRunner.{AstGenProgramMetaData, DefaultAstGenRunnerResult}
import io.shiftleft.semanticcpg.utils.FileUtil.*
import io.shiftleft.semanticcpg.utils.{ExternalCommand, FileUtil}
import io.shiftleft.utils.IOUtils
import org.slf4j.LoggerFactory

import java.nio.file.{Files, Path, Paths}
import java.util.regex.Pattern
import scala.util.{Failure, Success, Try}
import scala.util.matching.Regex

object AstGenRunner {

  private val LineLengthThreshold: Int = 10000

  private val TypeDefinitionFileExtensions = List(".t.ts", ".d.ts")

  private val MinifiedPathRegex: Regex = ".*([.-]min\\..*js|bundle\\.js)".r

  private val SourceFileExtensions =
    Set(".js", ".jsx", ".cjs", ".mjs", ".xsjs", ".xsjslib", ".ts", ".tsx", ".vue", ".ejs")

  val AstGenDefaultIgnoreRegex: Seq[Regex] =
    List(
      "(conf|test|spec|[.-]min|\\.d)\\.(js|jsx|cjs|mjs|xsjs|xsjslib|ts|tsx)$".r,
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

  private object astGenMetaData
      extends AstGenProgramMetaData(
        name = "astgen",
        configPrefix = "jssrc2cpg",
        binEnvVar = Some("ASTGEN_BIN"),
        versionFlag = "--version"
      )
}

class AstGenRunner(config: Config) extends io.joern.x2cpg.astgen.AstGenRunner(AstGenRunner.astGenMetaData, config) {

  import io.joern.jssrc2cpg.utils.AstGenRunner.*

  private val logger = LoggerFactory.getLogger(getClass)

  private val executableArgs = {
    val tsArgs = if (!config.tsTypes) Seq("--no-tsTypes") else Seq.empty
    val ignoredFilesRegex = if (config.ignoredFilesRegex.toString().nonEmpty) {
      Seq("--exclude-regex", config.ignoredFilesRegex.toString())
    } else {
      Seq.empty
    }
    val ignoreFileArgs =
      if (config.ignoredFiles.nonEmpty) Seq("--exclude-file") ++ config.ignoredFiles.map(f => s"\"$f\"") else Seq.empty
    tsArgs ++ ignoredFilesRegex ++ ignoreFileArgs
  }

  override protected def skippedFiles(in: Path, astGenOut: List[String]): List[String] = {
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

  override protected def fileFilter(file: String, out: Path): Boolean = {
    Try {
      val filePath = file.stripSuffix(".json").replace(out.toString, config.inputPath)
      // The minified and transpiled checks both need the file's lines; read them at most once per invocation and only
      // when a check actually forces this lazy val (i.e. for existing .js files).
      lazy val fileLines = IOUtils.readLinesInFile(Paths.get(filePath))
      filePath match {
        // We are not interested in JS / TS type definition files at this stage.
        // TODO: maybe we can enable that later on and use the type definitions there
        //  for enhancing the CPG with additional type information for functions
        case _ if TypeDefinitionFileExtensions.exists(filePath.endsWith)               => false
        case _ if isIgnoredByUserConfig(filePath)                                      => false
        case _ if isIgnoredByDefault(filePath, fileLines)                              => false
        case _ if isTranspiledFile(filePath, fileLines) && !hasEjsSourceFile(filePath) => false
        case _                                                                         => true
      }
    } match {
      case Success(result) => result
      case Failure(exception) =>
        logger.warn(s"An error occurred while processing file path $file during filtering stage : ", exception)
        false
    }
  }

  private def isMinifiedFile(filePath: String, fileLines: => Seq[String]): Boolean = filePath match {
    case p if MinifiedPathRegex.matches(p) => true
    case p if Files.exists(Paths.get(p)) && p.endsWith(".js") =>
      val lines             = fileLines
      val linesOfCode       = lines.size
      val longestLineLength = if (lines.isEmpty) 0 else lines.map(_.length).max
      if (longestLineLength >= LineLengthThreshold && linesOfCode <= 50) {
        logger.debug(s"'$filePath' seems to be a minified file (contains a line with length $longestLineLength)")
        true
      } else false
    case _ => false
  }

  private def isIgnoredByDefault(filePath: String, fileLines: => Seq[String]): Boolean = {
    lazy val isIgnored     = IgnoredFilesRegex.exists(_.matches(filePath))
    lazy val isIgnoredTest = IgnoredTestsRegex.exists(_.matches(filePath))
    lazy val isMinified    = isMinifiedFile(filePath, fileLines)
    if (isIgnored || isIgnoredTest || isMinified) {
      logger.debug(s"'$filePath' ignored by default")
      true
    } else {
      false
    }
  }

  private def isTranspiledFile(filePath: String, fileLines: => Seq[String]): Boolean = {
    val file = Paths.get(filePath)
    // We ignore files iff:
    // - they are *.js files and
    // - they contain a //sourceMappingURL comment or have an associated source map file and
    // - a file with the same name is located directly next to them
    lazy val isJsFile            = Files.exists(file) && file.extension().contains(".js")
    lazy val hasSourceMapComment = fileLines.exists(_.contains("//sourceMappingURL"))
    lazy val hasSourceMapFile    = Files.exists(Paths.get(s"$filePath.map"))
    lazy val hasSourceMap        = hasSourceMapComment || hasSourceMapFile
    lazy val hasFileWithSameName =
      file.getParent
        .listFiles()
        .filterNot(_ == file)
        .exists(_.nameWithoutExtension(includeAll = false) == file.nameWithoutExtension())
    if (isJsFile && hasSourceMap && hasFileWithSameName) {
      logger.debug(s"'$filePath' ignored by default (seems to be the result of transpilation)")
      true
    } else {
      false
    }
  }

  private def hasEjsSourceFile(filePath: String): Boolean = {
    val file = Paths.get(filePath)
    if (file.extension().contains(".js")) {
      val vueFilePath = file.getParent
        .listFiles()
        .find { siblingFile =>
          siblingFile.nameWithoutExtension(includeAll = false) == file.nameWithoutExtension() &&
          siblingFile.extension().contains(".ejs")
        }
      vueFilePath.isDefined
    } else {
      false
    }
  }

  /** Changes the file-extension by renaming this file; if file does not have an extension, it adds the extension. If
    * file does not exist (or is a directory) no change is done and the current file is returned.
    */
  private def changeExtensionTo(file: Path, extension: String): Path = {
    val newName = s"${file.nameWithoutExtension(includeAll = false)}.${extension.stripPrefix(".")}"
    if (Files.isRegularFile(file)) Files.move(file, file.resolveSibling(newName))
    else if (Files.notExists(file)) Paths.get(newName)
    else file
  }

  private def processEjsFiles(in: Path, out: Path, ejsFiles: List[String]): Try[Seq[String]] = {
    val tmpJsFiles = ejsFiles.flatMap { ejsFilePath =>
      val ejsFile             = Paths.get(ejsFilePath)
      val maybeTranspiledFile = Paths.get(s"${ejsFilePath.stripSuffix(".ejs")}.js")
      if (!isTranspiledFile(maybeTranspiledFile.toString, IOUtils.readLinesInFile(maybeTranspiledFile))) {
        val sourceFileContent = IOUtils.readEntireFile(ejsFile)
        val preprocessContent = new EjsPreprocessor().preprocess(sourceFileContent)
        (out / in.relativize(ejsFile).toString).getParent
          .createWithParentsIfNotExists(asDirectory = true, createParents = true)
        val newEjsFile = out / in.relativize(ejsFile).toString
        ejsFile.copyTo(newEjsFile)

        val jsFile  = Files.writeString(changeExtensionTo(newEjsFile, ".js"), preprocessContent)
        val newFile = Files.createFile(newEjsFile)
        Files.writeString(newFile, sourceFileContent)
        Some(jsFile)
      } else None
    }

    val result =
      ExternalCommand.run(
        (astGenCommand +: executableArgs) ++ Seq("-t", "ts", "-o", out.toString),
        workingDir = Option(out)
      )

    val jsons = SourceFiles.determine(out.toString, Set(".json"))
    jsons.foreach { jsonPath =>
      val jsonFile        = Paths.get(jsonPath)
      val jsonContent     = IOUtils.readEntireFile(jsonFile)
      val json            = ujson.read(jsonContent)
      val fullName        = json("fullName").str
      val relativeName    = json("relativeName").str
      val newFullName     = fullName.patch(fullName.lastIndexOf(".js"), ".ejs", 3)
      val newRelativeName = relativeName.patch(relativeName.lastIndexOf(".js"), ".ejs", 3)
      json("relativeName") = newRelativeName
      json("fullName") = newFullName
      Files.writeString(jsonFile, json.toString)
    }

    tmpJsFiles.foreach(FileUtil.delete(_))
    result.toTry
  }

  private def ejsFiles(in: Path, out: Path): Try[Seq[String]] = {
    val files =
      SourceFiles.determine(
        in.toString,
        Set(".ejs"),
        ignoredDefaultRegex = Some(AstGenDefaultIgnoreRegex),
        ignoredFilesRegex = Some(config.ignoredFilesRegex),
        ignoredFilesPath = Some(config.ignoredFiles)
      )
    if (files.nonEmpty) processEjsFiles(in, out, files)
    else Success(Seq.empty)
  }

  private def vueFiles(in: Path, out: Path): Try[Seq[String]] = {
    val files = SourceFiles.determine(
      in.toString,
      Set(".vue"),
      ignoredDefaultRegex = Some(AstGenDefaultIgnoreRegex),
      ignoredFilesRegex = Some(config.ignoredFilesRegex),
      ignoredFilesPath = Some(config.ignoredFiles)
    )
    if (files.nonEmpty) {
      ExternalCommand
        .run((astGenCommand +: executableArgs) ++ Seq("-t", "vue", "-o", out.toString), workingDir = Option(in))
        .toTry
    } else {
      Success(Seq.empty)
    }
  }

  private def jsFiles(in: Path, out: Path): Try[Seq[String]] = {
    ExternalCommand
      .run((astGenCommand +: executableArgs) ++ Seq("-t", "ts", "-o", out.toString), workingDir = Option(in))
      .toTry
  }

  override protected def runAstGenNative(in: String, out: Path, exclude: String, include: String): Try[Seq[String]] = {
    val inPath = Paths.get(in)
    for {
      ejsResult <- ejsFiles(inPath, out)
      vueResult <- vueFiles(inPath, out)
      jsResult  <- jsFiles(inPath, out)
    } yield jsResult ++ vueResult ++ ejsResult
  }

  private def checkParsedFiles(files: List[String], in: Path): List[String] = {
    val numOfParsedFiles = files.size
    logger.info(s"Parsed $numOfParsedFiles files.")
    if (numOfParsedFiles == 0) {
      logger.warn("You may want to check the DEBUG logs for a list of files that are ignored by default.")
      SourceFiles.determine(in.toString, SourceFileExtensions, ignoredDefaultRegex = Option(AstGenDefaultIgnoreRegex))
    }
    files
  }

  override def execute(out: Path): DefaultAstGenRunnerResult = {
    val in = Paths.get(config.inputPath)
    runAstGenNative(config.inputPath, out, config.ignoredFilesRegex.toString(), "") match {
      case Success(result) =>
        val parsed  = checkParsedFiles(filterFiles(SourceFiles.determine(out.toString, Set(".json")), out), in)
        val skipped = skippedFiles(in, result.toList)
        DefaultAstGenRunnerResult(parsed, skipped)
      case Failure(f) =>
        logger.error("\t- running astgen failed!", f)
        val parsed = checkParsedFiles(filterFiles(SourceFiles.determine(out.toString, Set(".json")), out), in)
        DefaultAstGenRunnerResult(parsed, List.empty)
    }
  }

}
