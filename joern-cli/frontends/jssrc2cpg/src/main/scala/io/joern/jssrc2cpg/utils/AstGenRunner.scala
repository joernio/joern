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

  /** Directory names that javascript-astgen never descends into (`IGNORE_DIRS` in its `Defaults.ts`).
    *
    * astgen only sees the original source tree for plain JS / TS / Vue files. `.ejs` files are found and preprocessed
    * here, before astgen runs, so this runner has to skip these directories itself for them.
    */
  private val IgnoredDirectoryNames = List(
    "node_modules",
    "venv",
    "docs",
    "test",
    "tests",
    "e2e",
    "e2e-beta",
    "examples",
    "cypress",
    "jest-cache",
    "eslint-rules",
    "codemods",
    "flow-typed",
    "i18n",
    "vendor",
    "www",
    "dist",
    "build"
  )

  /** Matches any file below an ignored directory, at any depth, with the path relative to the input directory. */
  val AstGenDefaultIgnoreRegex: Seq[Regex] = {
    val separator = Pattern.quote(java.io.File.separator)
    IgnoredDirectoryNames.map(dir => s"(?i)(.*$separator)?${Pattern.quote(dir)}$separator.*".r)
  }

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
    val tsArgs            = if (!config.tsTypes) Seq("--no-tsTypes") else Seq.empty
    val ignoredFilesRegex = if (config.ignoredFilesRegex.toString().nonEmpty) {
      Seq("--exclude-regex", config.ignoredFilesRegex.toString())
    } else {
      Seq.empty
    }
    val ignoreFileArgs = config.ignoredFiles.flatMap(file => Seq("--exclude-file", file))
    tsArgs ++ ignoredFilesRegex ++ ignoreFileArgs
  }

  override protected def skippedFiles(in: Path, astGenOut: List[String]): List[String] = {
    val skipped = astGenOut.collect {
      // javascript-astgen prefixes every relevant log statement with 'Parsing'
      case out if out.startsWith("Parsing") =>
        val filename = out.substring(out.indexOf(" ") + 1, out.indexOf(":") - 1)
        val reason   = out.substring(out.indexOf(":") + 2)
        logger.warn(s"- failed to parse '$filename': '$reason'")
        Option(filename)
      case out =>
        logger.debug(s"- $out")
        None
    }
    skipped.flatten
  }

  /** Applies the filters that astgen cannot: user configuration for the `.ejs` sources it never sees, and the removal
    * of transpiled output. Test files, bundles, minified files, tooling configuration and type definitions are already
    * skipped by astgen itself, so no JSON is ever produced for them.
    */
  override protected def fileFilter(file: String, out: Path): Boolean = {
    Try {
      val filePath = file.stripSuffix(".json").replace(out.toString, config.inputPath)
      // Read the file's lines at most once per invocation and only when the transpiled check forces this lazy val
      // (i.e. for existing .js files).
      lazy val fileLines = IOUtils.readLinesInFile(Paths.get(filePath))
      filePath match {
        case _ if isIgnoredByUserConfig(filePath)                                      => false
        case _ if isTranspiledFile(filePath, fileLines) && !hasEjsSourceFile(filePath) => false
        case _                                                                         => true
      }
    } match {
      case Success(result)    => result
      case Failure(exception) =>
        logger.warn(s"An error occurred while processing file path $file during filtering stage : ", exception)
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

    val result = ExternalCommand.run(
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
    val files = SourceFiles.determine(
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
    ExternalCommand
      .run((astGenCommand +: executableArgs) ++ Seq("-t", "vue", "-o", out.toString), workingDir = Option(in))
      .toTry
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

  private def checkParsedFiles(files: List[String]): List[String] = {
    val numOfParsedFiles = files.size
    logger.info(s"Parsed $numOfParsedFiles files.")
    if (numOfParsedFiles == 0) {
      logger.warn(
        "No files were parsed. By default, test files, minified files, bundles, tooling configuration and " +
          "folders like 'node_modules', 'dist', 'build' or 'test' are ignored."
      )
    }
    files
  }

  override def execute(out: Path): DefaultAstGenRunnerResult = {
    val in = Paths.get(config.inputPath)
    runAstGenNative(config.inputPath, out, config.ignoredFilesRegex.toString(), "") match {
      case Success(result) =>
        val parsed  = checkParsedFiles(filterFiles(SourceFiles.determine(out.toString, Set(".json")), out))
        val skipped = skippedFiles(in, result.toList)
        DefaultAstGenRunnerResult(parsed, skipped)
      case Failure(f) =>
        logger.error("\t- running astgen failed!", f)
        val parsed = checkParsedFiles(filterFiles(SourceFiles.determine(out.toString, Set(".json")), out))
        DefaultAstGenRunnerResult(parsed, List.empty)
    }
  }

}
