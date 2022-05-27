package io.joern.joerncli

import better.files.File
import io.joern.console.cpgcreation.{cpgGeneratorForLanguage, guessLanguage}
import io.joern.console.{FrontendConfig, InstallConfig}
import io.joern.joerncli.CpgBasedTool.newCpgCreatedString
import io.shiftleft.codepropertygraph.generated.Languages
import scala.jdk.CollectionConverters._

object JoernParse extends App {

  // Special string used to separate joern-parse opts from frontend-specific opts
  val ARGS_DELIMITER = "--frontend-args"

  val optionParser = new scopt.OptionParser[ParserConfig]("joern-parse") {
    arg[String]("input")
      .optional()
      .text("source file or directory containing source files")
      .action((x, c) => c.copy(inputPath = x))

    opt[String]('o', "output")
      .text("output filename")
      .action((x, c) => c.copy(outputCpgFile = x))

    opt[String]("language")
      .text("source language")
      .action((x, c) => c.copy(language = x))

    opt[Unit]("list-languages")
      .text("list available language options")
      .action((_, c) => c.copy(listLanguages = true))

    opt[String]("namespaces")
      .text("namespaces to include: comma separated string")
      .action((x, c) => c.copy(namespaces = x.split(",").map(_.trim).toList))

    note("Overlay application stage")

    opt[Unit]("nooverlays")
      .text("do not apply default overlays")
      .action((x, c) => c.copy(enhance = false))
    opt[Unit]("overlaysonly")
      .text("Only apply default overlays")
      .action((x, c) => c.copy(enhanceOnly = true))

    opt[Int]("max-num-def")
      .text("Maximum number of definitions in per-method data flow calculation")
      .action((x, c) => c.copy(maxNumDef = x))

    note("Misc")
    help("help").text("display this help message")

    note(s"Args specified after the $ARGS_DELIMITER separator will be passed to the front-end verbatim")
  }

  val DEFAULT_CPG_OUT_FILE = "cpg.bin"

  val (parserArgs, frontendArgs) = CpgBasedTool.splitArgs(args)
  val installConfig              = new InstallConfig()

  run() match {
    case Right(msg) => println(msg)

    case Left(errMsg) =>
      println(s"Failure: $errMsg")
      System.exit(1)
  }

  private def run(): Either[String, String] = {
    parseConfig(parserArgs) match {
      case Right(config) =>
        if (config.listLanguages) {
          Right(buildLanguageList())
        } else
          for {
            _        <- checkInputPath(config)
            language <- getLanguage(config)
            _        <- generateCpg(config, language)
            _        <- applyDefaultOverlays(config)
          } yield newCpgCreatedString(config.outputCpgFile)

      case Left(err) => Left(err)
    }
  }

  private def checkInputPath(config: ParserConfig): Either[String, Unit] = {

    if (config.inputPath == "") {
      println(optionParser.usage)
      Left("Input path required")
    } else if (!File(config.inputPath).exists) {
      Left("Input path does not exist at `" + config.inputPath + "`, exiting.")
    } else {
      Right(())
    }
  }

  private def buildLanguageList(): String = {
    val s = new StringBuilder()
    s ++= "Available languages (case insensitive):\n"
    s ++= Languages.ALL.asScala.map(lang => s"- ${lang.toLowerCase}").mkString("\n")
    s.toString()
  }

  private def getLanguage(config: ParserConfig): Either[String, String] = {
    if (config.language.isEmpty) {
      val inputPath = config.inputPath
      guessLanguage(inputPath) match {
        case Some(guess) => Right(guess)

        case None =>
          Left(
            s"Could not guess language from input path $inputPath. Please specify a language using the --language option."
          )
      }
    } else {
      Right(config.language)
    }
  }

  private def generateCpg(config: ParserConfig, language: String): Either[String, String] = {
    if (config.enhanceOnly) {
      Right("No generation required")
    } else {
      println(s"Parsing code at: ${config.inputPath} - language: `$language`")
      println("[+] Running language frontend")
      val generator =
        cpgGeneratorForLanguage(language.toUpperCase, FrontendConfig(), installConfig.rootPath.path, frontendArgs).get
      generator.generate(config.inputPath, outputPath = config.outputCpgFile, namespaces = config.namespaces) match {
        case Some(cmd) => Right(cmd)
        case None      => Left(s"Could not generate CPG with language = $language and input = ${config.inputPath}")
      }
    }
  }

  private def applyDefaultOverlays(config: ParserConfig): Either[String, String] = {
    try {
      println("[+] Applying default overlays")
      if (config.enhance) {
        DefaultOverlays.create(config.outputCpgFile, config.maxNumDef).close()
      }
      Right("Code property graph generation successful")
    } catch {
      case err: Throwable => Left(err.getMessage)
    }
  }

  case class ParserConfig(
    inputPath: String = "",
    outputCpgFile: String = DEFAULT_CPG_OUT_FILE,
    namespaces: List[String] = List.empty,
    enhance: Boolean = true,
    enhanceOnly: Boolean = false,
    language: String = "",
    listLanguages: Boolean = false,
    maxNumDef: Int = DefaultOverlays.defaultMaxNumberOfDefinitions
  )

  private def parseConfig(parserArgs: List[String]): Either[String, ParserConfig] = {

    optionParser.parse(parserArgs, ParserConfig()) match {
      case Some(config) => Right(config)

      case None =>
        Left("Could not parse command line options")
    }
  }

}
