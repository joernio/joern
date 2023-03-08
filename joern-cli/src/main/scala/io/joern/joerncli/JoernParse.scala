package io.joern.joerncli

import better.files.File
import io.joern.console.cpgcreation.{CpgGenerator, cpgGeneratorForLanguage, guessLanguage}
import io.joern.console.{FrontendConfig, InstallConfig}
import io.joern.joerncli.CpgBasedTool.newCpgCreatedString
import io.shiftleft.codepropertygraph.generated.Languages

import scala.collection.mutable
import scala.jdk.CollectionConverters._
import scala.util.{Failure, Success}

object JoernParse {
  // Special string used to separate joern-parse opts from frontend-specific opts
  val ARGS_DELIMITER          = "--frontend-args"
  val DEFAULT_CPG_OUT_FILE    = "cpg.bin"
  var generator: CpgGenerator = _

  def main(args: Array[String]) = {
    run(args) match {
      case Right(msg) => println(msg)
      case Left(errMsg) =>
        println(s"Failure: $errMsg")
        System.exit(1)
    }
  }

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
      .action((_, c) => c.copy(enhance = false))
    opt[Unit]("overlaysonly")
      .text("Only apply default overlays")
      .action((_, c) => c.copy(enhanceOnly = true))

    opt[Int]("max-num-def")
      .text("Maximum number of definitions in per-method data flow calculation")
      .action((x, c) => c.copy(maxNumDef = x))

    note("Misc")
    help("help").text("display this help message")

    note(s"Args specified after the $ARGS_DELIMITER separator will be passed to the front-end verbatim")
  }

  private def run(args: Array[String]): Either[String, String] = {
    val (parserArgs, frontendArgs) = CpgBasedTool.splitArgs(args)
    val installConfig              = new InstallConfig()

    parseConfig(parserArgs) match {
      case Right(config) =>
        if (config.listLanguages) Right(buildLanguageList())
        else run(config, frontendArgs, installConfig)

      case Left(err) => Left(err)
    }
  }

  def run(
    config: ParserConfig,
    frontendArgs: List[String] = List.empty,
    installConfig: InstallConfig = InstallConfig()
  ): Either[String, String] =
    for {
      _        <- checkInputPath(config)
      language <- getLanguage(config)
      _        <- generateCpg(installConfig, frontendArgs, config, language)
      _        <- applyDefaultOverlays(config)
    } yield newCpgCreatedString(config.outputCpgFile)

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
    val s = new mutable.StringBuilder()
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

  private def generateCpg(
    installConfig: InstallConfig,
    frontendArgs: List[String],
    config: ParserConfig,
    language: String
  ): Either[String, String] = {
    if (config.enhanceOnly) {
      Right("No generation required")
    } else {
      println(s"Parsing code at: ${config.inputPath} - language: `$language`")
      println("[+] Running language frontend")
      generator =
        cpgGeneratorForLanguage(language.toUpperCase, FrontendConfig(), installConfig.rootPath.path, frontendArgs).get
      generator.generate(config.inputPath, outputPath = config.outputCpgFile, namespaces = config.namespaces) match {
        case Success(cmd) => Right(cmd)
        case Failure(exception) =>
          Left(
            s"Could not generate CPG with language = $language and input = ${config.inputPath}: ${exception.getMessage}"
          )
      }
    }
  }

  private def applyDefaultOverlays(config: ParserConfig): Either[String, String] = {
    try {
      println("[+] Applying default overlays")
      if (config.enhance) {
        val cpg = DefaultOverlays.create(config.outputCpgFile, config.maxNumDef)
        generator.applyPostProcessingPasses(cpg)
        cpg.close()
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
