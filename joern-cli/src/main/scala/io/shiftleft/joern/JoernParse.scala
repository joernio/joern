package io.shiftleft.joern

import better.files.File
import io.shiftleft.codepropertygraph.generated.Languages
import io.shiftleft.console.{FrontendConfig, InstallConfig}
import io.shiftleft.console.cpgcreation.{cpgGeneratorForLanguage, guessLanguage}

import scala.jdk.CollectionConverters._

object JoernParse extends App {

  // Special string used to separate joern-parse opts from frontend-specific opts
  val ARGS_DELIMITER = "--frontend-args"
  val DEFAULT_CPG_OUT_FILE = "cpg.bin"

  val (parserArgs, frontendArgs) = splitArgs()
  val installConfig = new InstallConfig()

  run() match {
    case Right(msg) => println(msg)

    case Left(errMsg) =>
      println(s"PARSE FAILED: $errMsg")
      System.exit(1)
  }

  def run(): Either[String, String] = {
    parseConfig(parserArgs) match {
      case Right(config) =>
        if (config.listLanguages) {
          Right(buildLanguageList())
        } else
          for {
            _ <- checkInputPath(config)
            language <- getLanguage(config)
            _ <- generateCpg(config, language)
            _ <- enhanceCpg(config)
          } yield "Operation success"

      case Left(err) => Left(err)
    }
  }

  def splitArgs(): (List[String], List[String]) = {
    args.indexOf(ARGS_DELIMITER) match {
      case -1 => (args.toList, Nil)

      case splitIdx =>
        val (parseOpts, frontendOpts) = args.toList.splitAt(splitIdx)
        (parseOpts, frontendOpts.tail) // Take the tail to ignore the delimiter
    }
  }

  def checkInputPath(config: ParserConfig): Either[String, Unit] = {
    if (config.inputPath == "" || !File(config.inputPath).exists) {
      Left("Invalid input path provided")
    } else {
      Right(())
    }
  }

  def buildLanguageList(): String = {
    val s = new StringBuilder()
    s ++= "Available languages (case insensitive):\n"
    s ++= Languages.ALL.asScala.map(lang => s"- ${lang.toLowerCase}").mkString("\n")
    s.toString()
  }

  def getLanguage(config: ParserConfig): Either[String, String] = {
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

  def generateCpg(config: ParserConfig, language: String): Either[String, String] = {
    if (config.enhanceOnly) {
      Right("No generation required")
    } else {
      val generator =
        cpgGeneratorForLanguage(language.toUpperCase, FrontendConfig(), installConfig.rootPath.path, frontendArgs).get
      generator.generate(config.inputPath, outputPath = config.outputCpgFile, namespaces = config.namespaces) match {
        case Some(cmd) => Right(cmd)
        case None      => Left(s"Could not generate CPG with language = $language and input = ${config.inputPath}")
      }
    }
  }

  def enhanceCpg(config: ParserConfig): Either[String, String] = {
    try {
      if (config.enhance) {
        Cpg2Scpg.run(config.outputCpgFile, config.dataFlow).close()
      }
      Right("Enhance successful")
    } catch {
      case err: Throwable => Left(err.getMessage)
    }
  }

  case class ParserConfig(inputPath: String = "",
                          outputCpgFile: String = DEFAULT_CPG_OUT_FILE,
                          namespaces: List[String] = List.empty,
                          enhance: Boolean = true,
                          dataFlow: Boolean = true,
                          enhanceOnly: Boolean = false,
                          language: String = "",
                          listLanguages: Boolean = false)

  def parseConfig(parserArgs: List[String]): Either[String, ParserConfig] =
    new scopt.OptionParser[ParserConfig]("joern-parse") {
      arg[String]("input")
        .optional()
        .text("source file or directory containing source files")
        .action((x, c) => c.copy(inputPath = x))

      opt[String]("out")
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

      note("Enhancement stage")

      opt[Unit]("noenhance")
        .text("do not run enhancement stage")
        .action((x, c) => c.copy(enhance = false))
      opt[Unit]("enhanceonly")
        .text("Only run the enhancement stage")
        .action((x, c) => c.copy(enhanceOnly = true))
      opt[Unit]("nodataflow")
        .text("do not perform data flow analysis in enhancement stage")
        .action((x, c) => c.copy(dataFlow = false))

      note("Misc")
      help("help").text("display this help message")

      note(s"Args specified after the $ARGS_DELIMITER separator will be passed to the front-end verbatim")
    }.parse(parserArgs, ParserConfig()) match {
      case Some(config) => Right(config)

      case None =>
        Left("Could not parse command line options")
    }

}
