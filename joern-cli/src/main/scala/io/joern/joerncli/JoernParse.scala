package io.joern.joerncli

import io.joern.console.cpgcreation.{CpgGenerator, cpgGeneratorForLanguage, guessLanguage}
import io.joern.console.{FrontendConfig, InstallConfig}
import io.joern.joerncli.CpgBasedTool.newCpgCreatedString
import io.joern.x2cpg.frontendspecific.FrontendArgsDelimitor
import io.shiftleft.codepropertygraph.generated.Languages

import java.nio.file.{Files, Paths}
import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.util.{Failure, Success, Try}

object JoernParse {
  val DefaultCpgOutFile       = "cpg.bin"
  var generator: CpgGenerator = scala.compiletime.uninitialized

  def main(args: Array[String]): Unit = {
    run(args) match {
      case Success(msg) =>
        println(msg)
      case Failure(err) =>
        err.printStackTrace()
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
      .action((x, c) => c.copy(namespaces = x.split(",").map(_.trim).toSeq))

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

    note(s"Args specified after the $FrontendArgsDelimitor separator will be passed to the front-end verbatim")
  }

  private def run(args: Array[String]): Try[String] = {
    val (parserArgs, frontendArgs) = CpgBasedTool.splitArgs(args)
    val installConfig              = new InstallConfig()

    parseConfig(parserArgs).flatMap { config =>
      if (config.listLanguages)
        Try(buildLanguageList())
      else
        run(config, frontendArgs, installConfig)
    }
  }

  def run(
    config: ParserConfig,
    frontendArgs: List[String] = List.empty,
    installConfig: InstallConfig = InstallConfig()
  ): Try[String] = {
    for {
      _        <- checkInputPath(config)
      language <- getLanguage(config)
      _        <- generateCpg(installConfig, frontendArgs, config, language)
      _        <- applyDefaultOverlays(config)
    } yield newCpgCreatedString(config.outputCpgFile)
  }

  private def checkInputPath(config: ParserConfig): Try[Unit] = {
    Try {
      if (config.inputPath == "") {
        println(optionParser.usage)
        throw new AssertionError(s"Input path required")
      } else if (!Files.exists(Paths.get(config.inputPath)))
        throw new AssertionError(s"Input path does not exist at `${config.inputPath}`, exiting.")
      else ()
    }
  }

  private def buildLanguageList(): String = {
    val s = new mutable.StringBuilder()
    s ++= "Available languages (case insensitive):\n"
    s ++= Languages.ALL.asScala.map(lang => s"- ${lang.toLowerCase}").mkString("\n")
    s.toString()
  }

  private def getLanguage(config: ParserConfig): Try[String] = {
    Try {
      if (config.language.nonEmpty) {
        config.language
      } else {
        guessLanguage(config.inputPath)
          .getOrElse(
            throw new AssertionError(
              s"Could not guess language from input path ${config.inputPath}. Please specify a language using the --language option."
            )
          )
      }
    }
  }

  private def generateCpg(
    installConfig: InstallConfig,
    frontendArgs: Seq[String],
    config: ParserConfig,
    language: String
  ): Try[String] = {
    if (config.enhanceOnly) {
      Success("No generation required")
    } else {
      println(s"Parsing code at: ${config.inputPath} - language: `$language`")
      println("[+] Running language frontend")
      Try {
        cpgGeneratorForLanguage(language.toUpperCase, FrontendConfig(), installConfig.rootPath, frontendArgs.toList).get
      }.flatMap { newGenerator =>
        generator = newGenerator
        generator
          .generate(config.inputPath, outputPath = config.outputCpgFile)
          .recover { case exception =>
            throw new RuntimeException(
              s"Could not generate CPG with language = $language and input = ${config.inputPath}",
              exception
            )
          }
      }
    }
  }

  private def applyDefaultOverlays(config: ParserConfig): Try[String] = {
    Try {
      println("[+] Applying default overlays")
      if (config.enhance) {
        val cpg = DefaultOverlays.create(config.outputCpgFile, config.maxNumDef)
        generator.applyPostProcessingPasses(cpg)
        cpg.close()
      }
      "Code property graph generation successful"
    }
  }

  case class ParserConfig(
    inputPath: String = "",
    outputCpgFile: String = DefaultCpgOutFile,
    namespaces: Seq[String] = Seq.empty,
    enhance: Boolean = true,
    enhanceOnly: Boolean = false,
    language: String = "",
    listLanguages: Boolean = false,
    maxNumDef: Int = DefaultOverlays.defaultMaxNumberOfDefinitions
  )

  private def parseConfig(parserArgs: Seq[String]): Try[ParserConfig] = {
    Try {
      optionParser
        .parse(parserArgs, ParserConfig())
        .getOrElse(
          throw new RuntimeException(s"Error while not parsing command line options: `${parserArgs.mkString(",")}`")
        )
    }
  }

}
