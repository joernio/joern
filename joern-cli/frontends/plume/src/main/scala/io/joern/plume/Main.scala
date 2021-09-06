package io.joern.plume

import io.github.plume.oss.Extractor
import io.github.plume.oss.drivers.{DriverFactory, GraphDatabase, OverflowDbDriver}
import better.files._
import scopt.OParser
import scala.util.{Failure, Success, Try, Using}

case class Config(outputPath: String)

object Main extends App {
    val builder = OParser.builder[Config]
    import builder._
    OParser.sequence(
      programName("plume"),
      opt[String]('o', "out")
        .text("output path")
        .action((_, c) => c.copy(outputPath = true)),
      opt[Unit]("include-comments")
        .text(s"includes all comments into the CPG")
        .action((_, c) => c.copy(includeComments = true)),

      opt[Unit]("log-problems")
        .text(s"enables logging of all parse problems while generating the CPG")
        .action((_, c) => c.copy(logProblems = true)),
      opt[Unit]("log-preprocessor")
        .text(s"enables logging of all preprocessor statements while generating the CPG")
        .action((_, c) => c.copy(logPreprocessor = true)),
      opt[Unit]("print-ifdef-only")
        .text(s"prints a comma-separated list of all preprocessor ifdef and if statements; does not create a CPG")
        .action((_, c) => c.copy(printIfDefsOnly = true)),
      opt[String]("include")
        .unbounded()
        .text("header include paths")
        .action((incl, cfg) => cfg.copy(includePaths = cfg.includePaths + incl)),
    )

  // def createCpgForJava(config: JoernParse.ParserConfig): Either[String, String] = {
  //   val inputPath = config.inputPath
  //   val inFile = File(inputPath)
  //   if (!inFile.exists) {
  //     Left(s"$inputPath does not exist")
  //   } else if (!inFile.isDirectory || inFile.isRegularFile) {
  //     Left(s"$inputPath is neither a file nor a directory")
  //   } else {
  //     createCpgForInputPath(inputPath, config)
  //   }
  // }

  // private def createCpgForInputPath(inputPath: String, config: JoernParse.ParserConfig): Either[String, String] = {
  //   println(s"Creating CPG for: $inputPath")
  //   Using(DriverFactory.invoke(GraphDatabase.OVERFLOWDB).asInstanceOf[OverflowDbDriver]) { driver =>
  //     deleteIfExists(config.outputCpgFile)
  //     driver.storageLocation(config.outputCpgFile)
  //     val extractor = new Extractor(driver)
  //     extractor.load(new java.io.File(inputPath))
  //     extractor.project()
  //   } match {
  //     case Success(_) => Right("Java CPG Generation Success")
  //     case Failure(exc) => {
  //       exc.printStackTrace()
  //       Left(exc.getMessage)
  //     }
  //   }
  // }

  // private def deleteIfExists(fileName: String) = {
  //   val outFile = File(fileName)
  //   if (outFile.exists) {
  //     println(s"Output file ${fileName} exists. Removing first.")
  //     outFile.delete()
  //   }
  // }

}
