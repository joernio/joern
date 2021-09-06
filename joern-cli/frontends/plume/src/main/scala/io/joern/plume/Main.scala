package io.joern.plume

import io.github.plume.oss.Extractor
import io.github.plume.oss.drivers.{DriverFactory, GraphDatabase, OverflowDbDriver}
import better.files._
import scopt.OParser
import scala.util.{Failure, Success, Try, Using}

case class Config(inputPath: String = "", outputPath: String = "cpg.odb")

object Main extends App {
  val builder = OParser.builder[Config]
  import builder._
  val parser = OParser.sequence(
    programName("plume"),
    head("plume"),
    arg[String]("input")
      .text("input jar")
      .required()
      .action((x, c) => c.copy(inputPath = x)),
    opt[String]('o', "out")
      .text("output path - defaults to `cpg.odb`")
      .action((x, c) => c.copy(outputPath = x)),
  )

  OParser.parse(parser, args, Config()) match {
    case Some(Config(input, output)) =>
      println(s"invoking Plume on $input, writing results to $output")
      val driver = DriverFactory.invoke(GraphDatabase.OVERFLOWDB).asInstanceOf[OverflowDbDriver]
      deleteIfExists(output)
      driver.storageLocation(output)
      val extractor = new Extractor(driver)
      extractor.load(new java.io.File(input))
      extractor.project()
    case _ =>
    // arguments are bad, error message will have been displayed
  }

  private def deleteIfExists(fileName: String) = {
    val outFile = File(fileName)
    if (outFile.exists) {
      println(s"Output file ${fileName} exists. Removing first.")
      outFile.delete()
    }
  }
}
