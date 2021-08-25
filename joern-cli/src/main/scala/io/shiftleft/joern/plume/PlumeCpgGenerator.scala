package io.shiftleft.joern.plume

import io.github.plume.oss.Extractor
import io.github.plume.oss.drivers.{DriverFactory, GraphDatabase, OverflowDbDriver}
import io.shiftleft.joern.JoernParse
import better.files._

import scala.util.{Failure, Success, Try, Using}

object PlumeCpgGenerator {

  def createCpgForJava(config: JoernParse.ParserConfig): Either[String, String] = {
    val inputPath = config.inputPath
    val inFile = File(inputPath)
    if (!inFile.exists) {
      Left(s"$inputPath does not exist")
    } else if (!inFile.isDirectory || inFile.isRegularFile) {
      Left(s"$inputPath is neither a file nor a directory")
    } else {
      createCpgForInputPath(inputPath, config)
    }
  }

  private def createCpgForInputPath(inputPath: String, config: JoernParse.ParserConfig): Either[String, String] = {
    println(s"Creating CPG for: $inputPath")
    Using(DriverFactory.invoke(GraphDatabase.OVERFLOWDB).asInstanceOf[OverflowDbDriver]) { driver =>
      deleteIfExists(config.outputCpgFile)
      driver.storageLocation(config.outputCpgFile)
      val extractor = new Extractor(driver)
      extractor.load(new java.io.File(inputPath))
      extractor.project()
    } match {
      case Success(_)   => Right("Java CPG Generation Success")
      case Failure(exc) => {
        exc.printStackTrace()
        Left(exc.getMessage)
      }
    }
  }

  private def deleteIfExists(fileName: String) = {
    val outFile = File(fileName)
    if (outFile.exists) {
      println(s"Output file ${fileName} exists. Removing first.")
      outFile.delete()
    }
  }

}
