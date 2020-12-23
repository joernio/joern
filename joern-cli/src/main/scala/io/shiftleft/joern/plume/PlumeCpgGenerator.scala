package io.shiftleft.joern.plume

import io.github.plume.oss.Extractor
import io.github.plume.oss.drivers.{DriverFactory, GraphDatabase, OverflowDbDriver}
import io.shiftleft.joern.JoernParse

import better.files._
import scala.util.Using

object PlumeCpgGenerator {

  def createCpgForJava(config: JoernParse.ParserConfig): Unit = {
    config.inputPaths.foreach { inputPath =>
      val inFile = File(inputPath)
      if (!inFile.exists) {
        println(s"Error: $inputPath does not exist")
      } else {
        if (inFile.isDirectory) {
          createCpgForDirectory(inputPath, config)
        } else if (inFile.isRegularFile) {
          createCpgForArchive(inputPath, config)
        }
      }
    }

  }

  private def createCpgForDirectory(inputPath: String, config: JoernParse.ParserConfig) = {
    Using(DriverFactory.invoke(GraphDatabase.OVERFLOWDB).asInstanceOf[OverflowDbDriver]) { driver =>
      deleteIfExists(config.outputCpgFile)
      driver.setStorageLocation(config.outputCpgFile)
      println(inputPath)
      val extractor = new Extractor(driver)
      extractor.load(new java.io.File(inputPath))
      extractor.project()
    }
  }

  private def createCpgForArchive(inputPath: String, config: JoernParse.ParserConfig) = {
    val dir = File(inputPath).unzip()
    createCpgForDirectory(dir.path.toString, config)
    dir.delete()
  }

  private def deleteIfExists(fileName: String) = {
    val outFile = File(fileName)
    if (outFile.exists) {
      println(s"Output file ${fileName} exists. Removing first.")
      outFile.delete()
    }
  }

}
