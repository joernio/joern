package io.shiftleft.joern.plume

import io.github.plume.oss.Extractor
import io.github.plume.oss.drivers.{DriverFactory, GraphDatabase, OverflowDbDriver}
import io.shiftleft.joern.JoernParse
import better.files._

import scala.util.{Failure, Success, Try, Using}

object PlumeCpgGenerator {

  def createCpgForJava(config: JoernParse.ParserConfig): Unit = {
    val (existing, nonExisting) = config.inputPaths.partition(inputPath => File(inputPath).exists)
    nonExisting.foreach(inputPath => println(s"Error: $inputPath does not exist"))
    if (existing.isEmpty) { Try { throw new RuntimeException("Not valid input paths for CPG generation") } }

    try {
      existing.foreach { inputPath =>
        val inFile = File(inputPath)
        if (inFile.isDirectory) {
          createCpgForDirectory(inputPath, config)
        } else if (inFile.isRegularFile) {
          createCpgForArchive(inputPath, config)
        } else {
          Try { throw new RuntimeException(s"$inputPath is neither a file nor a directory") }
        }
      }
    } catch {
      case exc: Exception =>
        exc.printStackTrace()
    }
  }

  private def createCpgForDirectory(inputPath: String, config: JoernParse.ParserConfig): Unit = {
    println(s"Creating CPG for directory: $inputPath")
    Using(DriverFactory.invoke(GraphDatabase.OVERFLOWDB).asInstanceOf[OverflowDbDriver]) { driver =>
      deleteIfExists(config.outputCpgFile)
      driver.setStorageLocation(config.outputCpgFile)
      val extractor = new Extractor(driver)
      extractor.load(new java.io.File(inputPath))
      extractor.project()
    } match {
      case Success(_)   =>
      case Failure(exc) => throw exc
    }
  }

  private def createCpgForArchive(inputPath: String, config: JoernParse.ParserConfig): Unit = {
    println("Archive detected. Extracting.")
    val dir = File(inputPath).unzip()
    dir.listRecursively.foreach(println)
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
