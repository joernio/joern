package io.joern.gosrc2cpg.parser

import io.joern.gosrc2cpg.model.GoMod
import io.shiftleft.utils.IOUtils
import ujson.Value.Value

import java.nio.file.{Path, Paths}
import io.circe.parser._
import io.joern.gosrc2cpg.model.CirceEnDe.decoderModMetadata
import org.slf4j.LoggerFactory

object GoAstJsonParser {
  private val logger = LoggerFactory.getLogger(getClass)
  case class ParserResult(filename: String, fullPath: String, json: Value, fileContent: String)

  def readFile(file: Path): ParserResult = {
    val jsonContent       = IOUtils.readLinesInFile(file).mkString
    val json              = ujson.read(jsonContent)
    val fullFilePath      = json(ParserKeys.NodeFileName).str
    val filePath          = Paths.get(fullFilePath)
    val sourceFileContent = IOUtils.readEntireFile(filePath)
    ParserResult(filePath.getFileName.toString, fullFilePath, json, sourceFileContent)
  }

  def readModFile(file: Path): GoMod = {
    val jsonContent = IOUtils.readLinesInFile(file).mkString
    decode[GoMod](jsonContent) match {
      case Right(myData) =>
        myData
      case Left(error) =>
        logger.warn(s"Error decoding JSON - '${file.toString}': $error")
        null
    }
  }
}
