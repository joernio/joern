package io.joern.gosrc2cpg.parser

import io.circe.parser.*
import io.joern.gosrc2cpg.model.CirceEnDe.decoderModMetadata
import io.joern.gosrc2cpg.model.GoMod
import io.joern.x2cpg.astgen.ParserResult
import io.shiftleft.utils.IOUtils
import org.slf4j.LoggerFactory
import ujson.Value.Value

import java.nio.file.{Path, Paths}
import scala.util.Try

object GoAstJsonParser {

  private val logger = LoggerFactory.getLogger(getClass)

  def readFile(file: Path): ParserResult = {
    val jsonContent       = IOUtils.readLinesInFile(file).mkString
    val json              = ujson.read(jsonContent)
    val fullFilePath      = json(ParserKeys.NodeFileName).str
    val filePath          = Paths.get(fullFilePath)
    val sourceFileContent = IOUtils.readEntireFile(filePath)
    ParserResult(filePath.getFileName.toString, fullFilePath, json, sourceFileContent)
  }

  def readModFile(file: Path): Option[GoMod] = {
    val jsonContent = IOUtils.readLinesInFile(file).mkString
    (decode[GoMod](jsonContent) match {
      case Left(error) =>
        logger.warn(s"Error decoding JSON - '${file.toString}': $error")
        Left(error)
      case x => x
    }).toOption
  }
}
