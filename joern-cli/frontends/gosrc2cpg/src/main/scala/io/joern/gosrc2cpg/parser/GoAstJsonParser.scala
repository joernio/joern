package io.joern.gosrc2cpg.parser

import io.joern.gosrc2cpg.model.GoMod
import io.joern.x2cpg.astgen.ParserResult
import io.shiftleft.semanticcpg.utils.FileUtil.*
import io.shiftleft.utils.IOUtils
import org.slf4j.LoggerFactory
import ujson.Value.Value
import upickle.default.*

import java.nio.file.{Path, Paths}
import scala.util.{Try, Success, Failure}

object GoAstJsonParser {

  private val logger = LoggerFactory.getLogger(getClass)

  def readFile(file: Path): ParserResult = {
    val jsonContent       = IOUtils.readLinesInFile(file).mkString
    val json              = ujson.read(jsonContent)
    val fullFilePath      = json(ParserKeys.NodeFileName).str
    val filePath          = Paths.get(fullFilePath)
    val sourceFileContent = IOUtils.readEntireFile(filePath)
    ParserResult(filePath.fileName, fullFilePath, json, sourceFileContent)
  }

  def readModFile(file: Path): Option[GoMod] = {
    val jsonContent = IOUtils.readLinesInFile(file).mkString
    Try(read[GoMod](jsonContent)) match {
      case Failure(error) =>
        logger.warn(s"Error decoding JSON - '${file.toString}'", error)
        None
      case Success(x) => Some(x)
    }
  }
}
