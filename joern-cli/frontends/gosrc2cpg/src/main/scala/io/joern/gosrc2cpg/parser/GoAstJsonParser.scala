package io.joern.gosrc2cpg.parser

import io.shiftleft.utils.IOUtils
import ujson.Value.Value

import java.nio.file.{Path, Paths}

object GoAstJsonParser {

  case class ParserResult(filename: String, fullPath: String, json: Value, fileContent: String)

  def readFile(file: Path): ParserResult = {
    val jsonContent       = IOUtils.readLinesInFile(file).mkString
    val json              = ujson.read(jsonContent)
    val fullFilePath      = json(ParserKeys.NodeFileName).str
    val filePath          = Paths.get(fullFilePath)
    val sourceFileContent = IOUtils.readEntireFile(filePath)
    ParserResult(filePath.getFileName.toString, fullFilePath, json, sourceFileContent)
  }
}
