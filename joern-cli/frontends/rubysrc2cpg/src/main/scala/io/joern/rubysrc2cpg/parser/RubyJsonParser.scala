package io.joern.rubysrc2cpg.parser

import io.joern.x2cpg.astgen.ParserResult
import io.shiftleft.utils.IOUtils

import java.nio.file.{Path, Paths}

object RubyJsonParser {

  def readFile(file: Path): ParserResult = {
    val jsonContent       = IOUtils.readLinesInFile(file).mkString
    val json              = ujson.read(jsonContent)
    val fullFilePath      = json(ParserKeys.FilePath).str
    val filePath          = Paths.get(fullFilePath)
    val relFilePath       = json(ParserKeys.RelFilePath).str
    val sourceFileContent = IOUtils.readEntireFile(filePath)
    ParserResult(relFilePath, filePath.toString, json, sourceFileContent)
  }

}
