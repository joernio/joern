package io.joern.csharpsrc2cpg.parser

import io.joern.x2cpg.astgen.ParserResult
import io.shiftleft.semanticcpg.utils.FileUtil.*
import io.shiftleft.utils.IOUtils
import org.slf4j.LoggerFactory
import ujson.Value.Value

import java.nio.file.{Path, Paths}
import scala.util.Try
object DotNetJsonParser {

  def readFile(file: Path): ParserResult = {
    val jsonContent       = IOUtils.readLinesInFile(file).mkString
    val json              = ujson.read(jsonContent)
    val fullFilePath      = json(ParserKeys.FileName).str
    val filePath          = Paths.get(fullFilePath)
    val sourceFileContent = IOUtils.readEntireFile(filePath)
    ParserResult(filePath.fileName, fullFilePath, json, sourceFileContent)
  }

}
