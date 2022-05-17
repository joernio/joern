package io.joern.jssrc2cpg.parser

import io.shiftleft.utils.IOUtils
import ujson.Value.Value

import java.nio.file.Path
import java.nio.file.Paths

object BabelJsonParser {

  case class ParseResult(filename: String, fullPath: String, json: Value, fileContent: String)

  def readFile(rootPath: Path, file: Path): ParseResult = {
    val jsonContent       = IOUtils.readLinesInFile(file).mkString
    val json              = ujson.read(jsonContent)
    val filename          = json("relativeName").str
    val fullPath          = Paths.get(rootPath.toString, filename)
    val sourceFileContent = IOUtils.readLinesInFile(fullPath).mkString("", "\n", "\n")
    ParseResult(filename, fullPath.toString, json, sourceFileContent)
  }

}
