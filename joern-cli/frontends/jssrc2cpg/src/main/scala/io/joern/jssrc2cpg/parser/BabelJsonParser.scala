package io.joern.jssrc2cpg.parser

import io.joern.jssrc2cpg.utils.JsIOUtils
import ujson.Value.Value

import java.nio.file.Path
import java.nio.file.Paths

object BabelJsonParser {

  case class ParseResult(filename: String, fullPath: String, json: Value, fileContent: String)

  def readFile(rootPath: Path, file: Path): ParseResult = {
    val json     = ujson.read(file)
    val filename = json("relativeName").str
    val fullPath = Paths.get(rootPath.toString, filename)
    val result   = JsIOUtils.readFile(fullPath)
    ParseResult(filename, fullPath.toString, json, result)
  }

}
