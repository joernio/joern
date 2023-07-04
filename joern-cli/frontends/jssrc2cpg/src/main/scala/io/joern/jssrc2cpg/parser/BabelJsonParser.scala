package io.joern.jssrc2cpg.parser

import io.shiftleft.utils.IOUtils
import ujson.Value.Value

import java.nio.file.Path
import java.nio.file.Paths

object BabelJsonParser {

  case class ParseResult(
    filename: String,
    fullPath: String,
    json: Value,
    fileContent: String,
    typeMap: Map[Int, String]
  )

  def readFile(rootPath: Path, file: Path): ParseResult = {
    val typeMapPath = Paths.get(file.toString.replace(".json", ".typemap"))
    val typeMap = if (typeMapPath.toFile.exists()) {
      val typeMapJsonContent = IOUtils.readLinesInFile(typeMapPath).mkString
      val typeMapJson        = ujson.read(typeMapJsonContent)
      typeMapJson.obj.map { case (k, v) => k.toInt -> v.str }.toMap
    } else {
      Map.empty[Int, String]
    }

    val jsonContent       = IOUtils.readLinesInFile(file).mkString
    val json              = ujson.transform(jsonContent, JsValueVisitor)
    val filename          = json("relativeName").str
    val fullPath          = Paths.get(rootPath.toString, filename)
    val sourceFileContent = IOUtils.readEntireFile(fullPath)
    ParseResult(filename, fullPath.toString, json, sourceFileContent, typeMap)
  }

}
