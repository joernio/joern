package io.joern.jssrc2cpg.parser

import io.joern.x2cpg.astgen.BaseParserResult
import io.shiftleft.utils.IOUtils
import ujson.Value.Value

import java.nio.file.{Path, Paths}
import scala.util.Try

object BabelJsonParser {

  case class ParseResult(
    filename: String,
    fullPath: String,
    json: Value,
    fileContent: String,
    typeMap: Map[String, String],
    fileLoc: Int
  ) extends BaseParserResult

  /** Strips the last file extension, i.e. everything after and including the final '.'. */
  def stripExtension(path: String): String = {
    val dotIndex = path.lastIndexOf('.')
    if (dotIndex >= 0) path.substring(0, dotIndex) else path
  }

  private def loadTypeMap(file: Path): Try[Map[String, String]] = Try {
    val typeMapPathString = stripExtension(file.toString) + ".typemap"
    val typeMapPath       = Paths.get(typeMapPathString)
    if (typeMapPath.toFile.exists()) {
      val typeMapJsonContent = IOUtils.readEntireFile(typeMapPath)
      val typeMapJson        = ujson.read(typeMapJsonContent)
      typeMapJson.obj.map { case (k, v) => k -> v.str }.toMap
    } else {
      Map.empty
    }
  }

  private def loadJson(file: Path): Try[Value] = Try {
    val jsonContent = IOUtils.readEntireFile(file)
    ujson.read(jsonContent)
  }

  private def generateParserResult(rootPath: Path, json: Value, typeMap: Map[String, String]): Try[ParseResult] = Try {
    val filename          = json("relativeName").str
    val fullPath          = Paths.get(rootPath.toString, filename)
    val sourceFileContent = IOUtils.readEntireFile(fullPath)
    val fileLoc           = sourceFileContent.lines().count().toInt
    ParseResult(filename, fullPath.toString, json, sourceFileContent, typeMap, fileLoc)
  }

  def readFile(rootPath: Path, file: Path): Try[ParseResult] = {
    val typeMap = loadTypeMap(file).getOrElse(Map.empty)
    for {
      json        <- loadJson(file)
      parseResult <- generateParserResult(rootPath, json, typeMap)
    } yield parseResult
  }

}
