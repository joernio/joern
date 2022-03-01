package io.joern.jssrc2cpg.parser

import io.shiftleft.utils.IOUtils
import ujson.Value.Value

import java.nio.file.Path
import java.nio.file.Paths
import scala.util.Try

object BabelJsonParser {

  case class ParseResult(filename: String, fullPath: String, json: Value, fileContent: String)

  private def readSourceFile(path: Path): String = IOUtils
    .readLinesInFile(path)
    .flatMap(_.toCharArray.appendedAll(System.lineSeparator().toCharArray))
    .mkString

  def readFile(rootPath: Path, file: Path): Option[ParseResult] = {
    Try {
      val jsonContent       = IOUtils.readLinesInFile(file).mkString
      val json              = ujson.read(jsonContent)
      val filename          = json("relativeName").str
      val fullPath          = Paths.get(rootPath.toString, filename)
      val sourceFileContent = readSourceFile(fullPath)
      ParseResult(filename, fullPath.toString, json, sourceFileContent)
    }.toOption
  }

}
