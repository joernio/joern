package io.joern.jssrc2cpg.parser

import better.files.File
import io.joern.jssrc2cpg.utils.AstGenRunner.ASTGEN_OUT
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

  def readFile(file: Path): Option[ParseResult] = {
    Try {
      val jsonContent       = IOUtils.readLinesInFile(file).mkString
      val json              = ujson.read(jsonContent)
      val fullPath          = File(file.toString.replace(ASTGEN_OUT, "").replace(".json", "")).canonicalPath
      val filename          = json("relativeName").str
      val sourceFileContent = readSourceFile(Paths.get(fullPath))
      ParseResult(filename, fullPath, json, sourceFileContent)
    }.toOption
  }

}
