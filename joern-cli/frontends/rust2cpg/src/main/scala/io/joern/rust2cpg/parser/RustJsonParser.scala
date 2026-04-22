package io.joern.rust2cpg.parser

import io.joern.rust2cpg.parser.RustNodeSyntax.RustNode
import io.shiftleft.utils.IOUtils

import java.nio.charset.StandardCharsets
import java.nio.file.{Path, Paths}
import scala.util.Try

object RustJsonParser {

  final case class ParseResult(
    filename: String,
    fullPath: String,
    ast: RustNode,
    fileContent: String,
    contentBytes: Array[Byte],
    loc: Int
  )

  def readFile(file: Path): Try[ParseResult] =
    Try(IOUtils.readEntireFile(file)).flatMap(readJsonString)

  def readJsonString(jsonContent: String): Try[ParseResult] = Try {
    val json              = ujson.read(jsonContent)
    val filename          = json("relativeFilePath").str
    val fullPath          = Paths.get(json("fullFilePath").str)
    val sourceFileContent = json("content").str
    val contentBytes      = sourceFileContent.getBytes(StandardCharsets.UTF_8)
    val ast               = RustNodeSyntax.createRustNode(json("children").arr.head)
    val loc               = json("loc").num.toInt
    ParseResult(filename, fullPath.toString, ast, sourceFileContent, contentBytes, loc)
  }

}
