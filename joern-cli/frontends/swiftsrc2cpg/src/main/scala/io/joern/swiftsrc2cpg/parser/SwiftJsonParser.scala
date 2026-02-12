package io.joern.swiftsrc2cpg.parser

import io.joern.swiftsrc2cpg.parser.SwiftNodeSyntax.SwiftNode
import io.shiftleft.utils.IOUtils

import java.nio.charset.StandardCharsets
import java.nio.file.Path
import java.nio.file.Paths
import scala.util.Try

object SwiftJsonParser {

  case class ParseResult(
    filename: String,
    fullPath: String,
    ast: SwiftNode,
    fileContent: String,
    contentBytes: Array[Byte],
    loc: Int
  )

  def readFile(file: Path): Try[ParseResult] = Try {
    val jsonContent       = IOUtils.readEntireFile(file)
    val json              = ujson.read(jsonContent)
    val filename          = json("relativeFilePath").str
    val fullPath          = Paths.get(json("fullFilePath").str)
    val sourceFileContent = json("content").str
    val contentBytes      = sourceFileContent.getBytes(StandardCharsets.UTF_8)
    val ast               = SwiftNodeSyntax.createSwiftNode(json)
    val loc               = json("loc").num.toInt
    ParseResult(filename, fullPath.toString, ast, sourceFileContent, contentBytes, loc)
  }

}
