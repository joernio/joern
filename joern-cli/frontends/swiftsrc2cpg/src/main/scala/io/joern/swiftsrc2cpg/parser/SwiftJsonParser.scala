package io.joern.swiftsrc2cpg.parser

import io.joern.swiftsrc2cpg.parser.SwiftNodeSyntax.SwiftNode
import io.shiftleft.utils.IOUtils

import java.nio.file.Path
import java.nio.file.Paths

object SwiftJsonParser {

  case class ParseResult(filename: String, fullPath: String, ast: SwiftNode, fileContent: String)

  def readFile(rootPath: Path, file: Path): ParseResult = {
    val jsonContent       = IOUtils.readEntireFile(file)
    val json              = ujson.read(jsonContent)
    val filename          = json("relativeFilePath").str
    val fullPath          = Paths.get(json("fullFilePath").str)
    val sourceFileContent = IOUtils.readEntireFile(fullPath)
    val ast               = SwiftNodeSyntax.createSwiftNode(json)
    ParseResult(filename, fullPath.toString, ast, sourceFileContent)
  }

}
