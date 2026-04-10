package io.joern.swiftsrc2cpg.parser

import com.google.gson.Strictness
import com.google.gson.stream.JsonReader
import io.joern.swiftsrc2cpg.parser.SwiftNodeSyntax.SwiftNode
import io.shiftleft.utils.IOUtils

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
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

  /** Lightweight extraction of only the "relativeFilePath" field using streaming JSON parsing. Scans top-level keys and
    * returns as soon as the field is found, without parsing the rest of the file.
    */
  def readRelativeFilePath(file: Path): Try[String] = Try {
    val reader = new JsonReader(Files.newBufferedReader(file, StandardCharsets.UTF_8))
    try {
      reader.setStrictness(Strictness.LENIENT)
      reader.beginObject()
      var result: String = null
      while (result == null && reader.hasNext) {
        val name = reader.nextName()
        if (name == "relativeFilePath") {
          result = reader.nextString()
        } else {
          reader.skipValue()
        }
      }
      if (result == null) throw new NoSuchElementException(s"'relativeFilePath' not found in '$file'")
      result
    } finally {
      reader.close()
    }
  }

}
