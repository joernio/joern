package io.joern.rust2cpg.parser

import io.joern.rust2cpg.parser.RustNodeSyntax.RustNode
import io.shiftleft.utils.IOUtils

import java.nio.charset.StandardCharsets
import java.nio.file.Path
import scala.util.Try

object RustJsonParser {

  final case class ParseResult(
    filename: String,
    fullPath: String,
    ast: RustNode,

    /** The resolved crate name this file belongs to. */
    crateName: Option[String],

    /** The fully qualified module this file belongs to excluding the crateName. It can be None when: the module is the
      * crate root, or when rust_ast_gen failed to resolve it.
      */
    modulePath: Option[String],
    fileContent: String,
    contentBytes: Array[Byte],
    loc: Int
  )

  def readFile(file: Path): Try[ParseResult] =
    Try(IOUtils.readEntireFile(file)).flatMap(readJsonString)

  def readJsonString(jsonContent: String): Try[ParseResult] = Try {
    val json              = ujson.read(jsonContent)
    val filename          = json("relativeFilePath").str
    val fullPath          = json("fullFilePath").str
    val sourceFileContent = json("content").str
    val crateName         = json.obj.get("crateName").flatMap(_.strOpt)
    val modulePath        = json.obj.get("modulePath").flatMap(_.strOpt)
    val contentBytes      = sourceFileContent.getBytes(StandardCharsets.UTF_8)
    val ast               = RustNodeSyntax.createRustNode(json("children").arr.head)
    val loc               = json("loc").num.toInt
    ParseResult(filename, fullPath, ast, crateName, modulePath, sourceFileContent, contentBytes, loc)
  }

  // Concrete nodes have `range` but no `text`. Macro-expanded nodes have `text` but a zeroed range.
  // Instead of checking if a node lives under some `macroExpansion` in the tree, we can simply check
  // whether `text` is defined.
  extension (node: RustNode) {
    def isMacroExpanded: Boolean = node.text.isDefined
  }

}
