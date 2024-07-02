package io.joern.csharpsrc2cpg

import io.joern.csharpsrc2cpg.parser.DotNetJsonAst.DotNetParserNode
import io.joern.x2cpg.astgen.BaseNodeInfo
import ujson.Value

import scala.Option

package object parser {
  case class DotNetNodeInfo(
    node: DotNetParserNode,
    json: Value,
    code: String,
    lineNumber: Option[Int],
    columnNumber: Option[Int],
    lineNumberEnd: Option[Int],
    columnNumberEnd: Option[Int]
  ) extends BaseNodeInfo[DotNetParserNode]

}
