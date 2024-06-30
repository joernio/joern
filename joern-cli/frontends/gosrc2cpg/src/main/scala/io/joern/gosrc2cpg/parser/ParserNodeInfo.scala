package io.joern.gosrc2cpg.parser

import io.joern.gosrc2cpg.parser.ParserAst.ParserNode
import io.joern.x2cpg.astgen.BaseNodeInfo
import ujson.Value

case class ParserNodeInfo(
  node: ParserNode,
  json: Value,
  code: String,
  lineNumber: Option[Int],
  columnNumber: Option[Int],
  lineNumberEnd: Option[Int],
  columnNumberEnd: Option[Int]
) extends BaseNodeInfo[ParserNode]
