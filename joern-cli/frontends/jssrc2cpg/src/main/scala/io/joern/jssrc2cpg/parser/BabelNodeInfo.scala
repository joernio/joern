package io.joern.jssrc2cpg.parser

import io.joern.jssrc2cpg.parser.BabelAst.BabelNode
import ujson.Value

case class BabelNodeInfo(
  node: BabelNode,
  json: Value,
  code: String,
  lineNumber: Option[Int],
  columnNumber: Option[Int],
  lineNumberEnd: Option[Int],
  columnNumberEnd: Option[Int]
)
