package io.joern.jssrc2cpg.parser

import io.joern.jssrc2cpg.parser.BabelAst.BabelNode
import ujson.Value

case class BabelNodeInfo(
  node: BabelNode,
  json: Value,
  code: String,
  lineNumber: Option[Integer],
  columnNumber: Option[Integer],
  lineNumberEnd: Option[Integer],
  columnNumberEnd: Option[Integer]
)
