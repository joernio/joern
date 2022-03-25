package io.joern.pysrc2cpg

import io.joern.pythonparser.ast

class NodeToCode(content: String) {
  def getCode(node: ast.iattributes): String = {
    content.substring(node.input_offset, node.end_input_offset)
  }
}
