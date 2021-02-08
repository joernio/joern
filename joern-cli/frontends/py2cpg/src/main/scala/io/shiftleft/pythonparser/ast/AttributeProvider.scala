package io.shiftleft.pythonparser.ast

import io.shiftleft.pythonparser.Token

trait AttributeProvider {
  def lineno: Int
  def col_offset: Int
}

class TokenAttributeProvider(token: Token) extends AttributeProvider {
  override def lineno: Int = {
    token.beginLine
  }

  override def col_offset: Int = {
    token.beginColumn
  }
}

class NodeAttributeProvider(astNode: iattributes) extends AttributeProvider {
  override def lineno: Int = {
    astNode.lineno
  }

  override def col_offset: Int = {
    astNode.col_offset
  }
}
