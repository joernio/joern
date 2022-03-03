package io.joern.pythonparser.ast

import io.joern.pythonparser.Token

trait AttributeProvider {
  def lineno: Int
  def col_offset: Int

  override def toString: String = {
    s"$lineno,$col_offset"
  }
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
