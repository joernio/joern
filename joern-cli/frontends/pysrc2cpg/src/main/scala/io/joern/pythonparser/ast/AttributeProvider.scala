package io.joern.pythonparser.ast

import io.joern.pythonparser.Token

trait AttributeProvider {
  def lineno: Int
  def col_offset: Int
  def end_lineno: Int
  def end_col_offset: Int

  override def toString: String = {
    s"$lineno,$col_offset,$end_lineno,$end_col_offset"
  }
}

class TokenAttributeProvider(startToken: Token, endToken: Token) extends AttributeProvider {
  override def lineno: Int = {
    startToken.beginLine
  }

  override def col_offset: Int = {
    startToken.beginColumn
  }

  override def end_lineno: Int = {
    endToken.endLine
  }

  override def end_col_offset: Int = {
    endToken.endColumn
  }
}

class NodeAttributeProvider(astNode: iattributes, endToken: Token) extends AttributeProvider {
  override def lineno: Int = {
    astNode.lineno
  }

  override def col_offset: Int = {
    astNode.col_offset
  }

  override def end_lineno: Int = {
    endToken.endLine
  }

  override def end_col_offset: Int = {
    endToken.endColumn
  }
}
