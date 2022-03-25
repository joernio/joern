package io.joern.pythonparser.ast

import io.joern.pythonparser.Token

trait AttributeProvider {
  def lineno: Int
  def col_offset: Int
  def input_offset: Int
  def end_lineno: Int
  def end_col_offset: Int
  def end_input_offset: Int

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

  override def input_offset: Int = {
    startToken.startPos
  }

  override def end_lineno: Int = {
    endToken.endLine
  }

  override def end_col_offset: Int = {
    endToken.endColumn
  }

  override def end_input_offset: Int = {
    endToken.endPos
  }
}

class NodeAttributeProvider(astNode: iattributes, endToken: Token) extends AttributeProvider {
  override def lineno: Int = {
    astNode.lineno
  }

  override def col_offset: Int = {
    astNode.col_offset
  }

  override def input_offset: Int = {
    astNode.input_offset
  }

  override def end_lineno: Int = {
    endToken.endLine
  }

  override def end_col_offset: Int = {
    endToken.endColumn
  }

  override def end_input_offset: Int = {
    endToken.endPos
  }
}
