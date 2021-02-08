package io.shiftleft.pythonparser.ast

sealed trait iboolop extends iast

object And extends iboolop {
  override def print: String = {
    "and"
  }
}

case object Or extends iboolop {
  override def print: String = {
    "or"
  }
}
