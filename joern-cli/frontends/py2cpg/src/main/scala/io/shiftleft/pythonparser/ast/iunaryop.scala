package io.shiftleft.pythonparser.ast

sealed trait iunaryop extends iast

case object Invert extends iunaryop {
  override def print: String = {
    "~"
  }
}

case object Not extends iunaryop {
  override def print: String = {
    "not"
  }
}

case object UAdd extends iunaryop {
  override def print: String = {
    "+"
  }
}

case object USub extends iunaryop {
  override def print: String = {
    "-"
  }
}
