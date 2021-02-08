package io.shiftleft.pythonparser.ast

sealed trait iconstant extends iast

case class StringConstant(value: String) extends iconstant {
  override def print: String = {
    value
  }
}
case class BoolConstant(value: Boolean) extends iconstant {
  override def print: String = {
    if (value) {
      "True"
    } else {
      "False"
    }
  }
}
// Python integer class is unbounded. Lets use BigInt to
// reflect this and see whether that gets to expensive.
case class IntConstant(value: BigInt) extends iconstant {
  def this(value: String) = {
    this(BigInt(value))
  }
  override def print: String = {
    value.toString()
  }
}
case object NoneConstant extends iconstant {
  override def print: String = {
    "None"
  }
}
case object EllipsisConstant extends iconstant {
  override def print: String = {
    "..."
  }
}
