package io.shiftleft.pythonparser.ast

trait iast {
  def print: String = toString
  protected def indent(printString: String, indentString: String = "\t"): String = {
    indentString + printString.replaceAll("\n", "\n" + indentString)
  }
}
trait imod extends iast

