package io.shiftleft.pythonparser.ast

sealed trait ialias extends iast

case class Alias(name: String, asName: Option[String]) extends ialias {
  def this(name: String, asName: String) = {
    this(name, Option(asName))
  }
  override def print: String = {
    name + asName.map(n => " as " + n).getOrElse("")
  }
}
