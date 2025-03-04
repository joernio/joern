package io.shiftleft.semanticcpg.language.types.propertyaccessors

import io.shiftleft.codepropertygraph.generated.nodes.AstNode
import io.shiftleft.semanticcpg.language.*

class SourceCodeAccessors[A <: AstNode](val traversal: Iterator[A]) extends AnyVal {

  def sourceCode: Iterator[String] =
    sourceCode(traversal)

  def sourceCode(regex: String): Iterator[A] = {
    traversal.where(sourceCode(_).filter(_.matches(regex)))
  }

  def sourceCode(regexes: String*): Iterator[A] =
    if (regexes.isEmpty) Iterator.empty
    else {
      val regexes0 = regexes.map(_.r).toSet
      traversal.where(sourceCode(_).filter(value => regexes0.exists(_.matches(value))))
    }

  def sourceCodeExact(value: String): Iterator[A] =
    traversal.where(sourceCode(_).filter(_ == value))

  def sourceCodeExact(values: String*): Iterator[A] =
    if (values.isEmpty) Iterator.empty
    else {
      val valuesSet = values.to(Set)
      traversal.where(sourceCode(_).filter(valuesSet.contains))
    }

  def sourceCodeNot(regex: String): Iterator[A] =
    traversal.where(sourceCode(_).filterNot(_.matches(regex)))

  def sourceCodeNot(regexes: String*): Iterator[A] =
    if (regexes.isEmpty) Iterator.empty
    else {
      val regexes0 = regexes.map(_.r).toSet
      traversal.where(sourceCode(_).filter(value => !regexes0.exists(_.matches(value))))
    }

  private def sourceCode(traversal: Iterator[A]): Iterator[String] =
    traversal.map(_.sourceCode)

}
