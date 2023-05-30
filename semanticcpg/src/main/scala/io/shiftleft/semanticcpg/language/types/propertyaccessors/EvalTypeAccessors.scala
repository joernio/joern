package io.shiftleft.semanticcpg.language.types.propertyaccessors

import io.shiftleft.codepropertygraph.generated.Properties
import io.shiftleft.codepropertygraph.generated.nodes.AstNode
import overflowdb.traversal._

class EvalTypeAccessors[A <: AstNode](val traversal: Traversal[A]) extends AnyVal {

  def evalType: Traversal[String] =
    evalType(traversal)

  def evalType(regex: String): Traversal[A] = {
    traversal.where(evalType(_).filter(_.matches(regex)))
  }

  def evalType(regexes: String*): Traversal[A] =
    if (regexes.isEmpty) Iterator.empty
    else {
      val regexes0 = regexes.map(_.r).toSet
      traversal.where(evalType(_).filter(value => regexes0.exists(_.matches(value))))
    }

  def evalTypeExact(value: String): Traversal[A] =
    traversal.where(evalType(_).filter(_ == value))

  def evalTypeExact(values: String*): Traversal[A] =
    if (values.isEmpty) Iterator.empty
    else {
      val valuesSet = values.to(Set)
      traversal.where(evalType(_).filter(valuesSet.contains))
    }

  def evalTypeNot(regex: String): Traversal[A] =
    traversal.where(evalType(_).filterNot(_.matches(regex)))

  def evalTypeNot(regexes: String*): Traversal[A] =
    if (regexes.isEmpty) Iterator.empty
    else {
      val regexes0 = regexes.map(_.r).toSet
      traversal.where(evalType(_).filter(value => !regexes0.exists(_.matches(value))))
    }

  private def evalType(traversal: Traversal[A]): Traversal[String] =
    traversal.flatMap(_._evalTypeOut).flatMap(_._refOut).property(Properties.FULL_NAME)

}
