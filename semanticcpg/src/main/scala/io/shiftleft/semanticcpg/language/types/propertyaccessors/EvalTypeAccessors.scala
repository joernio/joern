package io.shiftleft.semanticcpg.language.types.propertyaccessors

import io.shiftleft.codepropertygraph.generated.{EdgeTypes, Properties}
import overflowdb.traversal._
import overflowdb.traversal.filter.P
import overflowdb.{Node, toPropertyKeyOps}

class EvalTypeAccessors[A <: Node](val traversal: Traversal[A]) extends AnyVal {

  def evalType: Traversal[String] =
    traversal.out(EdgeTypes.EVAL_TYPE).out(EdgeTypes.REF).property(Properties.FULL_NAME)

  def evalType(regex: String): Traversal[A] =
    traversal.where(
      _.out(EdgeTypes.EVAL_TYPE)
        .out(EdgeTypes.REF)
        .has(Properties.FULL_NAME.where(_.matches(regex)))
    )

  def evalType(values: String*): Traversal[A] =
    if (values.isEmpty) Traversal.empty
    else {
      val regexes0 = values.map(_.r).toSet
      traversal.where(
        _.out(EdgeTypes.EVAL_TYPE)
          .out(EdgeTypes.REF)
          .has(Properties.FULL_NAME.where { value =>
            regexes0.exists(_.matches(value))
          })
      )
    }

  def evalTypeExact(value: String): Traversal[A] =
    traversal.where(
      _.out(EdgeTypes.EVAL_TYPE)
        .out(EdgeTypes.REF)
        .has(Properties.FULL_NAME, value)
    )

  def evalTypeExact(values: String*): Traversal[A] =
    if (values.isEmpty) Traversal.empty
    else {
      traversal.where(
        _.out(EdgeTypes.EVAL_TYPE)
          .out(EdgeTypes.REF)
          .has(Properties.FULL_NAME.where(P.within(values.to(Set))))
      )
    }

  def evalTypeNot(value: String): Traversal[A] =
    traversal.where(
      _.out(EdgeTypes.EVAL_TYPE)
        .out(EdgeTypes.REF)
        .hasNot(Properties.FULL_NAME.where(_.matches(value)))
    )

  def evalTypeNot(regexes: String*): Traversal[A] =
    if (regexes.isEmpty) Traversal.empty
    else {
      val regexes0 = regexes.map(_.r).toSet
      traversal.where(
        _.out(EdgeTypes.EVAL_TYPE)
          .out(EdgeTypes.REF)
          .hasNot(Properties.FULL_NAME.where { value =>
            regexes0.exists(_.matches(value))
          })
      )
    }
}
