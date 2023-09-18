package io.shiftleft.semanticcpg.language

import io.shiftleft.codepropertygraph.generated.v2.nodes.AbstractNode

import scala.jdk.CollectionConverters.*

/** Typeclass for (pretty) printing an object
  */
trait Show[A] {
  def apply(a: A): String
}

object Show {
  def default[A]: Show[A] = Default.asInstanceOf[Show[A]]

  private val Default = new Show[Any] {
    override def apply(a: Any): String = a match {
      case node: AbstractNode =>
      s"${node.label}: ${node.propertiesMap}"

      case other => other.toString
    }

  }

}
