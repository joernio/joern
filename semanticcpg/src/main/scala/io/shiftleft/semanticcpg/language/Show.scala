package io.shiftleft.semanticcpg.language

import io.shiftleft.codepropertygraph.generated.nodes.NewNode
import overflowdb.Node

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
      case node: NewNode =>
        val label      = node.label
        val properties = propsToString(node.properties.toList)
        s"($label): $properties"

      case node: Node =>
        val label      = node.label
        val id         = node.id().toString
        val properties = propsToString(node.propertiesMap.asScala.toList)
        s"($label,$id): $properties"

      case other => other.toString
    }

    private def propsToString(keyValues: List[(String, Any)]): String = {
      keyValues
        .filter(_._2.toString.nonEmpty)
        .sortBy(_._1)
        .map { case (key, value) => s"$key: $value" }
        .mkString(", ")
    }
  }

}
