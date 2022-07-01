package io.shiftleft.semanticcpg.testing

import io.shiftleft.codepropertygraph.generated.nodes.StoredNode
import overflowdb.{Edge, Node, Property, PropertyKey}

import java.util

/** mixin trait for test nodes */
trait DummyNodeImpl extends StoredNode {
  // Members declared in overflowdb.Element
  def graph(): overflowdb.Graph                                                                          = ???
  def property[A](x$1: overflowdb.PropertyKey[A]): A                                                     = ???
  def property(x$1: String): Object                                                                      = ???
  def propertyKeys(): java.util.Set[String]                                                              = ???
  def propertiesMap(): java.util.Map[String, Object]                                                     = ???
  def propertyOption(x$1: String): java.util.Optional[Object]                                            = ???
  def propertyOption[A](x$1: overflowdb.PropertyKey[A]): java.util.Optional[A]                           = ???
  override def addEdgeImpl(label: String, inNode: Node, keyValues: Any*): Edge                           = ???
  override def addEdgeImpl(label: String, inNode: Node, keyValues: util.Map[String, AnyRef]): Edge       = ???
  override def addEdgeSilentImpl(label: String, inNode: Node, keyValues: Any*): Unit                     = ???
  override def addEdgeSilentImpl(label: String, inNode: Node, keyValues: util.Map[String, AnyRef]): Unit = ???
  override def setPropertyImpl(key: String, value: Any): Unit                                            = ???
  override def setPropertyImpl[A](key: PropertyKey[A], value: A): Unit                                   = ???
  override def setPropertyImpl(property: Property[_]): Unit                                              = ???
  override def removePropertyImpl(key: String): Unit                                                     = ???
  override def removeImpl(): Unit                                                                        = ???

  // Members declared in scala.Equals
  def canEqual(that: Any): Boolean = ???

  def both(x$1: String*): java.util.Iterator[overflowdb.Node]  = ???
  def both(): java.util.Iterator[overflowdb.Node]              = ???
  def bothE(x$1: String*): java.util.Iterator[overflowdb.Edge] = ???
  def bothE(): java.util.Iterator[overflowdb.Edge]             = ???
  def id(): Long                                               = ???
  def in(x$1: String*): java.util.Iterator[overflowdb.Node]    = ???
  def in(): java.util.Iterator[overflowdb.Node]                = ???
  def inE(x$1: String*): java.util.Iterator[overflowdb.Edge]   = ???
  def inE(): java.util.Iterator[overflowdb.Edge]               = ???
  def out(x$1: String*): java.util.Iterator[overflowdb.Node]   = ???
  def out(): java.util.Iterator[overflowdb.Node]               = ???
  def outE(x$1: String*): java.util.Iterator[overflowdb.Edge]  = ???
  def outE(): java.util.Iterator[overflowdb.Edge]              = ???

  // Members declared in scala.Product
  def productArity: Int           = ???
  def productElement(n: Int): Any = ???

  // Members declared in io.shiftleft.codepropertygraph.generated.nodes.StoredNode
  def productElementLabel(n: Int): String     = ???
  def valueMap: java.util.Map[String, AnyRef] = ???
}
