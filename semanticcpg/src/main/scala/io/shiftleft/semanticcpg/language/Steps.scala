package io.shiftleft.semanticcpg.language

import io.shiftleft.codepropertygraph.generated.nodes.{AbstractNode, StoredNode}
import org.json4s.native.Serialization.{write, writePretty}
import org.json4s.{CustomSerializer, Extraction, Formats}
import io.shiftleft.codepropertygraph.generated.help.{Doc, Traversal}
import replpp.Colors
import replpp.Operators.*

import java.util.List as JList
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

/** Base class for our DSL These are the base steps available in all steps of the query language. There are no
  * constraints on the element types, unlike e.g. [[NodeSteps]]
  */
@Traversal(elementType = classOf[AnyRef])
class Steps[A](val traversal: Iterator[A]) extends AnyVal {

  /** Execute the traversal and convert it to a mutable buffer
    */
  def toBuffer(): mutable.Buffer[A] = traversal.to(mutable.Buffer)

  /** Shorthand for `toBuffer`
    */
  def b: mutable.Buffer[A] = toBuffer()

  /** Alias for `toList`
    * @deprecated
    */
  def exec(): List[A] = traversal.toList

  /** Execute the travel and convert it to a Java stream.
    */
  def toStream(): LazyList[A] = traversal.to(LazyList)

  /** Alias for `toStream`
    */
  def s: LazyList[A] = toStream()

  /** Execute the traversal and convert it into a Java list (as opposed to the Scala list obtained via `toList`)
    */
  def jl: JList[A] = b.asJava

  /** Execute this traversal and pretty print the results. This may mean that not all properties of the node are
    * displayed or that some properties have undergone transformations to improve display. A good example is flow
    * pretty-printing. This is the only three of the methods which we may modify on a per-node-type basis, typically via
    * implicits of type Show[NodeType].
    */
  @Doc(info = "execute this traversal and pretty print the results")
  def p(implicit show: Show[A] = Show.default): List[String] =
    traversal.toList.map(show.apply)

  @Doc(info = "execute this traversal and show the pretty-printed results in `less`")
  // uses scala-repl-pp's `#|^` operator which let's `less` inherit stdin and stdout
  def browse: Unit = {
    given Colors = Colors.Default
    traversal #|^ "less"
  }

  /** Execute traversal and convert the result to json. `toJson` (export) contains the exact same information as
    * `toList`, only in json format. Typically, the user will call this method upon inspection of the results of
    * `toList` in order to export the data for processing with other tools.
    */
  @Doc(info = "execute traversal and convert the result to json")
  def toJson: String = toJson(pretty = false)

  /** Execute traversal and convert the result to pretty json. */
  @Doc(info = "execute traversal and convert the result to pretty json")
  def toJsonPretty: String = toJson(pretty = true)

  protected def toJson(pretty: Boolean): String = {
    implicit val formats: Formats = org.json4s.DefaultFormats + Steps.nodeSerializer

    val results = traversal.toList
    if (pretty) writePretty(results)
    else write(results)
  }

}

object Steps {
  private lazy val nodeSerializer = new CustomSerializer[AbstractNode](implicit format =>
    (
      { case _ => ??? }, // deserializer not required for now
      { case node: AbstractNode =>
        val elementMap = Map.newBuilder[String, Any]
        (0 until node.productArity).foreach { i =>
          val label   = node.productElementName(i)
          val element = node.productElement(i)
          elementMap.addOne(label -> element)
        }
        elementMap.addOne("_label" -> node.label)
        if (node.isInstanceOf[StoredNode]) {
          elementMap.addOne("_id" -> node.asInstanceOf[StoredNode].id())
        }
        Extraction.decompose(elementMap.result())
      }
    )
  )
}
