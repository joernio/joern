package io.joern.dataflowengineoss.semanticsloader

import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Method
import io.shiftleft.semanticcpg.language.*

import scala.collection.mutable

object FullNameSemantics {

  def fromList(elements: List[FlowSemantic]): FullNameSemantics = {
    new FullNameSemantics(
      mutable.Map.newBuilder
        .addAll(elements.map { e =>
          e.methodFullName -> e
        })
        .result()
    )
  }

  def empty: FullNameSemantics = fromList(List())

}

class FullNameSemantics private (methodToSemantic: mutable.Map[String, FlowSemantic]) extends Semantics {

  /** The map below keeps a mapping between results of a regex and the regex string it matches. e.g.
    *
    * `path/to/file.py:<module>.Foo.sink` -> `^path.*Foo\\.sink$`
    */
  private val regexMatchedFullNames = mutable.HashMap.empty[String, String]

  /** Initialize all the method semantics that use regex with all their regex results before query time.
    */
  override def initialize(cpg: Cpg): Unit = {
    import io.shiftleft.semanticcpg.language._

    methodToSemantic.filter(_._2.regex).foreach { case (regexString, _) =>
      cpg.method.fullName(regexString).fullName.foreach { methodMatch =>
        regexMatchedFullNames.put(methodMatch, regexString)
      }
    }
  }

  def elements: List[FlowSemantic] = methodToSemantic.values.toList

  private def forMethod(fullName: String): Option[FlowSemantic] = regexMatchedFullNames.get(fullName) match {
    case Some(matchedFullName) => methodToSemantic.get(matchedFullName)
    case None                  => methodToSemantic.get(fullName)
  }

  override def forMethod(method: Method): Option[FlowSemantic] = forMethod(method.fullName)

  def serialize: String = {
    elements
      .sortBy(_.methodFullName)
      .map { elem =>
        s"\"${elem.methodFullName}\" " + elem.mappings
          .collect { case FlowMapping(x, y) => s"$x -> $y" }
          .mkString(" ")
      }
      .mkString("\n")
  }

  /** Immutably extends the current `FullNameSemantics` with `extraFlows`.
    */
  def plus(extraFlows: List[FlowSemantic]): FullNameSemantics = FullNameSemantics.fromList(elements ++ extraFlows)

}
