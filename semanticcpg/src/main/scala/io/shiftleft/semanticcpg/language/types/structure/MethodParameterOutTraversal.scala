package io.shiftleft.semanticcpg.language.types.structure

import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._
import overflowdb.traversal._

import scala.jdk.CollectionConverters._

class MethodParameterOutTraversal(val traversal: Traversal[MethodParameterOut]) extends AnyVal {

  def paramIn: Traversal[MethodParameterIn] = traversal.flatMap(_.parameterLinkIn.headOption)

  def index: Traversal[Int] = paramIn.index

  /* method parameter indexes are  based, i.e. first parameter has index  (that's how java2cpg generates it) */
  def index(num: Int): Traversal[MethodParameterOut] =
    traversal.filter(_.paramIn.index(num).nonEmpty)

  /* get all parameters from (and including)
   * method parameter indexes are  based, i.e. first parameter has index  (that's how java2cpg generates it) */
  def indexFrom(num: Int): Traversal[MethodParameterOut] =
    traversal.filter(_.parameterLinkIn.index.headOption.exists(_ >= num))

  /* get all parameters up to (and including)
   * method parameter indexes are  based, i.e. first parameter has index  (that's how java2cpg generates it) */
  def indexTo(num: Int): Traversal[MethodParameterOut] =
    traversal.filter(_.parameterLinkIn.index.headOption.exists(_ <= num))

  def argument: Traversal[Expression] =
    for {
      paramOut <- traversal
      method   <- paramOut.method
      call     <- method._callViaCallIn
      arg      <- call._argumentOut.asScala.collect { case node: Expression with HasArgumentIndex => node }
      if paramOut._parameterLinkIn.property("INDEX").headOption.contains(arg.argumentIndex)
    } yield arg

}
