package io.shiftleft.semanticcpg.language.types.structure

import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._

import scala.jdk.CollectionConverters._

class MethodParameterOutTraversal(val traversal: Traversal[MethodParameterOut]) extends AnyVal {

  def paramIn: Traversal[MethodParameterIn] = traversal.flatMap(_.parameterLinkIn.headOption)

  /* method parameter indexes are  based, i.e. first parameter has index  (that's how java2cpg generates it) */
  def index(num: Int): Traversal[MethodParameterOut] =
    traversal.filter { _.index == num }

  /* get all parameters from (and including)
   * method parameter indexes are  based, i.e. first parameter has index  (that's how java2cpg generates it) */
  def indexFrom(num: Int): Traversal[MethodParameterOut] =
    traversal.filter(_.index >= num)

  /* get all parameters up to (and including)
   * method parameter indexes are  based, i.e. first parameter has index  (that's how java2cpg generates it) */
  def indexTo(num: Int): Traversal[MethodParameterOut] =
    traversal.filter(_.index <= num)

  def argument: Traversal[Expression] =
    for {
      paramOut <- traversal
      method = paramOut.method
      call <- method.callIn
      arg  <- call.argumentOut.collectAll[Expression]
      if paramOut.parameterLinkIn.index.headOption.contains(arg.argumentIndex)
    } yield arg

}
