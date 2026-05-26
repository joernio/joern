package io.shiftleft.semanticcpg.language.types.structure

import flatgraph.help.Doc
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*

class MethodParameterOutTraversal(val traversal: Iterator[MethodParameterOut]) extends AnyVal {

  def paramIn: Iterator[MethodParameterIn] = {
    // TODO define a named step in schema
    traversal.flatMap(_.parameterLinkIn)
  }

  /* method parameter indexes are  based, i.e. first parameter has index  (that's how java2cpg generates it) */
  def index(num: Int): Iterator[MethodParameterOut] =
    traversal.filter(_.index.contains(num))

  def index(num: Option[Int]): Iterator[MethodParameterOut] =
    num.fold(traversal.filter(_.index.isEmpty))(index)

  def indexIfPresent(num: Option[Int]): Iterator[MethodParameterOut] =
    num.fold(Iterator.empty[MethodParameterOut])(index)

  /* get all parameters from (and including)
   * method parameter indexes are  based, i.e. first parameter has index  (that's how java2cpg generates it) */
  def indexFrom(num: Int): Iterator[MethodParameterOut] =
    traversal.filter(_.index.exists(_ >= num))

  /* get all parameters up to (and including)
   * method parameter indexes are  based, i.e. first parameter has index  (that's how java2cpg generates it) */
  def indexTo(num: Int): Iterator[MethodParameterOut] =
    traversal.filter(_.index.exists(_ <= num))

  @Doc(info = "Traverse to arguments (actual parameters) associated with this formal parameter")
  def argument(implicit callResolver: ICallResolver): Iterator[Expression] =
    paramIn.argument
}
