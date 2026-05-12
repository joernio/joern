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
    traversal.filter { _.index == num }

  def index(num: Option[Int]): Iterator[MethodParameterOut] =
    num match { case Some(value) => index(value); case None => Iterator.empty }

  /* get all parameters from (and including)
   * method parameter indexes are  based, i.e. first parameter has index  (that's how java2cpg generates it) */
  def indexFrom(num: Int): Iterator[MethodParameterOut] =
    traversal.filter(_.index >= num)

  def indexFrom(num: Option[Int]): Iterator[MethodParameterOut] =
    num match { case Some(value) => indexFrom(value); case None => Iterator.empty }

  /* get all parameters up to (and including)
   * method parameter indexes are  based, i.e. first parameter has index  (that's how java2cpg generates it) */
  def indexTo(num: Int): Iterator[MethodParameterOut] =
    traversal.filter(_.index <= num)

  def indexTo(num: Option[Int]): Iterator[MethodParameterOut] =
    num match { case Some(value) => indexTo(value); case None => Iterator.empty }

  @Doc(info = "Traverse to arguments (actual parameters) associated with this formal parameter")
  def argument(implicit callResolver: ICallResolver): Iterator[Expression] =
    paramIn.argument
}
