package io.joern.scanners.c

import io.joern.scanners._
import io.joern.console._
import io.joern.dataflowengineoss.language._
import io.shiftleft.semanticcpg.language._
import io.joern.macros.QueryMacros._
import io.joern.dataflowengineoss.queryengine.EngineContext
import io.joern.dataflowengineoss.semanticsloader.Semantics
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes._

/** Just to make sure that we support reachableBy queries, which did not work before
  * https://github.com/joernio/joern/pull/791 initially reported in https://github.com/joernio/joern/issues/751
  */
object QueryWithReachableBy extends QueryBundle {

  @q
  def test()(implicit context: EngineContext): Query =
    Query.make(
      name = "test",
      author = "me",
      title = "test",
      description = """
        |test
        |""".stripMargin,
      score = 2,
      withStrRep({ cpg =>
        cpg.call.argument(1).reachableBy(cpg.call.argument(1))
      }),
      tags = List(QueryTags.default)
    )

}
