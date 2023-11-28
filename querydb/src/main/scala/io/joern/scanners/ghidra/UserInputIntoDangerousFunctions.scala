package io.joern.scanners.ghidra

import io.joern.scanners._
import io.joern.console._
import io.joern.macros.QueryMacros._
import io.shiftleft.semanticcpg.language._
import io.joern.dataflowengineoss.language._
import io.joern.dataflowengineoss.queryengine.EngineContext

object UserInputIntoDangerousFunctions extends QueryBundle {

  implicit val resolver: ICallResolver = NoResolve

  @q
  def getenvToStrcpy()(implicit context: EngineContext): Query =
    Query.make(
      name = "getenv-to-strcpy",
      author = Crew.claudiu,
      title = "`getenv` fn arguments used in strcpy source buffer",
      description = """
          |User-input ends up in source buffer argument of strcpy, which might overflow the destination buffer.
          |""".stripMargin,
      score = 4,
      withStrRep({ cpg =>
        def source =
          cpg.call.methodFullName("getenv").cfgNext.isCall.argument(2)
        def sink = cpg.method.fullName("strcpy").parameter.index(2)
        sink.reachableBy(source)
      }),
      tags = List(QueryTags.badfn)
    )

}
