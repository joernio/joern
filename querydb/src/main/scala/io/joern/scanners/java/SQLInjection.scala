package io.joern.scanners.java

import io.joern.scanners.*
import io.shiftleft.semanticcpg.language.*
import io.joern.console.*
import io.joern.macros.QueryMacros.*
import io.joern.dataflowengineoss.language.*
import io.joern.dataflowengineoss.queryengine.EngineContext

// The queries are tied to springframework
object SQLInjection extends QueryBundle {

  implicit val resolver: ICallResolver = NoResolve

  @q
  def sqlInjection()(implicit context: EngineContext): Query =
    Query.make(
      name = "sql-injection",
      author = Crew.niko,
      title = "SQL injection: A parameter is used in an insecure database API call.",
      description = """
        |An attacker controlled parameter is used in an insecure database API call.
        |
        |If the parameter is not validated and sanitized, this is a SQL injection.
        |""".stripMargin,
      score = 5,
      withStrRep({ cpg =>
        def source =
          cpg.method
            .where(_.methodReturn.evalType("org.springframework.web.servlet.ModelAndView"))
            .parameter

        def sink = cpg.method.name("query").parameter.order(1)

        sink.reachableBy(source)
      }),
      tags = List(QueryTags.sqlInjection, QueryTags.default)
    )
}
