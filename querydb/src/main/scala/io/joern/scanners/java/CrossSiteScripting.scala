package io.joern.scanners.java

import io.joern.scanners.*
import io.shiftleft.semanticcpg.language.*
import io.joern.console.*
import io.joern.macros.QueryMacros.*
import io.joern.dataflowengineoss.language.*
import io.joern.dataflowengineoss.queryengine.EngineContext

object CrossSiteScripting extends QueryBundle {

  implicit val resolver: ICallResolver = NoResolve

  @q
  def xssServlet()(implicit context: EngineContext): Query =
    Query.make(
      name = "xss-servlet",
      author = Crew.malte,
      title = "Reflected Cross-Site Scripting: Servlet Returns HTTP Input in Response",
      description = """
        |A servlet returns a URL parameter as part of the response.
        |
        |Unless the parameter is escaped or validated in-between, this is a reflected XSS vulnerability.
        |""".stripMargin,
      score = 8,
      withStrRep({ cpg =>
        def source =
          cpg.call.methodFullNameExact(
            "javax.servlet.http.HttpServletRequest.getParameter:java.lang.String(java.lang.String)"
          )

        def responseWriter =
          cpg.call.methodFullNameExact("javax.servlet.http.HttpServletResponse.getWriter:java.io.PrintWriter()")

        def sinks =
          cpg.call
            .methodFullNameExact("java.io.PrintWriter.println:void(java.lang.String)")
            .where(_.argument(0).reachableBy(responseWriter))

        sinks.where(_.argument(1).reachableBy(source))
      }),
      tags = List(QueryTags.xss, QueryTags.default)
    )
}
