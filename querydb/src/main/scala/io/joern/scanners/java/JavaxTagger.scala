package io.joern.scanners.java

import io.joern.console.*
import io.joern.macros.QueryMacros.*
import io.joern.scanners.{Crew, QueryTags}
import io.shiftleft.semanticcpg.language.*

/** Taggers for the javax namespace.
  */
object JavaxTagger extends QueryBundle {

  implicit val resolver: ICallResolver = NoResolve

  @q
  def javaxSources(): Query =
    Query.make(
      name = "javax-servlet-sources",
      author = Crew.dave,
      title = "Attacker-Controlled HTTP Request",
      description = """
                      |One can consider the cookies, configs, property-map of `javax.servlet.http.HttpServletRequest`s
                      |to be attacker-controlled.
                      |""".stripMargin,
      score = 8,
      withStrRep({ cpg =>
        cpg.typeDecl
          .fullNameExact("javax.servlet.http.HttpServletRequest")
          .referencingType
          .flatMap(_.evalTypeIn)
          .isParameter ++
          cpg.typeDecl
            .fullNameExact("javax.servlet.http.Cookie")
            .method
            .nameExact("getValue")
            .methodReturn ++
          cpg.method.nameExact("getServletConfig").methodReturn
      }),
      tags = List(QueryTags.taint, QueryTags.source, QueryTags.default)
    )

  @q
  def javaxSinks(): Query =
    Query.make(
      name = "javax-servlet-sinks",
      author = Crew.dave,
      title = "Sensitive HTTP Response",
      description = """
          |An attacker controlling `HttpServletResponse.sendRedirect` may
          |be able to redirect users to malicious websites.
          |""".stripMargin,
      score = 6,
      withStrRep({ cpg =>
        cpg.method
          .filter(_.fullName.startsWith("javax.servlet.http.HttpServletResponse"))
          .nameExact("sendRedirect")
          .parameter
          .argument
          .where(_.argumentIndex(1))
      }),
      tags = List(QueryTags.taint, QueryTags.sink, QueryTags.default)
    )
}
