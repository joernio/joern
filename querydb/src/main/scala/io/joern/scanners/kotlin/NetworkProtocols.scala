package io.joern.scanners.kotlin

import io.joern.console.*
import io.joern.macros.QueryMacros.*
import io.joern.scanners.{Crew, QueryTags}
import io.shiftleft.semanticcpg.language.*

object NetworkProtocols extends QueryBundle {

  implicit val resolver: ICallResolver = NoResolve

  @q
  def usageOfInsecureProtocol(): Query =
    Query.make(
      name = "usage-of-insecure-protocol",
      author = Crew.claudiu,
      title = "Insecure Protocol used",
      description = """
        |Using insecure network protocols allows attackers who control the network to replace, remove and inject data.
        |""".stripMargin,
      score = 4,
      withStrRep({ cpg =>
        cpg.method
          .fullNameExact("java.net.URL.<init>:void(java.lang.String)")
          .callIn
          .where(_.argument.isLiteral.code("^[^h]*http:.*"))
      }),
      tags = List(QueryTags.insecureNetworkTraffic, QueryTags.android),
      codeExamples = CodeExamples(
        List("""
          |import java.io.BufferedReader; import java.io.InputStreamReader; import java.net.URL;
          |fun fn1() {
          |    val url = URL("http://phrack.org") // <---- relevant line
          |    val connection = url.openConnection()
          |    BufferedReader(InputStreamReader(connection.getInputStream())).use { inp ->
          |        var line: String?
          |        while (inp.readLine().also { line = it } != null) {
          |            println(line)
          |        }
          |    }
          |}
          |""".stripMargin),
        List("""
          |import java.io.BufferedReader; import java.io.InputStreamReader; import java.net.URL;
          |fun fn2() {
          |    val url = URL("https://phrack.org") // <---- relevant line
          |    val connection = url.openConnection()
          |    BufferedReader(InputStreamReader(connection.getInputStream())).use { inp ->
          |        var line: String?
          |        while (inp.readLine().also { line = it } != null) {
          |            println(line)
          |        }
          |    }
          |}
          |""".stripMargin)
      )
    )
}
