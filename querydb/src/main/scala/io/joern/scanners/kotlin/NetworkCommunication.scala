package io.joern.scanners.kotlin

import io.joern.scanners._
import io.joern.console._
import io.joern.dataflowengineoss.queryengine.EngineContext
import io.joern.dataflowengineoss.semanticsloader.Semantics
import io.joern.dataflowengineoss.language._
import io.joern.macros.QueryMacros._
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.semanticcpg.language._

object NetworkCommunication extends QueryBundle {
  implicit val engineContext: EngineContext = EngineContext(Semantics.empty)
  implicit val resolver: ICallResolver      = NoResolve

  // todo: improve by including trust managers created via `object` expressions
  // todo: improve by ensuring the `null` on the RHS of the overwritten TrustManager methods are considered correctly
  @q
  def nopTrustManagerUsed(): Query =
    Query.make(
      name = "usage-of-nop-trust-manager",
      author = Crew.claudiu,
      title = "NOP trust manager used to initialize SSL context",
      description = "Traffic sent using this context can be intercepted by attackers on the same network",
      score = 5,
      withStrRep({ cpg =>
        val nopTrustManagerFullNames =
          cpg.typeDecl
            .isExternal(false)
            .filter(_.inheritsFromTypeFullName.contains("javax.net.ssl.X509TrustManager"))
            .filter { node =>
              node.method.nameExact("checkClientTrusted").block.expressionDown.isEmpty ||
              node.method.nameExact("checkServerTrusted").block.expressionDown.isEmpty
            }
            .fullName
            .toSeq
        def nopTrustManagersAllocs =
          cpg.method.fullNameExact(Operators.alloc).callIn.typeFullNameExact(nopTrustManagerFullNames: _*)
        def sslCtxInitCalls = cpg.method
          .fullNameExact("javax.net.ssl.SSLContext.init:void(kotlin.Array,kotlin.Array,java.security.SecureRandom)")
          .callIn
        sslCtxInitCalls.filter { call =>
          call.argument(2).reachableBy(nopTrustManagersAllocs).nonEmpty
        }
      }),
      tags = List(QueryTags.android, QueryTags.insecureNetworkTraffic),
      multiFileCodeExamples = MultiFileCodeExamples(
        positive = List(
          List(
            CodeSnippet(
              """
                |package mypkg
                |
                |import javax.net.ssl.SSLContext
                |import javax.net.ssl.TrustManager
                |import javax.net.ssl.X509TrustManager
                |
                |class NOPTrustManager : X509TrustManager {
                |    override fun getAcceptedIssuers(): Array<X509Certificate>? = null
                |    override fun checkClientTrusted(certs: Array<X509Certificate>, authType: String) {}
                |    override fun checkServerTrusted(certs: Array<X509Certificate>, authType: String) {}
                |}
                |
                |fun main() {
                |    val acceptAllTrustManager = NOPTrustManager()
                |    val trustAllCerts = arrayOf<TrustManager>(acceptAllTrustManager)
                |    val sslContext = SSLContext.getInstance("SSL")
                |    sslContext.init(null, trustAllCerts, java.security.SecureRandom())
                |}
                |""".stripMargin,
              "App.kt"
            )
          )
        ),
        negative = List(
          List(
            CodeSnippet(
              """
                |package mypkg
                |
                |import javax.net.ssl.SSLContext
                |import javax.net.ssl.TrustManager
                |import javax.net.ssl.X509TrustManager
                |
                |# nop trust manager defined but not used
                |class NOPTrustManager : X509TrustManager {
                |    override fun getAcceptedIssuers(): Array<X509Certificate>? = null
                |    override fun checkClientTrusted(certs: Array<X509Certificate>, authType: String) {}
                |    override fun checkServerTrusted(certs: Array<X509Certificate>, authType: String) {}
                |}
                |""".stripMargin,
              "App.kt"
            )
          )
        )
      )
    )
}
