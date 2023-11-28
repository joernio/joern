package io.joern.scanners.java

import io.joern.console.*
import io.joern.macros.QueryMacros.*
import io.joern.scanners.{Crew, QueryTags}
import io.shiftleft.codepropertygraph.generated.*
import io.shiftleft.semanticcpg.language.*

object CertificateChecks extends QueryBundle {

  implicit val resolver: ICallResolver = NoResolve

  @q
  def certChecks(): Query =
    Query.make(
      name = "ineffective-certificate-check",
      author = Crew.malte,
      title = "Ineffective Certificate Validation: The validation result is always positive",
      description = """
        |A certificate validation function is implemented as a function that only consists of a prologue where local
        |variables are initialized to arguments, followed by a (positive) return statement.
        |""".stripMargin,
      score = 6,
      withStrRep({ cpg =>
        val validators = Map(
          // javax.net.ssl.HostnameVerifier
          "verify" -> "boolean(java.lang.String,javax.net.ssl.SSLSession)",
          // javax.net.ssl.X509ExtendedTrustManager
          "checkClientTrusted" -> "void(java.security.cert.X509Certificate[],java.lang.String,java.net.Socket)",
          "checkClientTrusted" -> "void(java.security.cert.X509Certificate[],java.lang.String,javax.net.ssl.SSLEngine)",
          "checkServerTrusted" -> "void(java.security.cert.X509Certificate[],java.lang.String,java.net.Socket)",
          "checkServerTrusted" -> "void(java.security.cert.X509Certificate[],java.lang.String,javax.net.ssl.SSLEngine)"
        )

        // skip over arguments getting copied to local variables
        def isPrologue(node: nodes.CfgNode): Boolean = node match {
          case id: nodes.Identifier =>
            id.refsTo.forall(_.isInstanceOf[nodes.Local])
          case c: nodes.Call =>
            c.methodFullName == Operators.assignment && c.argument.forall(isPrologue)
          case _ => false
        }
        def skipPrologue(node: nodes.CfgNode): Iterator[nodes.CfgNode] =
          node.start.repeat(_.cfgNext)(_.until(_.filter(!isPrologue(_))))

        cpg.method
          .nameExact(validators.keys.toSeq: _*)
          .signatureExact(validators.values.toSeq: _*)
          .cfgFirst
          .flatMap(skipPrologue)
          .filter {
            case lit: nodes.Literal =>
              lit.code == "1" && lit.cfgNext
                .forall(_.isInstanceOf[nodes.Return])
            case _: nodes.Return => true
            case _               => false
          }
      }),
      tags = List(QueryTags.badimpl, QueryTags.default)
    )
}
