package io.joern.scanners.java

import io.joern.scanners._
import io.shiftleft.semanticcpg.language._
import io.joern.console._
import io.joern.macros.QueryMacros._
import io.joern.dataflowengineoss.language._
import io.joern.dataflowengineoss.queryengine.EngineContext

/** @see
  *   <a href="https://files.sri.inf.ethz.ch/website/papers/diffcode-pldi2018.pdf">Inferring Crypto API Rules from Code
  *   Changes</a>
  */
object CryptographyMisuse extends QueryBundle {

  implicit val resolver: ICallResolver = NoResolve

  @q
  def unsafeHashAlgo()(implicit context: EngineContext): Query =
    Query.make(
      name = "unsafe-crypto-hash-algo",
      author = Crew.dave,
      title = "Unsafe cryptographic hash algorithm used.",
      description = """
          | MD5 and SHA-1 are considered weak and insecure; an attacker can easily use an MD5 collision to forge valid
          | digital certificates or use dictionary/brute-force attacks to obtain passwords. Use SHA-256 instead.
          |""".stripMargin,
      score = 8,
      withStrRep({ cpg =>
        def source = cpg.literal("\"MD5\"|\"SHA-1\"")

        def sink =
          cpg.method.fullName("java.security.MessageDigest.getInstance.*").parameter

        sink.reachableBy(source).l
      }),
      tags = List(QueryTags.cryptography, QueryTags.default)
    )

}
