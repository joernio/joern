package io.joern.scanners.java

import io.joern.scanners.*
import io.shiftleft.semanticcpg.language.*
import io.joern.console.*
import io.joern.macros.QueryMacros.*
import io.joern.dataflowengineoss.language.*
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
      score = 6,
      withStrRep({ cpg =>
        def source = cpg.literal("\"MD5\"|\"SHA-1\"")

        def sink =
          cpg.method.fullName("java.security.MessageDigest.getInstance.*").parameter

        sink.reachableBy(source)
      }),
      tags = List(QueryTags.cryptography, QueryTags.default),
      codeExamples = CodeExamples(
        List(
          """
          |String algo = "MD5";
          |MessageDigest md = MessageDigest.getInstance(algo);
          |""".stripMargin,
          """
          |String algo = "SHA-1";
          |MessageDigest md = MessageDigest.getInstance(algo);
          |""".stripMargin
        ),
        List("""
          |String algo = "SHA-256";
          |MessageDigest md = MessageDigest.getInstance(algo);
          |""".stripMargin)
      )
    )

  @q
  def lowIterationPbeKey()(implicit context: EngineContext): Query =
    Query.make(
      name = "low-pbe-key-iterations",
      author = Crew.dave,
      title = "Low number of iterations detected for password-based encryption.",
      description = """
          | Do not use password-based encryption with iterations count less than 1000. You should use the maximum number
          | of rounds which is tolerable, performance-wise, in your application.
          |""".stripMargin,
      score = 6,
      withStrRep({ cpg =>
        def source = cpg.literal.code("\\d+")

        def sink =
          cpg.method.fullName("javax.crypto.spec.PBEKeySpec.<init>.*").parameter

        sink.reachableBy(source).dedup.filter(f => Integer.parseInt(f.code) < 1000)
      }),
      tags = List(QueryTags.cryptography, QueryTags.default),
      codeExamples = CodeExamples(
        List("""
            |SecretKeyFactory factory = SecretKeyFactory.getInstance("PBKDF2WithHmacSHA512");
            |SecretKey key = factory.generateSecret(new PBEKeySpec(password.toCharArray(), salt, 500, keyLength));
            |""".stripMargin),
        List("""
            |SecretKeyFactory factory = SecretKeyFactory.getInstance("PBKDF2WithHmacSHA512");
            |SecretKey key = factory.generateSecret(new PBEKeySpec(password.toCharArray(), salt, 4000, keyLength));
            |""".stripMargin)
      )
    )

}
