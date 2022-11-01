package io.joern.scanners.java

import io.joern.suites.JavaQueryTestSuite

class CryptographyMisuseTests extends JavaQueryTestSuite {

  override def queryBundle = CryptographyMisuse

  "the `unsafeHashAlgo` query" when {
    "find use of MD5 hash algorithm" in {
      code(
        """import java.lang.CloneNotSupportedException;
          |import java.security.DigestException;
          |import java.security.MessageDigest;
          |
          |public class Foo {
          | public static void main(String[] args) {
          |   String algo = "MD5";
          |   MessageDigest md = MessageDigest.getInstance(algo);
          |
          |   try {
          |     md.update("Hello, world!");
          |     MessageDigest tc1 = md.clone();
          |     byte[] toChapter1Digest = tc1.digest();
          |   } catch (CloneNotSupportedException cnse) {
          |     throw new DigestException("couldn't make digest of partial content");
          |   }
          | }
          |}
          |""".stripMargin)
      val query = queryBundle.unsafeHashAlgo()
      val results = findMatchingCalls(cpg, query)
      println(results)
    }
  }

}
