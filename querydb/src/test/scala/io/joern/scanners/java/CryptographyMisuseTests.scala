package io.joern.scanners.java

import io.joern.suites.JavaQueryTestSuite

class CryptographyMisuseTests extends JavaQueryTestSuite(CryptographyMisuse) {

  "the `unsafeHashAlgo` query" when {
    "find the use of the MD5 hash algorithm" in {
      val cpg = code("""import java.lang.CloneNotSupportedException;
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
      val query   = queryBundle.unsafeHashAlgo()
      val results = findMatchingLiterals(cpg, query)
      results shouldBe List("\"MD5\"")
    }

    "ignore the use of the SHA256 hash algorithm" in {
      val cpg = code("""import java.lang.CloneNotSupportedException;
          |import java.security.DigestException;
          |import java.security.MessageDigest;
          |
          |public class Foo {
          | public static void main(String[] args) {
          |   String algo = "SHA256";
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
      val query   = queryBundle.unsafeHashAlgo()
      val results = findMatchingLiterals(cpg, query)
      results shouldBe List()
    }
  }

  "the `lowIterationPbeKey` query" when {
    "find low number of iterations given to PBEKey constructor" in {
      val cpg = code("""import javax.crypto.spec.*;
          |import javax.crypto.*;
          |
          |public class Foo {
          |    public static void main(String[] args) {
          |        var iterations = 30;
          |        SecretKeyFactory factory = SecretKeyFactory.getInstance("PBKDF2WithHmacSHA512");
          |        SecretKey key            = factory.generateSecret( new PBEKeySpec(password.toCharArray(), salt, iterations, keyLength) );
          |    }
          |}
          |""".stripMargin)
      val query   = queryBundle.lowIterationPbeKey()
      val results = findMatchingLiterals(cpg, query)
      results shouldBe List("30")
    }

    "ignore sufficiently high number of iterations given to PBEKey constructor" in {
      val cpg = code("""import javax.crypto.spec.*;
          |import javax.crypto.*;
          |
          |public class Foo {
          |    public static void main(String[] args) {
          |        var iterations = 5000;
          |        SecretKeyFactory factory = SecretKeyFactory.getInstance("PBKDF2WithHmacSHA512");
          |        SecretKey key            = factory.generateSecret( new PBEKeySpec(password.toCharArray(), salt, iterations, keyLength) );
          |    }
          |}
          |""".stripMargin)
      val query   = queryBundle.lowIterationPbeKey()
      val results = findMatchingLiterals(cpg, query)
      results shouldBe List()
    }
  }

}
