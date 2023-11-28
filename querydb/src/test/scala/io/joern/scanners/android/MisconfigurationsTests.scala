package io.joern.scanners.android

import io.joern.suites.AndroidQueryTestSuite

class MisconfigurationsTests extends AndroidQueryTestSuite(Misconfigurations) {

  "the `manifestXmlBackupEnabled` query" when {

    "should match a config file when `allowBackup` is set to `true`" in {
      val cpg = code("fun main() = println(0xbadf00d)")
        .moreCode(makeAndroidXml(true), "AndroidManifest.xml")
      val query = queryBundle.manifestXmlBackupEnabled()
      findMatchingConfigFiles(cpg, query) shouldBe Set("AndroidManifest.xml")
    }

    "should not match a config file when `allowBackup` is set to `false`" in {
      val cpg = code("fun main() = println(0xbadf00d)")
        .moreCode(makeAndroidXml(false), "AndroidManifest.xml")
      val query = queryBundle.manifestXmlBackupEnabled()
      findMatchingConfigFiles(cpg, query) shouldBe Set()
    }

    "should not match anything when there is no file named `AndroidManifest.xml` in the cpg" in {
      val cpg = code("fun main() = println(0xbadf00d)")
        .moreCode(makeAndroidXml(false), "NOPNOPNOPNOPNOPNOP.xml")
      val query = queryBundle.manifestXmlBackupEnabled()
      findMatchingConfigFiles(cpg, query) shouldBe Set()
    }

    "should not match anything when the file named `AndroidManifest.xml` is empty" in {
      val cpg = code("fun main() = println(0xbadf00d)")
        .moreCode("", "AndroidManifest.xml")
      val query = queryBundle.manifestXmlBackupEnabled()
      findMatchingConfigFiles(cpg, query) shouldBe Set()
    }

    "should not match anything when the file named `AndroidManifest.xml` contains invalid XML" in {
      val invalidXml =
        """
          |<?xml version="1.0"?>
          |<!DOCTYPE lolz [
          | <!ENTITY lol "lol">
          | <!ELEMENT lolz (#PCDATA)>
          | <!ENTITY lol1 "&lol;&lol;&lol;&lol;&lol;&lol;&lol;&lol;&lol;&lol;">
          | <!ENTITY lol2 "&lol1;&lol1;&lol1;&lol1;&lol1;&lol1;&lol1;&lol1;&lol1;&lol1;">
          | <UNFINISHED_BILLION_LAUGHS
          |<lolz>&lol2;</lolz>
          |""".stripMargin
      val cpg = code("fun main() = println(0xbadf00d)")
        .moreCode(invalidXml, "AndroidManifest.xml")
      val query = queryBundle.manifestXmlBackupEnabled()
      findMatchingConfigFiles(cpg, query) shouldBe Set()
    }
  }

  "the `tapJacking` query" when {

    "should match on all multi-file positive examples" in {
      val q = queryBundle.tapJacking()
      q.multiFileCodeExamples.positive.filter(_.nonEmpty).foreach { codeExample =>
        val first = codeExample.head
        val cpg   = code(first.content, first.filename)
        val finalCpg = codeExample.drop(1).foldLeft(cpg) { (foldCpg, example) =>
          foldCpg.moreCode(example.content, example.filename)
        }
        findMatchingConfigFiles(finalCpg, q) shouldBe Set("build.gradle")
      }
    }

    "should not on all multi-file negative examples" in {
      val q = queryBundle.tapJacking()
      q.multiFileCodeExamples.negative.filter(_.nonEmpty).foreach { codeExample =>
        val first = codeExample.head
        val cpg   = code(first.content, first.filename)
        val finalCpg = codeExample.drop(1).foldLeft(cpg) { (foldCpg, example) =>
          foldCpg.moreCode(example.content, example.filename)
        }
        findMatchingConfigFiles(finalCpg, q) shouldBe Set()
      }
    }

    "should match a CONFIG_FILE node when `targetSdkVersion` is set to `22`" in {
      val cpg = code("fun main() = println(0xbadf00d)")
        .moreCode(makeBuildGradle(22), "build.gradle")
      val query = queryBundle.tapJacking()
      findMatchingConfigFiles(cpg, query) shouldBe Set("build.gradle")
    }

    "should not match a CONFIG_FILE when `targetSdkVersion` is set to `23`" in {
      val cpg = code("fun main() = println(0xbadf00d)")
        .moreCode(makeBuildGradle(), "build.gradle")
      val query = queryBundle.tapJacking()
      findMatchingConfigFiles(cpg, query) shouldBe Set()
    }

    "should not match anything when the file named `build.gradle` is empty" in {
      val cpg = code("fun main() = println(0xbadf00d)")
        .moreCode("", "build.gradle")
      val query = queryBundle.tapJacking()
      findMatchingConfigFiles(cpg, query) shouldBe Set()
    }
  }

  "the `vulnerablePRNGOnAndroidv16_18` query" when {
    "should match when the minSdk version is v16-18 and a call to SecureRandom default constructor is present" ignore {
      val cpg = code("""import java.security.SecureRandom
          |
          |fun main() {
          |    SecureRandom random = new SecureRandom()
          |}
          |""".stripMargin)
        .moreCode(makeBuildGradle(minSdk = 16), "build.gradle")
      val query = queryBundle.vulnerablePRNGOnAndroidv16_18()
      findMatchingConfigFiles(cpg, query) shouldBe Set("build.gradle")
    }

    "should match when the minSdk version is v16-18 and a call to SecureRandom.getInstance with a PRNG algorithm " +
      "is set" ignore {
        val cpg = code("""import java.security.SecureRandom
          |
          |fun main() {
          |    SecureRandom random = SecureRandom.getInstance("NativePRNG")
          |}
          |""".stripMargin)
          .moreCode(makeBuildGradle(minSdk = 18), "build.gradle")
        val query = queryBundle.vulnerablePRNGOnAndroidv16_18()
        findMatchingConfigFiles(cpg, query) shouldBe Set("build.gradle")
      }

    "should not match when the minSdk version is v16-18 and a call to SecureRandom.getInstance with a non-PRNG " +
      "algorithm is set" in {
        val cpg = code("""import java.security.SecureRandom
          |
          |fun main() {
          |    SecureRandom random = SecureRandom.getInstance("PKCS11")
          |}
          |""".stripMargin)
          .moreCode(makeBuildGradle(minSdk = 18), "build.gradle")
        val query = queryBundle.vulnerablePRNGOnAndroidv16_18()
        findMatchingConfigFiles(cpg, query) shouldBe Set()
      }

    "should not match when the minSdk version is v16-18 but no call to SecureRandom is present" in {
      val cpg = code("import java.security.SecureRandom\nfun main() = println(\"I'm okay\")")
        .moreCode(makeBuildGradle(minSdk = 17), "build.gradle")
      val query = queryBundle.vulnerablePRNGOnAndroidv16_18()
      findMatchingConfigFiles(cpg, query) shouldBe Set()
    }

    "should not match when the minSdk version is 18 > and a call to SecureRandom is present" in {
      val cpg = code("""import java.security.SecureRandom
          |
          |fun main() {
          |    SecureRandom random = new SecureRandom()
          |}
          |""".stripMargin)
        .moreCode(makeBuildGradle(minSdk = 19), "build.gradle")
      val query = queryBundle.vulnerablePRNGOnAndroidv16_18()
      findMatchingConfigFiles(cpg, query) shouldBe Set()
    }
  }
}
