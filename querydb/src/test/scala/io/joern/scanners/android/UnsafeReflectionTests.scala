package io.joern.scanners.android

import io.joern.suites.AndroidQueryTestSuite

class UnsafeReflectionTests extends AndroidQueryTestSuite(UnsafeReflection) {

  private def makeBuildGradle(targetSdk: Int): String = {
    s"""
       |plugins {
       |    id 'com.android.application'
       |    id 'kotlin-android'
       |}
       |
       |android {
       |    compileSdk 32
       |    defaultConfig {
       |        applicationId "com.example.slimandroid"
       |        minSdk 23
       |        targetSdk $targetSdk
       |        versionCode 1
       |        versionName "1.0"
       |    }
       |}
       |""".stripMargin
  }

  "should match on all multi-file positive examples" in {
    val q = queryBundle.fragmentInjection()
    q.multiFileCodeExamples.positive.filter(_.nonEmpty).foreach { snippets =>
      val cpg = cpgForSnippets(snippets)
      findMatchingConfigFiles(cpg, q).size shouldBe 1
    }
  }

  "should not on all multi-file negative examples" in {
    val q = queryBundle.fragmentInjection()
    q.multiFileCodeExamples.negative.filter(_.nonEmpty).foreach { snippets =>
      val cpg = cpgForSnippets(snippets)
      findMatchingConfigFiles(cpg, q).size shouldBe 0
    }
  }

  "should match a CONFIG_FILE node when `targetSdkVersion` is set to `18`" in {
    val cpg = code("fun main() = println(0xff)")
      .moreCode(makeBuildGradle(18), "build.gradle")
    val query = queryBundle.fragmentInjection()
    findMatchingConfigFiles(cpg, query).size shouldBe 1
  }

  "should not match a CONFIG_FILE when `targetSdkVersion` is set to `19`" in {
    val cpg = code("fun main() = println(0xff)")
      .moreCode(makeBuildGradle(19), "build.gradle")
    val query = queryBundle.fragmentInjection()
    findMatchingConfigFiles(cpg, query).size shouldBe 0
  }

  "should not match a CONFIG_FILE when `targetSdkVersion` is set to `20`" in {
    val cpg = code("fun main() = println(0xff)")
      .moreCode(makeBuildGradle(20), "build.gradle")
    val query = queryBundle.fragmentInjection()
    findMatchingConfigFiles(cpg, query).size shouldBe 0
  }

  "should not match anything when the file named `build.gradle` is empty" in {
    val cpg   = code("fun main() = println(0xff)").moreCode("", "build.gradle")
    val query = queryBundle.fragmentInjection()
    findMatchingConfigFiles(cpg, query).size shouldBe 0
  }
}
