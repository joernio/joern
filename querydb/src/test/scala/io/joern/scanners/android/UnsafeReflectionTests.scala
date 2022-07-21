package io.joern.scanners.android

import io.joern.console.scan._
import io.shiftleft.codepropertygraph.generated.nodes.ConfigFile
import io.joern.suites.AndroidQueryTestSuite

class UnsafeReflectionTests extends AndroidQueryTestSuite {

  override def queryBundle = UnsafeReflection

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
    q.multiFileCodeExamples.positive.filter(_.nonEmpty).foreach { example =>
      val first = example(0)
      val cpg   = code(first.content, first.filename)
      val finalCpg = example.drop(1).foldLeft(cpg) { (c, e) =>
        c.moreCode(e.content, e.filename)
      }
      q(finalCpg).flatMap(_.evidence).collect { case c: ConfigFile => c }.size shouldBe 1
    }
  }

  "should not on all multi-file negative examples" in {
    val q = queryBundle.fragmentInjection()
    q.multiFileCodeExamples.negative.filter(_.nonEmpty).foreach { codeExample =>
      val first = codeExample(0)
      val cpg   = code(first.content, first.filename)
      val finalCpg = codeExample.drop(1).foldLeft(cpg) { (c, e) =>
        c.moreCode(e.content, e.filename)
      }
      q(finalCpg).flatMap(_.evidence).collect { case c: ConfigFile => c }.size shouldBe 0
    }
  }

  "should match a CONFIG_FILE node when `targetSdkVersion` is set to `18`" in {
    val cpg   = code("fun main() = println(0xff)")
    val cpg2  = cpg.moreCode(makeBuildGradle(18), "build.gradle")
    val query = queryBundle.fragmentInjection()
    query(cpg2).flatMap(_.evidence).collect { case c: ConfigFile => c }.size shouldBe 1
  }

  "should not match a CONFIG_FILE when `targetSdkVersion` is set to `19`" in {
    val cpg = code("fun main() = println(0xff)")
      .moreCode(makeBuildGradle(19), "build.gradle")
    val query = queryBundle.fragmentInjection()
    query(cpg).flatMap(_.evidence).collect { case c: ConfigFile => c }.size shouldBe 0
  }

  "should not match a CONFIG_FILE when `targetSdkVersion` is set to `20`" in {
    val cpg = code("fun main() = println(0xff)")
      .moreCode(makeBuildGradle(20), "build.gradle")
    val query = queryBundle.fragmentInjection()
    query(cpg).flatMap(_.evidence).collect { case c: ConfigFile => c }.size shouldBe 0
  }

  "should not match anything when the file named `build.gradle` is empty" in {
    val cpg   = code("fun main() = println(0xff)").moreCode("", "build.gradle")
    val query = queryBundle.fragmentInjection()
    query(cpg).flatMap(_.evidence).collect { case c: ConfigFile => c }.size shouldBe 0
  }
}
