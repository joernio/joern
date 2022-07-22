package io.joern.scanners.android

import io.joern.suites.AndroidQueryTestSuite

class AndroidMisconfigurationsTests extends AndroidQueryTestSuite {

  override def queryBundle = AndroidMisconfigurations

  "the `manifestXmlDebuggableEnabled` query" when {
    def makeAndroidXml(allowBackup: Boolean): String = {
      s"""|<?xml version="1.0" encoding="utf-8"?>
          |<manifest xmlns:android="http://schemas.android.com/apk/res/android"
          |    package="com.example.slimandroid">
          |
          |    <application
          |        android:allowBackup="$allowBackup"
          |        android:label="SlimAndroid"
          |        android:supportsRtl="true"
          |        android:theme="@style/Theme.AppCompat">
          |        <activity
          |            android:name=".MainActivity"
          |            android:exported="true">
          |            <intent-filter>
          |                <action android:name="android.intent.action.MAIN" />
          |                <category android:name="android.intent.category.LAUNCHER" />
          |            </intent-filter>
          |        </activity>
          |    </application>
          |</manifest>""".stripMargin
    }

    "should match a config file when `allowBackup` is set to `true`" in {
      val cpg = code("fun main() = println(0xbadf00d)")
        .moreCode(makeAndroidXml(true), "AndroidManifest.xml")
      val query = queryBundle.manifestXmlDebuggableEnabled()
      findMatchingConfigFiles(cpg, query) shouldBe Set("AndroidManifest.xml")
    }

    "should not match a config file when `allowBackup` is set to `false`" in {
      val cpg = code("fun main() = println(0xbadf00d)")
        .moreCode(makeAndroidXml(false), "AndroidManifest.xml")
      val query = queryBundle.manifestXmlDebuggableEnabled()
      findMatchingConfigFiles(cpg, query) shouldBe Set()
    }

    "should not match anything when there is no file named `AndroidManifest.xml` in the cpg" in {
      val cpg = code("fun main() = println(0xbadf00d)")
        .moreCode(makeAndroidXml(false), "NOPNOPNOPNOPNOPNOP.xml")
      val query = queryBundle.manifestXmlDebuggableEnabled()
      findMatchingConfigFiles(cpg, query) shouldBe Set()
    }

    "should not match anything when the file named `AndroidManifest.xml` is empty" in {
      val cpg = code("fun main() = println(0xbadf00d)")
        .moreCode("", "AndroidManifest.xml")
      val query = queryBundle.manifestXmlDebuggableEnabled()
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
      val query = queryBundle.manifestXmlDebuggableEnabled()
      findMatchingConfigFiles(cpg, query) shouldBe Set()
    }
  }

  "the `tapJacking` query" when {
    def makeBuildGradle(targetSdk: Int): String = {
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
      val q = queryBundle.tapJacking()
      q.multiFileCodeExamples.positive.filter(_.nonEmpty).foreach { codeExample =>
        val first = codeExample(0)
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
        val first = codeExample(0)
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
        .moreCode(makeBuildGradle(23), "build.gradle")
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
}
