package io.joern.suites

import io.joern.console.scan.*
import io.joern.console.{CodeSnippet, Query, QueryBundle}
import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.joern.util.QueryUtil
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.ConfigFile
import io.shiftleft.semanticcpg.language.*

class AndroidQueryTestSuite[QB <: QueryBundle](val queryBundle: QB)
    extends KotlinCode2CpgFixture(withOssDataflow = true, withDefaultJars = true) {

  val argumentProvider = new QDBArgumentProvider(3)

  def allQueries: List[Query] = QueryUtil.allQueries(queryBundle, argumentProvider)

  protected def cpgForSnippets(snippets: List[CodeSnippet]): Cpg = {
    val first = snippets.head
    val cpg   = code(first.content, first.filename)
    snippets.drop(1).foldLeft(cpg) { (foldCpg, e) =>
      foldCpg.moreCode(e.content, e.filename)
    }
  }

  def findMatchingConfigFiles(cpg: Cpg, q: Query): Set[String] = {
    q(cpg).flatMap(_.evidence).collect { case c: ConfigFile => c }.name.toSetImmutable
  }

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

  def makeBuildGradle(targetSdk: Int = 23, minSdk: Int = 23): String = {
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
       |        minSdk $minSdk
       |        targetSdk $targetSdk
       |        versionCode 1
       |        versionName "1.0"
       |    }
       |}
       |""".stripMargin
  }
}
