package io.joern.suites

import io.joern.util.QueryUtil
import io.joern.console.{CodeSnippet, Query, QueryBundle}
import io.joern.kotlin2cpg.{Config, Kotlin2Cpg}
import io.joern.x2cpg.testfixtures.{Code2CpgFixture, DefaultTestCpg, LanguageFrontend}
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.utils.ProjectRoot
import io.joern.console.scan._
import io.shiftleft.codepropertygraph.generated.nodes.ConfigFile
import io.shiftleft.semanticcpg.language._

import java.io.File

trait Kotlin2CpgFrontend extends LanguageFrontend {
  override val fileSuffix: String = ".kt"

  def execute(sourceCodePath: File): Cpg = {
    val cpgFile = File.createTempFile("kt2cpg", ".zip")
    cpgFile.deleteOnExit()
    val kt2cpg = new Kotlin2Cpg()
    val config = Config(inputPath = sourceCodePath.getAbsolutePath, outputPath = cpgFile.getAbsolutePath)
    kt2cpg.createCpg(config).get
  }
}

class DefaultTestCpgWithKotlin extends DefaultTestCpg with Kotlin2CpgFrontend

class AndroidQueryTestSuite extends Code2CpgFixture(() => new DefaultTestCpgWithKotlin()) {

  val argumentProvider = new QDBArgumentProvider(3)

  def queryBundle: QueryBundle = QueryUtil.EmptyBundle

  def allQueries: List[Query] = QueryUtil.allQueries(queryBundle, argumentProvider)

  protected def cpgForSnippets(snippets: List[CodeSnippet]): Cpg = {
    val first = snippets(0)
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
