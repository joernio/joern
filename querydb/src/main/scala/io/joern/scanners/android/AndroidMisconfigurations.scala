package io.joern.scanners.android

import io.joern.scanners._
import io.joern.console._
import io.joern.macros.QueryMacros._
import io.shiftleft.semanticcpg.language._

object AndroidMisconfigurations extends QueryBundle {

  @q
  def manifestXmlDebuggableEnabled(): Query =
    Query.make(
      name = "manifest-debuggable-enabled",
      author = Crew.claudiu,
      title = "Backups enabled in Android Manifest File",
      description = """
          |Backup flag is set to true in AndroidManifest.xml which means that the application data can be retrieved via adb.
          |""".stripMargin,
      score = 3,
      withStrRep({ cpg =>
        import javax.xml.parsers.SAXParserFactory
        import scala.xml.{Elem, XML}

        object SecureXmlParsing {
          def parseXml(content: String): Option[Elem] = {
            try {
              val spf = SAXParserFactory.newInstance()

              spf.setValidating(false)
              spf.setNamespaceAware(false)
              spf.setXIncludeAware(false)
              spf.setFeature("http://xml.org/sax/features/validation", false)
              spf.setFeature("http://apache.org/xml/features/disallow-doctype-decl", false)
              spf.setFeature("http://apache.org/xml/features/nonvalidating/load-dtd-grammar", false)
              spf.setFeature("http://apache.org/xml/features/nonvalidating/load-external-dtd", false)
              spf.setFeature("http://xml.org/sax/features/external-parameter-entities", false)
              spf.setFeature("http://xml.org/sax/features/external-general-entities", false)

              Some(XML.withSAXParser(spf.newSAXParser()).loadString(content))
            } catch {
              case _: Throwable =>
                None
            }
          }
        }

        val androidUri = "http://schemas.android.com/apk/res/android"
        cpg.configFile
          .filter(_.name.endsWith("AndroidManifest.xml"))
          .where { config =>
            config.content
              .flatMap(SecureXmlParsing.parseXml)
              .filter(_.label == "manifest")
              .flatMap(_.child)
              .filter(_.label == "application")
              .filter { node =>
                val isAllowBackup = node.attribute(androidUri, "allowBackup")
                isAllowBackup match {
                  case Some(n) => n.toString == "true"
                  case None    => false
                }
              }
          }
      }),
      tags = List(QueryTags.android, QueryTags.misconfiguration),
      codeExamples = CodeExamples(
        List("""|<?xml version="1.0" encoding="utf-8"?>
            |<manifest xmlns:android="http://schemas.android.com/apk/res/android"
            |    package="com.example.slimandroid">
            |
            |    <application
            |        android:allowBackup="true"
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
            |</manifest>""".stripMargin),
        List("""|<?xml version="1.0" encoding="utf-8"?>
           |<manifest xmlns:android="http://schemas.android.com/apk/res/android"
           |    package="com.example.slimandroid">
           |
           |    <application
           |        android:allowBackup="false"
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
           |</manifest>""".stripMargin)
      )
    )

  // todo: check for `android:filterTouchesWhenObscured="[true|false]"`
  // todo: check for `onFilterTouchEventForSecurity`
  // see: https://redfoxsec.com/blog/android-tapjacking-vulnerability/
  // see: https://cwe.mitre.org/data/definitions/1021.html
  // see: https://infinum.com/blog/pentesting-misc-cases/
  // see: https://developer.android.com/reference/android/Manifest.permission.html#SYSTEM_ALERT_WINDOW
  @q
  def tapJacking(): Query =
    Query.make(
      name = "tap-jacking",
      author = Crew.claudiu,
      title = "Tap Jacking: target SDK <23 specified in `build.gradle` ",
      description = """
          |Android apps targeting API levels 22 and lower have the SYSTEM_ALERT_WINDOW permission enabled by default.
          |This allows apps to draw overlays over other apps. Attackers can use this option to create an overlay that
          |would essentially hijack user taps and use it to obtain sensitive user information.""".stripMargin,
      score = 6,
      withStrRep({ cpg =>
        def groovyBuildGradleFiles = cpg.configFile.name(".*build.gradle")
        val targetSdkVersionMatch  = """^[^t]+targetSdk[^0-9]+(\d+)""".r
        val firstSecureSdkVersion  = 23
        groovyBuildGradleFiles.filter { gradleFile =>
          gradleFile.content
            .split('\n')
            .exists { line =>
              targetSdkVersionMatch
                .findAllIn(line)
                .matchData
                .exists { m =>
                  m.groupCount > 0 && m.group(1).toInt < firstSecureSdkVersion
                }
            }
        }
      }),
      tags = List(QueryTags.android, QueryTags.misconfiguration),
      multiFileCodeExamples = MultiFileCodeExamples(
        positive = List(
          List(
            CodeSnippet("fun main() = println(0xbadf00d)", "SomeActivity.kt"),
            CodeSnippet(
              """
              |plugins {
              |    id 'com.android.application'
              |    id 'kotlin-android'
              |}
              |
              |android {
              |    compileSdk 22
              |    defaultConfig {
              |        applicationId "com.example.slimandroid"
              |        minSdk 22
              |        targetSdk 22
              |        versionCode 1
              |        versionName "1.0"
              |    }
              |}
              |""".stripMargin,
              "build.gradle"
            )
          )
        ),
        negative = List(
          List(
            CodeSnippet("fun main() = println(0xbadf00d)", "SomeActivity.kt"),
            CodeSnippet(
              """
                |plugins {
                |    id 'com.android.application'
                |    id 'kotlin-android'
                |}
                |
                |android {
                |    compileSdk 23
                |    defaultConfig {
                |        applicationId "com.example.slimandroid"
                |        minSdk 23
                |        targetSdk 23
                |        versionCode 1
                |        versionName "1.0"
                |    }
                |}
                |""".stripMargin,
              "build.gradle"
            )
          )
        )
      )
    )
}
