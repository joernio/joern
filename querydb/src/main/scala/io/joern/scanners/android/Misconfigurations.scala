package io.joern.scanners.android

import io.joern.console.*
import io.joern.dataflowengineoss.language.*
import io.joern.dataflowengineoss.queryengine.EngineContext
import io.joern.macros.QueryMacros.*
import io.joern.scanners.*
import io.shiftleft.semanticcpg.language.*

object Misconfigurations extends QueryBundle {

  @q
  def manifestXmlBackupEnabled(): Query =
    Query.make(
      name = "manifest-backup-enabled",
      author = Crew.claudiu,
      title = "Backups enabled in Android Manifest File",
      description = """
          |Backup flag is set to true in AndroidManifest.xml which means that the application data can be retrieved via adb.
          |""".stripMargin,
      score = 3,
      withStrRep({ cpg =>
        import io.joern.semanticcpg.utils.SecureXmlParsing

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

  /** <a
    * href="https://www.researchgate.net/publication/262257979_Predictability_of_Android_OpenSSL%27s_Pseudo_random_number_generator">Predictability
    * of Android OpenSSL's Pseudo random number generator</a>
    */
  @q
  def vulnerablePRNGOnAndroidv16_18()(implicit context: EngineContext): Query =
    Query.make(
      name = "vuln-prng-android-v16_18",
      author = Crew.dave,
      title = "Vulnerable underlying PRNG used on currently set version of Android",
      description = """
          |The underlying PRNG is vulnerable on Android v16-18. If the application is implemented by utilizing
          |org.webkit package and a key exchange scheme is RSA, the PreMasterSecret of the first SSL session
          |can be recovered using the restored PRNG states.
          |
          |For more information, see "Predictability of Android OpenSSL's Pseudo random number generator" by S.H. Kim 
          |et. al.
          |""".stripMargin,
      score = 6,
      withStrRep({ cpg =>
        def groovyBuildGradleFiles = cpg.configFile.name(".*build.gradle")

        val targetSdkVersionMatch = """^[^t]+minSdk[^0-9]+(\d+)""".r
        val insecureSdkVersionMin = 16
        val insecureSdkVersionMax = 18
        def satisfiesConfig = groovyBuildGradleFiles.filter { gradleFile =>
          gradleFile.content
            .split('\n')
            .exists { line =>
              targetSdkVersionMatch
                .findAllIn(line)
                .matchData
                .exists { m =>
                  m.groupCount > 0 &&
                  m.group(1).toInt >= insecureSdkVersionMin
                  m.group(1).toInt <= insecureSdkVersionMax
                }
            }
        }

        def source         = cpg.literal("\".*PRNG.*\"")
        def sink           = cpg.call.code(".*SecureRandom.getInstance.*")
        def defaultSecRand = cpg.call.methodFullNameExact("java.security.SecureRandom.<init>:void()")
        if (
          (defaultSecRand.nonEmpty || sink.reachableBy(source).nonEmpty) &&
          satisfiesConfig.nonEmpty
        )
          satisfiesConfig
        else
          Iterator.empty
      }),
      tags = List(QueryTags.android, QueryTags.cryptography, QueryTags.misconfiguration)
    )
}
