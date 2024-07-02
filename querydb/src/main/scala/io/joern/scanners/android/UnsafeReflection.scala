package io.joern.scanners.android

import io.joern.scanners.*
import io.joern.console.*
import io.joern.dataflowengineoss.queryengine.EngineContext
import io.joern.dataflowengineoss.semanticsloader.Semantics
import io.joern.macros.QueryMacros.*
import io.shiftleft.semanticcpg.language.*

object UnsafeReflection extends QueryBundle {
  implicit val engineContext: EngineContext = EngineContext(Semantics.empty)
  implicit val resolver: ICallResolver      = NoResolve

  // todo: support `build.gradle.kts`
  // todo: `targetSdkVersion` >= 19 ++ missing `isValidFragment`
  // todo: support multiple specifications of `targetSdkVersion` (e.g. inside `buildTypes`)
  // todo: build actual asts for `build.gradle[.kts]?` instead of using dirty regex matching
  // see: https://support.google.com/faqs/answer/7188427?hl=en
  // see: https://www.synopsys.com/blogs/software-security/fragment-injection/
  @q
  def fragmentInjection(): Query =
    Query.make(
      name = "fragment-injection",
      author = Crew.claudiu,
      title = "Attackers can load fragments that should be private",
      description = "-",
      score = 7,
      withStrRep({ cpg =>
        def groovyBuildGradleFiles                                  = cpg.configFile.name(".*build.gradle")
        val targetSdkVersionMatch                                   = """^[^t]+targetSdk[^0-9]+(\d+)""".r
        val minimumAndroidSdkVersionWhereNoAdditionalChecksRequired = 19
        groovyBuildGradleFiles.filter { gradleFile =>
          gradleFile.content
            .split('\n')
            .exists { line =>
              targetSdkVersionMatch
                .findAllIn(line)
                .matchData
                .exists { m =>
                  m.groupCount > 0 && m.group(1).toInt < minimumAndroidSdkVersionWhereNoAdditionalChecksRequired
                }
            }
        }
      }),
      tags = List(QueryTags.android),
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
              |    compileSdk 32
              |    defaultConfig {
              |        applicationId "com.example.slimandroid"
              |        minSdk 23
              |        targetSdk 18
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
                |    compileSdk 32
                |    defaultConfig {
                |        applicationId "com.example.slimandroid"
                |        minSdk 23
                |        targetSdk 19
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
