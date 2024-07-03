package io.joern.scanners.android

import io.joern.scanners.*
import io.joern.console.*
import io.joern.dataflowengineoss.queryengine.EngineContext
import io.joern.dataflowengineoss.semanticsloader.Semantics
import io.joern.macros.QueryMacros.*
import io.shiftleft.semanticcpg.language.*
import io.joern.dataflowengineoss.language.*

object RootDetection extends QueryBundle {
  implicit val engineContext: EngineContext = EngineContext(Semantics.empty)
  implicit val resolver: ICallResolver      = NoResolve

  @q
  def rootDetectionViaFileChecks()(implicit engineContext: EngineContext): Query =
    Query.make(
      name = "root-detection-via-file-checks",
      author = Crew.claudiu,
      title = "App attempts to detect rooting via file checks",
      description = "-",
      score = 3,
      withStrRep({ cpg =>
        val superUserCheckPathSuffixes = List("Superuser.apk", "superuser.apk", "/su")
        def pathsToSuperUserSuffixes =
          cpg.literal.filter(_.code.contains('/')).filter { node =>
            superUserCheckPathSuffixes.exists { ending =>
              node.code.stripSuffix("\"").stripSuffix("\'").endsWith(ending)
            }
          }
        def fileExistsCalls = cpg.call.methodFullNameExact("java.io.File.exists:boolean()")
        fileExistsCalls.where(_.argument.reachableBy(pathsToSuperUserSuffixes)).method.where(_.caller)
      }),
      tags = List(QueryTags.android),
      multiFileCodeExamples = MultiFileCodeExamples(
        positive = List(
          List(
            CodeSnippet(
              """
                |package no.such.pkg
                |
                |import android.support.v7.app.AppCompatActivity
                |import android.os.Bundle
                |import java.io.File
                |
                |class RootDetectionActivity : AppCompatActivity() {
                |    override fun onCreate(savedInstanceState: Bundle?) {
                |        super.onCreate(savedInstanceState)
                |        if (isRooted()) { println("Device is rooted") }
                |    }
                |
                |    fun isRooted(): Boolean {
                |      val paths = arrayOf("/system/app/Superuser/Superuser.apk", "/system/app/Superuser.apk","/sbin/su", "/system/bin/su", "/system/xbin/su", "/data/local/xbin/su", "/data/local/bin/su", "/system/sd/xbin/su", "/system/bin/failsafe/su", "/data/local/su", "/su/bin/su", "re.robv.android.xposed.installer-1.apk","/data/app/eu.chainfire.supersu-1/base.apk");
                |      for(path in paths) {
                |          val f = File(path)
                |          if (f.exists()) {
                |            return true
                |          }
                |      }
                |      return false
                |    }
                |}
                |""".stripMargin,
              "RootDetectionActivity.kt"
            )
          )
        ),
        negative = List(
          List(
            CodeSnippet(
              """
                |package no.such.pkg
                |
                |import android.support.v7.app.AppCompatActivity
                |import android.os.Bundle
                |import java.io.File
                |
                |class RootDetectionActivityNoCall : AppCompatActivity() {
                |    override fun onCreate(savedInstanceState: Bundle?) {
                |        super.onCreate(savedInstanceState)
                |        // no call to isRooted
                |    }
                |
                |    fun isRooted(): Boolean {
                |      val paths =  arrayOf("/system/app/Superuser/Superuser.apk", "/system/app/Superuser.apk","/sbin/su", "/system/bin/su", "/system/xbin/su", "/data/local/xbin/su", "/data/local/bin/su", "/system/sd/xbin/su", "/system/bin/failsafe/su", "/data/local/su", "/su/bin/su", "re.robv.android.xposed.installer-1.apk","/data/app/eu.chainfire.supersu-1/base.apk");
                |      for(path in paths) {
                |          val f = File(path)
                |          if (f.exists()) {
                |            return true
                |          }
                |      }
                |      return false
                |    }
                |}
                |""".stripMargin,
              "RootDetectionActivityNoCall.kt"
            ),
            CodeSnippet(
              """
                |package no.such.pkg
                |
                |import android.support.v7.app.AppCompatActivity
                |import android.os.Bundle
                |import java.io.File
                |
                |class RootDetectionActivityAlwaysFalse : AppCompatActivity() {
                |    override fun onCreate(savedInstanceState: Bundle?) {
                |        super.onCreate(savedInstanceState)
                |        if (isRootedNop()) { println("Device is rooted") }
                |    }
                |
                |    fun isRootedNop(): Boolean {
                |      val paths = arrayOf("/system/app/Superuser/Superuser.apk", "/system/app/Superuser.apk","/sbin/su", "/system/bin/su", "/system/xbin/su", "/data/local/xbin/su", "/data/local/bin/su", "/system/sd/xbin/su", "/system/bin/failsafe/su", "/data/local/su", "/su/bin/su", "re.robv.android.xposed.installer-1.apk","/data/app/eu.chainfire.supersu-1/base.apk");
                |      return false
                |    }
                |}
                |""".stripMargin,
              "RootDetectionActivityAlwaysFalse.kt"
            )
          )
        )
      )
    )
}
