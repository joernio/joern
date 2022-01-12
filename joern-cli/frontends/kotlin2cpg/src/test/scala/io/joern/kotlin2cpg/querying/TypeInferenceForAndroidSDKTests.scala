package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.Kt2CpgTestContext
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.semanticcpg.language._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class TypeInferenceForAndroidSDKTests extends AnyFreeSpec with Matchers {

  // good source of vulns
  "CPG for code with calls to Android WebView methods" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
        |package mypkg
        |
        |import android.content.Intent
        |import android.os.Bundle
        |import android.webkit.WebView
        |import android.app.Activity
        |import kotlinx.android.synthetic.main.activity_product_list.*
        |
        |class MyCustomActivity : Activity() {
        |  fun onCreate(savedInstanceState: Bundle?) {
        |    //val webview = findViewById<WebView>(R.id.webview)
        |    val webView = WebView(this)
        |
        |    //webview.settings.javaScriptEnabled = true
        |    //webview.settings.loadWithOverviewMode = true
        |    //webview.settings.useWideViewPort = true
        |    webview.settings.allowUniversalAccessFromFileURLs = true
        |    //webview.settings.userAgentString = USER_AGENT
        |
        |    webview.loadUrl("https://vx-underground.org/pwn.yr.webview.html")
        |  }
        |}
        | """.stripMargin)

    "should contain a CALL node for the `webview.settings` DQE on with the correct METHOD_FULL_NAME set" in {
      val List(c) = cpg.call.codeExact("webview.settings").take(1).l
      c.methodFullName shouldBe "kotlin.Any.settings:ANY(ANY)"
    }
  }

  "CPG for code with call to method of `Activity` superclass" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
        |package mypkg
        |
        |import android.content.Intent
        |import android.content.IntentFilter
        |import android.os.Bundle
        |import android.view.View
        |import android.app.Activity
        |
        |class AboutUsActivity : Activity() {
        |  fun onSendData(view: View) {
        |    val userName = "USERNAME"
        |    val password = "PASSWORD"
        |
        |    val intent = Intent("com.insecureshop.action.BROADCAST")
        |    intent.putExtra("username", userName)
        |    intent.putExtra("password", password)
        |    sendBroadcast(intent)
        |
        |    textView.text = "InsecureShop is an intentionally designed vulnerable android app built in Kotlin."
        |  }
        |}
        | """.stripMargin)

    "should contain a CALL node for `sendBroadcast` with the correct METHOD_FULL_NAME set" in {
      val List(c) = cpg.call.code("sendBroadcast.*").l
      c.methodFullName shouldBe "android.app.Activity.sendBroadcast:kotlin.Unit(android.content.Intent)"
    }
  }

  "CPG for code using the Android SDK" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
        |package mypkg
        |
        |import android.net.http.SslError
        |import android.webkit.SslErrorHandler
        |import android.webkit.WebView
        |import android.webkit.WebViewClient
        |
        |class Foo : WebViewClient() {
        |    override fun onReceivedSslError(view: WebView?, handler: SslErrorHandler?, error: SslError?) {
        |        handler?.proceed()
        |    }
        |}
        | """.stripMargin)

    "should contain a TYPE_DECL node for `Foo` with the correct inheritsFromTypeFullName value set" in {
      val List(x) = cpg.typeDecl.isExternal(false).name("Foo").l
      x.inheritsFromTypeFullName shouldBe List("android.webkit.WebViewClient")
    }
  }

  "CPG for code using the Android SDK defining a class inheriting from the `androidx` namespace" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
        |package com.insecureshop
        |
        |import android.os.Bundle
        |import androidx.appcompat.app.AppCompatActivity
        |
        |class ResultActivity : AppCompatActivity() {
        |
        |  override fun onCreate(savedInstanceState: Bundle?) {
        |    super.onCreate(savedInstanceState)
        |    setResult(-1, intent)
        |    finish()
        |  }
        |}
        | """.stripMargin)

    "should contain a TYPE_DECL node for `ResultActivity` with the correct FULL_NAME set" in {
      val List(x) = cpg.typeDecl.isExternal(false).name("ResultActivity").l
      x.fullName shouldBe "com.insecureshop.ResultActivity"
      x.inheritsFromTypeFullName shouldBe Seq("androidx.appcompat.app.AppCompatActivity")
    }
  }

  // https://developer.android.com/reference/android/R
  "CPG for code with use of Android's `R`" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
        |package mypkg
        |
        |import android.app.Activity
        |import android.os.Bundle
        |
        |class ProductListActivity : Activity() {
        |  override fun onCreate(savedInstanceState: Bundle?) {
        |    super.onCreate(savedInstanceState)
        |    // `R.layout.placeholder_custom_view` won't have any type info;
        |    // the `R` derived props are only available with build info which currently is not taken into account
        |    // TODO: check if it is possible to specify the `R` jar or content or whatever to get that info if necessary
        |    setContentView(R.layout.placeholder_custom_view)
        |  }
        |}
        |""".stripMargin)

    "should not contain any CALL nodes which are missing a `:` character in their MFNs" in {
      val List(c) = cpg.call.code("setContentView.*").l
      c.methodFullName.contains(":") shouldBe true
    }
  }
}
