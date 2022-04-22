package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.TestContext
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes.{
  Call,
  FieldIdentifier,
  Identifier,
  Literal,
  NewIdentifier,
  NewLiteral
}
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.semanticcpg.language._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class AndroidSDKTests extends AnyFreeSpec with Matchers {

  // TODO: reenable test
  /*
  // good source of vulns
  "CPG for code with calls to Android WebView methods" - {
    lazy val cpg = TestContext.buildCpg(
      """
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
        |    // TODO: as soon as type propagation is working, also test the following case
        |    // val webview = findViewById<WebView>(R.id.webview)
        |    val webView = WebView(this)
        |    webView.settings.allowUniversalAccessFromFileURLs = true
        |    webView.loadUrl("https://vx-underground.org/pwn.yr.webview.html")
        |  }
        |}
        | """.stripMargin,
      includeAllJars = true
    )

    "should contain a CALL node for the `webview.settings` DQE on with the correct METHOD_FULL_NAME set" in {
      val List(c) = cpg.call.codeExact("webView.settings").take(1).l
      c.methodFullName shouldBe Operators.fieldAccess
    }
  }
   */

  "CPG for code with call to method of `Activity` superclass" - {
    lazy val cpg = TestContext.buildCpg(
      """
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
        | """.stripMargin,
      includeAllJars = true
    )

    "should contain a CALL node for `sendBroadcast` with the correct METHOD_FULL_NAME set" in {
      val List(c) = cpg.call.code("sendBroadcast.*").l
      c.methodFullName shouldBe "android.app.Activity.sendBroadcast:void(android.content.Intent)"
    }
  }

  "CPG for code using the Android SDK" - {
    lazy val cpg = TestContext.buildCpg(
      """
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
        | """.stripMargin,
      includeAllJars = true
    )

    "should contain a TYPE_DECL node for `Foo` with the correct inheritsFromTypeFullName value set" in {
      val List(x) = cpg.typeDecl.isExternal(false).name("Foo").l
      x.inheritsFromTypeFullName shouldBe List("android.webkit.WebViewClient")
    }
  }

  "CPG for code using the Android SDK defining a class inheriting from the `androidx` namespace" - {
    lazy val cpg = TestContext.buildCpg(
      """
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
        | """.stripMargin,
      includeAllJars = true
    )

    "should contain a TYPE_DECL node for `ResultActivity` with the correct FULL_NAME set" in {
      val List(x) = cpg.typeDecl.isExternal(false).name("ResultActivity").l
      x.fullName shouldBe "com.insecureshop.ResultActivity"
      x.inheritsFromTypeFullName shouldBe Seq("androidx.appcompat.app.AppCompatActivity")
    }
  }

  // https://developer.android.com/reference/android/R
  "CPG for code with use of Android's `R`" - {
    lazy val cpg = TestContext.buildCpg(
      """
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
        |""".stripMargin,
      includeAllJars = true
    )

    "should not contain any CALL nodes which are missing a `:` character in their MFNs" in {
      val List(c) = cpg.call.code("setContentView.*").l
      c.methodFullName.contains(":") shouldBe true
    }
  }

  "CPG for code with use of Android log" - {
    lazy val cpg = TestContext.buildCpg(
      """
      |package mypkg
      |
      |import android.app.Activity
      |import android.os.Bundle
      |import android.util.Log
      |
      |class MyActivity : Activity() {
      |  override fun onCreate(savedInstanceState: Bundle?) {
      |    super.onCreate(savedInstanceState)
      |    var message = "MESSAGE"
      |    message.plus("_2")
      |    Log.d("PREFIX", username)
      |  }
      |}
      |""".stripMargin,
      includeAllJars = true
    )

    "should contain a CALL node for `Log.d` with the correct props set" in {
      val List(c) = cpg.call.methodFullName(".*Log.*").l
      c.methodFullName shouldBe "android.util.Log.d:java.lang.Integer(java.lang.String,java.lang.String)"

      c.argument.size shouldBe 2
      c.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH

      val List(firstArg: Literal, secondArg: Identifier) = cpg.call.methodFullName(".*Log.*").argument.l
      firstArg.code shouldBe "\"PREFIX\""
      firstArg.argumentIndex shouldBe 1
      firstArg.lineNumber shouldBe Some(12)
      firstArg.columnNumber shouldBe Some(10)

      secondArg.code shouldBe "username"
      secondArg.argumentIndex shouldBe 2
      secondArg.lineNumber shouldBe Some(12)
      secondArg.columnNumber shouldBe Some(20)
    }
  }

  "CPG for code with use of Android's `findViewById`" - {
    lazy val cpg = TestContext.buildCpg(
      """
      |package mypkg
      |
      |import android.app.Activity
      |import android.os.Bundle
      |import android.util.Log
      |import android.webkit.WebView
      |
      |class MyActivity : Activity() {
      |  override fun onCreate(savedInstanceState: Bundle?) {
      |    super.onCreate(savedInstanceState)
      |
      |    val webview = findViewById<WebView>(R.id.webview) as WebView
      |    webview.settings.javaScriptEnabled = true
      |  }
      |}
      |""".stripMargin,
      includeAllJars = true
    )

    "should contain a CALL node for `findViewById` with the correct props set" in {
      val List(c) = cpg.call.code("findViewB.*").codeNot(".*as.*").l
      c.methodFullName shouldBe "android.app.Activity.findViewById:android.view.View(java.lang.Integer)"
      c.argument.size shouldBe 1
      c.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
    }

    "should contain an IDENTIFIER node for webview with the correct props set" in {
      val List(i) = cpg.call.code(".*findViewB.*").argument(1).isIdentifier.l
      i.typeFullName shouldBe "android.webkit.WebView"
    }

    "should contain an TYPE_REF node for the `as` CALL lhs with the correct props set" in {
      val List(tr) = cpg.typeRef.codeExact("WebView").l
      tr.argumentIndex shouldBe 2
      tr.typeFullName shouldBe "android.webkit.WebView"
    }
  }

  "CPG for code with call to Android's `sendBroadcast`" - {
    lazy val cpg = TestContext.buildCpg(
      """
        |package mypkg
        |
        |import android.content.Intent
        |import android.os.Bundle
        |import android.app.Activity
        |
        |class MyCustomActivity : Activity() {
        |  fun onCreate(savedInstanceState: Bundle?) {
        |    super.onCreate(savedInstanceState)
        |    val aSecret = "SECRETSECRETSECRETSECRETSECRETSECRET"
        |    val intent = Intent("com.insecureshop.action.BROADCAST")
        |    intent.putExtra("ONE_TIME_PAD", aSecret)
        |    sendBroadcast(intent)
        |  }
        |}
        | """.stripMargin,
      includeAllJars = true
    )

    "should contain a CALL node for `super.onCreate.*` with the correct props set" in {
      def callQ = cpg.call.code(".*onCreate.*")

      val List(c) = callQ.l
      c.methodFullName shouldBe "android.app.Activity.onCreate:void(android.os.Bundle)"
      c.signature shouldBe "void(android.os.Bundle)"
      c.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH

      val List(firstArg: Identifier, secondArg: Identifier) = callQ.argument.l
      firstArg.code shouldBe "super"
      firstArg.typeFullName shouldBe "android.app.Activity"
      secondArg.code shouldBe "savedInstanceState"
      secondArg.typeFullName shouldBe "android.os.Bundle"
    }

    "should contain a CALL node for `sendBroadcast.*` with the correct props set" in {
      def callQ = cpg.call.code(".*sendBroadcast.*")

      val List(c) = callQ.l
      c.methodFullName shouldBe "android.app.Activity.sendBroadcast:void(android.content.Intent)"
      c.signature shouldBe "void(android.content.Intent)"
      c.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH

      val List(firstArg: Identifier) = callQ.argument.l
      firstArg.code shouldBe "intent"
      firstArg.typeFullName shouldBe "android.content.Intent"
    }

    "should contain a CALL node for `putExtra.*` with the correct props set" in {
      def callQ = cpg.call.code(".*putExtra.*").take(1)

      val List(c) = callQ.l
      c.methodFullName shouldBe "android.content.Intent.putExtra:android.content.Intent(java.lang.String,java.lang.String)"
      c.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH

      val List(firstArg: Identifier, secondArg: Literal, thirdArg: Identifier) = callQ.argument.l
      firstArg.code shouldBe "intent"
      firstArg.typeFullName shouldBe "android.content.Intent"
      firstArg.argumentIndex shouldBe 0
      secondArg.code shouldBe "\"ONE_TIME_PAD\""
      secondArg.typeFullName shouldBe "java.lang.String"
      secondArg.argumentIndex shouldBe 1
      thirdArg.code shouldBe "aSecret"
      thirdArg.typeFullName shouldBe "java.lang.String"
      thirdArg.argumentIndex shouldBe 2
    }
  }
}
