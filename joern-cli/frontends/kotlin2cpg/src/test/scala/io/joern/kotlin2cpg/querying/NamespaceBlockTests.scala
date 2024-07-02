package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class NamespaceBlockTests extends KotlinCode2CpgFixture(withOssDataflow = false) {
  "CPG for code with simple namespace declaration" should {
    val cpg = code("""
        |package com.test.PackageFoo
        |
        |class ClassFoo {
        |  fun methodFoo(x: Int) {
        |    return x * 2
        |  }
        |}
        |
        |fun add(x: Int, y: Int): Int {
        |  return x + y
        |}
        |""".stripMargin)

    "should contain two namespace blocks in total (<global>, PackageFoo)" in {
      cpg.namespaceBlock.size shouldBe 2
      cpg.namespaceBlock.name.l.toSet shouldBe Set("<global>", "PackageFoo")
    }

    "should contain correct namespace block for known file" in {
      val List(x) = cpg.namespaceBlock.filename(".*.kt").l
      x.name shouldBe "PackageFoo"
      x.filename should not be ""
      x.fullName shouldBe s"com.test.PackageFoo"
      x.order shouldBe 1
    }

    "should allow traversing from namespace block to namespace" in {
      cpg.namespaceBlock.filename(".*kt").namespace.name.l shouldBe List("PackageFoo")
    }

    "should allow traversing from namespace block to type declaration" in {
      cpg.namespaceBlock.filename(".*kt").typeDecl.name.l shouldBe List("<global>")
    }

    "should allow traversing from namespace block to file" in {
      cpg.namespaceBlock.filename(".*kt").file.size shouldBe 1
    }

  }

  "CPG for code with imports of simple Android packages" should {
    val cpg = code("""
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
       |    val webView = WebView(this)
       |    webview.settings.allowUniversalAccessFromFileURLs = true
       |    webview.loadUrl("https://vx-underground.org/cozy_bear/homepage.html")
       |  }
       |}
       | """.stripMargin)

    "should contain a NAMESPACE_BLOCK for the `import android.app.Activity` with the correct props set" in {
      val List(nsb) = cpg.namespaceBlock.name(".*Activity.*").l
      nsb.fullName shouldBe "android.app.Activity"
    }

    "should contain a NAMESPACE_BLOCK for the `import android.webkit.WebView` with the correct props set" in {
      val List(nsb) = cpg.namespaceBlock.name(".*WebView.*").l
      nsb.fullName shouldBe "android.webkit.WebView"
    }
  }
}
