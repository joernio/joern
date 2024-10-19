package io.joern.scanners.android

import io.joern.console._
import io.joern.dataflowengineoss.language._
import io.joern.dataflowengineoss.queryengine.EngineContext
import io.joern.dataflowengineoss.semanticsloader.NoSemantics
import io.joern.macros.QueryMacros._
import io.joern.scanners._
import io.shiftleft.semanticcpg.language._

object JavaScriptInterface extends QueryBundle {
  implicit val engineContext: EngineContext = EngineContext(NoSemantics)
  implicit val resolver: ICallResolver      = NoResolve

  @q
  def insecureLoadUrlToExec()(implicit engineContext: EngineContext): Query =
    Query.make(
      name = "insecure-load-url-to-exec",
      author = Crew.claudiu,
      title = "Data from an insecure URL load reaches `Runtime.getRuntime.exec` via JavaScript bridge.",
      description = "This query identifies insecure usage of `loadUrl` with `http://` that reaches `Runtime.exec()` through a JavaScript bridge.",
      score = 9,
      withStrRep({ cpg =>
        import io.shiftleft.codepropertygraph.generated.nodes.{Call, Identifier}
        import io.shiftleft.semanticcpg.language.android._

        def webViewsWithInsecureLoadUrlCalls =
          cpg.webView
            .callsEnableJS
            .where(_.loadUrlCalls.filter { callNode =>
              callNode.argument.reachableByLiteral("http://").nonEmpty
            })

        val appUsesCleartextTraffic = cpg.appManifest.usesCleartextTraffic.nonEmpty
        def exposedJavaScriptInterfaceObjects =
          if (appUsesCleartextTraffic)
            webViewsWithInsecureLoadUrlCalls.addJavascriptInterfaceCalls.argument(1)
          else Iterator.empty

        val exposedJavaScriptInterfaceObjectNames = exposedJavaScriptInterfaceObjects.collect {
          case ident: Identifier => ident.typeFullName
          case call: Call        => call.typeFullName
        }.toList

        def exposedJavaScriptInterfaceMethods =
          cpg.method.exposedToJS.where(_.typeDecl.filter { node =>
            exposedJavaScriptInterfaceObjectNames.exists(_ == node.fullName)
          })

        def runtimeExecCalls =
          cpg.call.name("exec").typeFullName("java.lang.Process")

        runtimeExecCalls.where(_.argument.reachableBy(exposedJavaScriptInterfaceMethods.parameter)).toList.iterator
      }),
      tags = List(QueryTags.android),
      multiFileCodeExamples = MultiFileCodeExamples(
        positive = List(
          List(
            CodeSnippet(
              """|package io.vroooom.vulnerableapp;
              |
              |import android.content.Context;
              |import android.os.Build;
              |import android.webkit.JavascriptInterface;
              |import android.widget.Toast;
              |
              |import java.io.IOException;
              |
              |public class JavaScriptBridge {
              |    Context mContext;
              |    JavaScriptBridge(Context c) {
              |        mContext = c;
              |    }
              |
              |    @JavascriptInterface
              |    public int getAndroidVersion() {
              |        return Build.VERSION.SDK_INT;
              |    }
              |
              |    @JavascriptInterface
              |    public void showToast(String text) {
              |        Toast.makeText(mContext, text, Toast.LENGTH_SHORT).show();
              |    }
              |
              |    // https://support.google.com/faqs/answer/9095419?hl=en
              |    @JavascriptInterface
              |    public void forgottenDebugFn(String cmd) {
              |        String[] cmdArray = new String[3];
              |        cmdArray[0] = "sh";
              |        cmdArray[1] = "-c";
              |        cmdArray[2] = cmd;
              |
              |        try {
              |            Runtime.getRuntime().exec(cmdArray);
              |        } catch (IOException e) {
              |            System.out.print("error");
              |        }
              |    }
              |}
              |""".stripMargin,
              "io/vroooom/vulnerableapp/JavaScriptBridge.java"
            ),
            CodeSnippet(
              """|package io.vroooom.vulnerableapp;
                |
                |import android.content.Intent;
                |import android.net.Uri;
                |import android.os.Bundle;
                |import android.support.v7.app.AppCompatActivity;
                |import android.webkit.WebView;
                |
                |public class MainActivityJava extends AppCompatActivity {
                |    @Override
                |    protected void onCreate(Bundle savedInstanceState) {
                |        super.onCreate(savedInstanceState);
                |        setContentView(R.layout.activity_main);
                |
                |        JavaScriptBridge jsBridge = new JavaScriptBridge(this);
                |        WebView webView = findViewById(R.id.webview);
                |        webView.getSettings().setJavaScriptEnabled(true);
                |        webView.addJavascriptInterface(jsBridge, "jsBridge");
                |
                |        String url = "http://phrack.org";
                |        webView.loadUrl(url);
                |        finish();
                |    }
                |}
                |""".stripMargin,
              "io/vroooom/vulnerableapp/MainActivityJava.java"
            ),
            CodeSnippet(
              """|<?xml version="1.0" encoding="utf-8"?>
                |<manifest xmlns:android="http://schemas.android.com/apk/res/android"
                |    xmlns:tools="http://schemas.android.com/tools">
                |    <uses-permission android:name="android.permission.INTERNET" />
                |
                |    <application
                |        android:allowBackup="true"
                |        android:usesCleartextTraffic="true"
                |        android:icon="@mipmap/ic_launcher"
                |        android:label="@string/app_name"
                |        android:roundIcon="@mipmap/ic_launcher_round"
                |        android:supportsRtl="true"
                |        android:theme="@style/Theme.Vulnerableapp"
                |        tools:targetApi="31">
                |
                |        <activity
                |            android:name=".MainActivityJava"
                |            android:exported="true">
                |            <intent-filter>
                |                <action android:name="android.intent.action.MAIN" />
                |                <category android:name="android.intent.category.LAUNCHER" />
                |            </intent-filter>
                |        </activity>
                |    </application>
                |</manifest>
                |""".stripMargin,
              "AndroidManifest.xml"
            )
          )
        ),
        negative = List(
          // Code examples where there is no vulnerability
        )
      )
    )
}
