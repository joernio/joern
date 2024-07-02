package io.joern.scanners.android

import io.joern.scanners.*
import io.joern.console.*
import io.joern.dataflowengineoss.queryengine.EngineContext
import io.joern.dataflowengineoss.semanticsloader.Semantics
import io.joern.macros.QueryMacros.*
import io.shiftleft.semanticcpg.language.*
import io.joern.dataflowengineoss.language.*

object Intents extends QueryBundle {
  implicit val engineContext: EngineContext = EngineContext(Semantics.empty)
  implicit val resolver: ICallResolver      = NoResolve

  @q
  def intentToRuntimeExec()(implicit engineContext: EngineContext): Query =
    Query.make(
      name = "intent-to-runtime-exec",
      author = Crew.claudiu,
      title = "Data from an intent reaches `Runtime.getRuntime.exec`.",
      description = "-",
      score = 9,
      withStrRep({ cpg =>
        import io.shiftleft.semanticcpg.language.android._
        val exportedActivityNames = cpg.configFile.exportedAndroidActivityNames.l
        def exportedActivities =
          cpg.typeDecl.filter { node => exportedActivityNames.contains(node.name) }
        def getIntentCalls =
          exportedActivities.method.call.name("getIntent").typeFullName("android.content.Intent")
        def runtimeExecCalls =
          cpg.call.name("exec").typeFullName("java.lang.Process")
        runtimeExecCalls.where(_.argument.reachableBy(getIntentCalls)).l.iterator
      }),
      tags = List(QueryTags.android),
      multiFileCodeExamples = MultiFileCodeExamples(
        positive = List(
          List(
            CodeSnippet(
              """|package no.such.pkg;
                 |
                 |import android.content.Intent;
                 |import android.net.Uri;
                 |import android.os.Bundle;
                 |import android.support.v7.app.AppCompatActivity;
                 |
                 |import java.io.IOException;
                 |
                 |public class DeepLinkActivity extends AppCompatActivity {
                 |    @Override
                 |    protected void onCreate(Bundle savedInstanceState) {
                 |        super.onCreate(savedInstanceState);
                 |        setContentView(R.layout.activity_main);
                 |        Intent intent = getIntent();
                 |        Uri uri;
                 |        if(intent != null && Intent.ACTION_VIEW.equals(intent.getAction()) && (uri = intent.getData()) != null) {
                 |            processDeeplink(uri);
                 |        }
                 |        finish();
                 |    }
                 |
                 |    private void processDeeplink(Uri uri) {
                 |        if("http".equals(uri.getScheme()) && "java.vroooom.io".equals(uri.getHost())) {
                 |            String path = uri.getPath();
                 |            String secret = uri.getQueryParameter("secret");
                 |            if("/cmdrun".equals(path) && secret.equals("b4dc0ffee")) {
                 |                String cmd = uri.getQueryParameter("cmd");
                 |                String[] cmdArray = new String[3];
                 |                cmdArray[0] = "sh";
                 |                cmdArray[1] = "-c";
                 |                cmdArray[2] = cmd;
                 |                try {
                 |                    Runtime.getRuntime().exec(cmdArray);
                 |                } catch (IOException e) {
                 |                    System.out.print("error");
                 |                }
                 |            }
                 |        }
                 |    }
                 |}
                 |""".stripMargin,
              "DeepLinkActivity.java"
            ),
            CodeSnippet(
              """|<?xml version="1.0" encoding="utf-8"?>
                 |<manifest xmlns:android="http://schemas.android.com/apk/res/android"
                 |    xmlns:tools="http://schemas.android.com/tools">
                 |    <uses-permission android:name="android.permission.INTERNET" />
                 |    <application
                 |        android:allowBackup="true"
                 |        android:dataExtractionRules="@xml/data_extraction_rules"
                 |        android:fullBackupContent="@xml/backup_rules"
                 |        android:usesCleartextTraffic="true"
                 |        android:icon="@mipmap/ic_launcher"
                 |        android:label="@string/app_name"
                 |        android:roundIcon="@mipmap/ic_launcher_round"
                 |        android:supportsRtl="true"
                 |        android:theme="@style/Theme.Vulnerableapp"
                 |        tools:targetApi="31">
                 |        <activity
                 |            android:name=".MainActivity"
                 |            android:exported="true">
                 |            <intent-filter>
                 |                <action android:name="android.intent.action.MAIN" />
                 |                <category android:name="android.intent.category.LAUNCHER" />
                 |            </intent-filter>
                 |
                 |            <meta-data
                 |                android:name="android.app.lib_name"
                 |                android:value="" />
                 |        </activity>
                 |
                 |        <activity android:name="DeepLinkActivity" android:exported="true">
                 |            <intent-filter android:autoVerify="true">
                 |                <action android:name="android.intent.action.VIEW" />
                 |                <category android:name="android.intent.category.DEFAULT" />
                 |                <category android:name="android.intent.category.BROWSABLE" />
                 |                <category android:name="android.intent.category.LAUNCHER" />
                 |                <data android:host="vulnerable" android:scheme="vroooom" />
                 |                <data android:scheme="http" />
                 |                <data android:scheme="https" />
                 |                <data android:host="java.vroooom.io" />
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
          // activity not exported in AndroidManifest.xml
          List(
            CodeSnippet(
              """|package no.such.pkg;
               |
               |import android.content.Intent;
               |import android.net.Uri;
               |import android.os.Bundle;
               |import android.support.v7.app.AppCompatActivity;
               |
               |import java.io.IOException;
               |
               |public class DeepLinkActivity extends AppCompatActivity {
               |    @Override
               |    protected void onCreate(Bundle savedInstanceState) {
               |        super.onCreate(savedInstanceState);
               |        setContentView(R.layout.activity_main);
               |        Intent intent = getIntent();
               |        Uri uri;
               |        if(intent != null && Intent.ACTION_VIEW.equals(intent.getAction()) && (uri = intent.getData()) != null) {
               |            processDeeplink(uri);
               |        }
               |        finish();
               |    }
               |
               |    private void processDeeplink(Uri uri) {
               |        if("http".equals(uri.getScheme()) && "java.vroooom.io".equals(uri.getHost())) {
               |            String path = uri.getPath();
               |            String secret = uri.getQueryParameter("secret");
               |            if("/cmdrun".equals(path) && secret.equals("b4dc0ffee")) {
               |                String cmd = uri.getQueryParameter("cmd");
               |                String[] cmdArray = new String[3];
               |                cmdArray[0] = "sh";
               |                cmdArray[1] = "-c";
               |                cmdArray[2] = cmd;
               |                try {
               |                    Runtime.getRuntime().exec(cmdArray);
               |                } catch (IOException e) {
               |                    System.out.print("error");
               |                }
               |            }
               |        }
               |    }
               |}
               |""".stripMargin,
              "DeepLinkActivity.java"
            ),
            CodeSnippet(
              """|<?xml version="1.0" encoding="utf-8"?>
               |<manifest xmlns:android="http://schemas.android.com/apk/res/android"
               |    xmlns:tools="http://schemas.android.com/tools">
               |    <uses-permission android:name="android.permission.INTERNET" />
               |    <application
               |        android:allowBackup="true"
               |        android:dataExtractionRules="@xml/data_extraction_rules"
               |        android:fullBackupContent="@xml/backup_rules"
               |        android:usesCleartextTraffic="true"
               |        android:icon="@mipmap/ic_launcher"
               |        android:label="@string/app_name"
               |        android:roundIcon="@mipmap/ic_launcher_round"
               |        android:supportsRtl="true"
               |        android:theme="@style/Theme.Vulnerableapp"
               |        tools:targetApi="31">
               |        <activity
               |            android:name=".MainActivity"
               |            android:exported="true">
               |            <intent-filter>
               |                <action android:name="android.intent.action.MAIN" />
               |                <category android:name="android.intent.category.LAUNCHER" />
               |            </intent-filter>
               |
               |            <meta-data
               |                android:name="android.app.lib_name"
               |                android:value="" />
               |        </activity>
               |    </application>
               |</manifest>
               |""".stripMargin,
              "AndroidManifest.xml"
            )
          ),
          // activity exported, but no call which uses data from the intent
          List(
            CodeSnippet(
              """|package no.such.pkg;
                 |
                 |import android.content.Intent;
                 |import android.net.Uri;
                 |import android.os.Bundle;
                 |import android.support.v7.app.AppCompatActivity;
                 |
                 |import java.io.IOException;
                 |
                 |public class DeepLinkActivity extends AppCompatActivity {
                 |    @Override
                 |    protected void onCreate(Bundle savedInstanceState) {
                 |        super.onCreate(savedInstanceState);
                 |        setContentView(R.layout.activity_main);
                 |        Intent intent = getIntent();
                 |        // no call to processDeeplink
                 |        finish();
                 |    }
                 |
                 |    private void processDeeplink(Uri uri) {
                 |        if("http".equals(uri.getScheme()) && "java.vroooom.io".equals(uri.getHost())) {
                 |            String path = uri.getPath();
                 |            String secret = uri.getQueryParameter("secret");
                 |            if("/cmdrun".equals(path) && secret.equals("b4dc0ffee")) {
                 |                String cmd = uri.getQueryParameter("cmd");
                 |                String[] cmdArray = new String[3];
                 |                cmdArray[0] = "sh";
                 |                cmdArray[1] = "-c";
                 |                cmdArray[2] = cmd;
                 |                try {
                 |                    Runtime.getRuntime().exec(cmdArray);
                 |                } catch (IOException e) {
                 |                    System.out.print("error");
                 |                }
                 |            }
                 |        }
                 |    }
                 |}
                 |""".stripMargin,
              "DeepLinkActivity.java"
            ),
            CodeSnippet(
              """|<?xml version="1.0" encoding="utf-8"?>
                 |<manifest xmlns:android="http://schemas.android.com/apk/res/android"
                 |    xmlns:tools="http://schemas.android.com/tools">
                 |    <uses-permission android:name="android.permission.INTERNET" />
                 |    <application
                 |        android:allowBackup="true"
                 |        android:dataExtractionRules="@xml/data_extraction_rules"
                 |        android:fullBackupContent="@xml/backup_rules"
                 |        android:usesCleartextTraffic="true"
                 |        android:icon="@mipmap/ic_launcher"
                 |        android:label="@string/app_name"
                 |        android:roundIcon="@mipmap/ic_launcher_round"
                 |        android:supportsRtl="true"
                 |        android:theme="@style/Theme.Vulnerableapp"
                 |        tools:targetApi="31">
                 |        <activity
                 |            android:name=".MainActivity"
                 |            android:exported="true">
                 |            <intent-filter>
                 |                <action android:name="android.intent.action.MAIN" />
                 |                <category android:name="android.intent.category.LAUNCHER" />
                 |            </intent-filter>
                 |
                 |            <meta-data
                 |                android:name="android.app.lib_name"
                 |                android:value="" />
                 |        </activity>
                 |
                 |        <activity android:name="DeepLinkActivity" android:exported="true">
                 |            <intent-filter android:autoVerify="true">
                 |                <action android:name="android.intent.action.VIEW" />
                 |                <category android:name="android.intent.category.DEFAULT" />
                 |                <category android:name="android.intent.category.BROWSABLE" />
                 |                <category android:name="android.intent.category.LAUNCHER" />
                 |                <data android:host="vulnerable" android:scheme="vroooom" />
                 |                <data android:scheme="http" />
                 |                <data android:scheme="https" />
                 |                <data android:host="java.vroooom.io" />
                 |            </intent-filter>
                 |        </activity>
                 |    </application>
                 |</manifest>
                 |""".stripMargin,
              "AndroidManifest.xml"
            )
          ),
          // data from the intent does not reach Runtime.getRuntime.exec
          List(
            CodeSnippet(
              """|package no.such.pkg;
                 |
                 |import android.content.Intent;
                 |import android.net.Uri;
                 |import android.os.Bundle;
                 |import android.support.v7.app.AppCompatActivity;
                 |
                 |import java.io.IOException;
                 |
                 |public class DeepLinkActivity extends AppCompatActivity {
                 |    @Override
                 |    protected void onCreate(Bundle savedInstanceState) {
                 |        super.onCreate(savedInstanceState);
                 |        setContentView(R.layout.activity_main);
                 |        Intent intent = getIntent();
                 |        Uri uri;
                 |        if(intent != null && Intent.ACTION_VIEW.equals(intent.getAction()) && (uri = intent.getData()) != null) {
                 |            processDeeplink(uri);
                 |        }
                 |        finish();
                 |    }
                 |
                 |    private void processDeeplink(Uri uri) {
                 |        if("http".equals(uri.getScheme()) && "java.vroooom.io".equals(uri.getHost())) {
                 |            String path = uri.getPath();
                 |            String secret = uri.getQueryParameter("secret");
                 |            if("/cmdrun".equals(path) && secret.equals("b4dc0ffee")) {
                 |                String cmd = uri.getQueryParameter("cmd");
                 |                String[] cmdArray = new String[3];
                 |                cmdArray[0] = "sh";
                 |                cmdArray[1] = "-c";
                 |                cmdArray[2] = cmd;
                 |                try {
                 |                    // no call to exec here...
                 |                    System.out.println(cmdArray);
                 |                } catch (IOException e) {
                 |                    System.out.print("error");
                 |                }
                 |            }
                 |        }
                 |    }
                 |}
                 |""".stripMargin,
              "DeepLinkActivity.java"
            ),
            CodeSnippet(
              """|<?xml version="1.0" encoding="utf-8"?>
                 |<manifest xmlns:android="http://schemas.android.com/apk/res/android"
                 |    xmlns:tools="http://schemas.android.com/tools">
                 |    <uses-permission android:name="android.permission.INTERNET" />
                 |    <application
                 |        android:allowBackup="true"
                 |        android:dataExtractionRules="@xml/data_extraction_rules"
                 |        android:fullBackupContent="@xml/backup_rules"
                 |        android:usesCleartextTraffic="true"
                 |        android:icon="@mipmap/ic_launcher"
                 |        android:label="@string/app_name"
                 |        android:roundIcon="@mipmap/ic_launcher_round"
                 |        android:supportsRtl="true"
                 |        android:theme="@style/Theme.Vulnerableapp"
                 |        tools:targetApi="31">
                 |        <activity
                 |            android:name=".MainActivity"
                 |            android:exported="true">
                 |            <intent-filter>
                 |                <action android:name="android.intent.action.MAIN" />
                 |                <category android:name="android.intent.category.LAUNCHER" />
                 |            </intent-filter>
                 |
                 |            <meta-data
                 |                android:name="android.app.lib_name"
                 |                android:value="" />
                 |        </activity>
                 |
                 |        <activity android:name="DeepLinkActivity" android:exported="true">
                 |            <intent-filter android:autoVerify="true">
                 |                <action android:name="android.intent.action.VIEW" />
                 |                <category android:name="android.intent.category.DEFAULT" />
                 |                <category android:name="android.intent.category.BROWSABLE" />
                 |                <category android:name="android.intent.category.LAUNCHER" />
                 |                <data android:host="vulnerable" android:scheme="vroooom" />
                 |                <data android:scheme="http" />
                 |                <data android:scheme="https" />
                 |                <data android:host="java.vroooom.io" />
                 |            </intent-filter>
                 |        </activity>
                 |    </application>
                 |</manifest>
                 |""".stripMargin,
              "AndroidManifest.xml"
            )
          )
        )
      )
    )
}
