package io.joern.scanners.android

import io.joern.scanners.*
import io.joern.console.*
import io.joern.dataflowengineoss.queryengine.EngineContext
import io.joern.dataflowengineoss.semanticsloader.NoSemantics
import io.joern.macros.QueryMacros.*
import io.shiftleft.semanticcpg.language.*
import io.joern.dataflowengineoss.language.*

object ArbitraryFileWrites extends QueryBundle {
  implicit val engineContext: EngineContext = EngineContext(NoSemantics)
  implicit val resolver: ICallResolver      = NoResolve

  // todo: improve accuracy, might lead to high number of false positives
  // todo: take into account behaviour for older SDKs
  @q
  def broadcastToFileWrite()(implicit engineContext: EngineContext): Query =
    Query.make(
      name = "broadcast-to-file-write",
      author = Crew.claudiu,
      title = "Data from a broadcast ends up in a file write operation.",
      description = "-",
      score = 6,
      withStrRep({ cpg =>
        import io.shiftleft.semanticcpg.language.android._
        def exposedBroadcastReceivers =
          cpg.registeredBroadcastReceivers.filter { receiver =>
            cpg.appManifest.exportedBroadcastReceiverNames.exists(receiver.name == _)
          }
        def exposedBroadcastReceiverData =
          exposedBroadcastReceivers.method.nameExact("onReceive").parameter.index(2)
        def fileWriteCalls =
          cpg.call.nameExact("write").where(_.argument.isIdentifier.typeFullNameExact("java.io.FileOutputStream"))
        fileWriteCalls.where(_.argument.reachableBy(exposedBroadcastReceiverData))
      }),
      tags = List(QueryTags.android),
      multiFileCodeExamples = MultiFileCodeExamples(
        positive = List(
          List(
            CodeSnippet(
              """|package io.vrooom.vulnerableapp;
                 |
                 |import android.content.BroadcastReceiver;
                 |import android.content.Context;
                 |import android.content.Intent;
                 |import android.util.Log;
                 |
                 |import java.io.FileOutputStream;
                 |import java.io.IOException;
                 |
                 |public class WriteFileBroadcastReceiver extends BroadcastReceiver {
                 |    public WriteFileBroadcastReceiver() {}
                 |
                 |    @Override
                 |    public void onReceive(Context context, Intent intent) {
                 |        String fileNameFromBroadcast = intent.getStringExtra("filename");
                 |        String contentFromBroadcast = intent.getStringExtra("content");
                 |        try {
                 |            String filePath = context.getFilesDir().toString() + fileNameFromBroadcast;
                 |            FileOutputStream outputStream = new FileOutputStream(filePath);
                 |            byte[] strToBytes = contentFromBroadcast.getBytes();
                 |            outputStream.write(strToBytes);
                 |            outputStream.close();
                 |            Log.d("WriteFileBroadcastReceiver", "filePath: " + filePath);
                 |            Log.d("WriteFileBroadcastReceiver", "content: " + contentFromBroadcast);
                 |        } catch (IOException ex) {
                 |            Log.d("exception", ex.getLocalizedMessage());
                 |        }
                 |    }
                 |}
                 |""".stripMargin,
              "io/vrooom/vulnerableapp/WriteFileBroadcastReceiver.java"
            ),
            CodeSnippet(
              """|package io.vrooom.vulnerableapp;
                   |
                   |import android.content.Intent;
                   |import android.content.IntentFilter;
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
                   |        IntentFilter filter = new IntentFilter();
                   |        filter.addAction(getPackageName() + "io.vrooom.intent.action.WRITE_FILE");
                   |        WriteFileBroadcastReceiver myReceiver = new WriteFileBroadcastReceiver();
                   |        registerReceiver(myReceiver, filter);
                   |    }
                   |}
                   |""".stripMargin,
              "io/vrooom/vulnerableapp/MainActivity.java"
            ),
            CodeSnippet(
              """|<?xml version="1.0" encoding="utf-8"?>
                 |<manifest xmlns:android="http://schemas.android.com/apk/res/android"
                 |    xmlns:tools="http://schemas.android.com/tools">
                 |    <uses-permission android:name="android.permission.INTERNET" />
                 |    <uses-permission android:name="android.permission.READ_EXTERNAL_STORAGE" />
                 |
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
                 |        android:requestLegacyExternalStorage="true"
                 |        tools:targetApi="31">
                 |        <activity
                 |            android:name=".MainActivity"
                 |            android:exported="true">
                 |            <intent-filter>
                 |                <action android:name="android.intent.action.MAIN" />
                 |                <category android:name="android.intent.category.LAUNCHER" />
                 |            </intent-filter>
                 |        </activity>
                 |        <receiver android:name=".WriteFileBroadcastReceiver" android:exported="true">
                 |            <intent-filter>
                 |                <action android:name="io.vrooom.intent.action.WRITE_FILE" />
                 |            </intent-filter>
                 |        </receiver>
                 |    </application>
                 |</manifest>
                |""".stripMargin,
              "AndroidManifest.xml"
            )
          ),
          List(
            CodeSnippet(
              """|package io.vrooom.vulnerableapp
                 |
                 |import android.content.BroadcastReceiver
                 |import android.content.Context
                 |import android.content.Intent
                 |import android.util.Log
                 |import java.io.FileOutputStream
                 |import java.io.IOException
                 |import java.nio.charset.Charset
                 |
                 |class WriteFileBroadcastReceiver : BroadcastReceiver() {
                 |    override fun onReceive(context: Context?, intent: Intent?) {
                 |        val fileNameFromBroadcast = intent!!.getStringExtra("filename")
                 |        val contentFromBroadcast = intent!!.getStringExtra("content")
                 |        try {
                 |            val filePath = context!!.filesDir.toString() + fileNameFromBroadcast
                 |            val outputStream: FileOutputStream = FileOutputStream(filePath)
                 |            val strToBytes = contentFromBroadcast!!.toByteArray(Charset.defaultCharset())
                 |            outputStream.write(strToBytes)
                 |            outputStream.close()
                 |            Log.d("WriteFileBroadcastReceiver", "filePath: " + filePath)
                 |            Log.d("WriteFileBroadcastReceiver", "content: " + contentFromBroadcast)
                 |        } catch (ex: IOException) {
                 |            Log.d("exception", ex.getLocalizedMessage())
                 |        }
                 |    }
                 |}
                 |""".stripMargin,
              "WriteFileBroadcastReceiver.kt"
            ),
            CodeSnippet(
              """|package io.vrooom.vulnerableapp
                 |
                 |import android.os.Bundle
                 |import android.support.v7.app.AppCompatActivity
                 |import android.webkit.WebView
                 |import android.content.IntentFilter
                 |
                 |class MainActivity : AppCompatActivity() {
                 |    override fun onCreate(savedInstanceState: Bundle?) {
                 |        super.onCreate(savedInstanceState)
                 |        setContentView(R.layout.activity_main)
                 |
                 |        val filter: IntentFilter = IntentFilter()
                 |        filter.addAction(packageName + "io.vrooom.intent.action.WRITE_FILE")
                 |        val receiver = WriteFileBroadcastReceiver()
                 |        registerReceiver(receiver, filter)
                 |    }
                 |}
                 |""".stripMargin,
              "MainActivity.kt"
            ),
            CodeSnippet(
              """|<?xml version="1.0" encoding="utf-8"?>
                 |<manifest xmlns:android="http://schemas.android.com/apk/res/android"
                 |    xmlns:tools="http://schemas.android.com/tools">
                 |    <uses-permission android:name="android.permission.INTERNET" />
                 |    <uses-permission android:name="android.permission.READ_EXTERNAL_STORAGE" />
                 |
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
                 |        android:requestLegacyExternalStorage="true"
                 |        tools:targetApi="31">
                 |        <activity
                 |            android:name=".MainActivity"
                 |            android:exported="true">
                 |            <intent-filter>
                 |                <action android:name="android.intent.action.MAIN" />
                 |                <category android:name="android.intent.category.LAUNCHER" />
                 |            </intent-filter>
                 |        </activity>
                 |        <receiver android:name=".WriteFileBroadcastReceiver" android:exported="true">
                 |            <intent-filter>
                 |                <action android:name="io.vrooom.intent.action.WRITE_FILE" />
                 |            </intent-filter>
                 |        </receiver>
                 |    </application>
                 |</manifest>
                 |""".stripMargin,
              "AndroidManifest.xml"
            )
          )
        ),
        negative = List(
          // receiver not registered
          List(
            CodeSnippet(
              """|package io.vrooom.vulnerableapp;
                 |
                 |import android.content.BroadcastReceiver;
                 |import android.content.Context;
                 |import android.content.Intent;
                 |import android.util.Log;
                 |
                 |import java.io.FileOutputStream;
                 |import java.io.IOException;
                 |
                 |public class WriteFileBroadcastReceiver extends BroadcastReceiver {
                 |    public WriteFileBroadcastReceiver() {}
                 |
                 |    @Override
                 |    public void onReceive(Context context, Intent intent) {
                 |        String fileNameFromBroadcast = intent.getStringExtra("filename");
                 |        String contentFromBroadcast = intent.getStringExtra("content");
                 |        try {
                 |            String filePath = context.getFilesDir().toString() + fileNameFromBroadcast;
                 |            FileOutputStream outputStream = new FileOutputStream(filePath);
                 |            byte[] strToBytes = contentFromBroadcast.getBytes();
                 |            outputStream.write(strToBytes);
                 |            outputStream.close();
                 |            Log.d("WriteFileBroadcastReceiver", "filePath: " + filePath);
                 |            Log.d("WriteFileBroadcastReceiver", "content: " + contentFromBroadcast);
                 |        } catch (IOException ex) {
                 |            Log.d("exception", ex.getLocalizedMessage());
                 |        }
                 |    }
                 |}
                 |""".stripMargin,
              "io/vrooom/vulnerableapp/WriteFileBroadcastReceiver.java"
            ),
            CodeSnippet(
              """|package io.vrooom.vulnerableapp;
                 |
                 |import android.content.Intent;
                 |import android.content.IntentFilter;
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
                 |        IntentFilter filter = new IntentFilter();
                 |        filter.addAction(getPackageName() + "io.vrooom.intent.action.WRITE_FILE");
                 |        WriteFileBroadcastReceiver myReceiver = new WriteFileBroadcastReceiver();
                 |        // no call to registerReceiver here
                 |    }
                 |}
                 |""".stripMargin,
              "io/vrooom/vulnerableapp/MainActivity.java"
            ),
            CodeSnippet(
              """|<?xml version="1.0" encoding="utf-8"?>
                 |<manifest xmlns:android="http://schemas.android.com/apk/res/android"
                 |    xmlns:tools="http://schemas.android.com/tools">
                 |    <uses-permission android:name="android.permission.INTERNET" />
                 |    <uses-permission android:name="android.permission.READ_EXTERNAL_STORAGE" />
                 |
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
                 |        android:requestLegacyExternalStorage="true"
                 |        tools:targetApi="31">
                 |        <activity
                 |            android:name=".MainActivity"
                 |            android:exported="true">
                 |            <intent-filter>
                 |                <action android:name="android.intent.action.MAIN" />
                 |                <category android:name="android.intent.category.LAUNCHER" />
                 |            </intent-filter>
                 |        </activity>
                 |        <receiver android:name=".WriteFileBroadcastReceiver" android:exported="true">
                 |            <intent-filter>
                 |                <action android:name="io.vrooom.intent.action.WRITE_FILE" />
                 |            </intent-filter>
                 |        </receiver>
                 |    </application>
                 |</manifest>
                 |""".stripMargin,
              "AndroidManifest.xml"
            )
          ),
          // no file write in `onReceive` method
          List(
            CodeSnippet(
              """|package io.vrooom.vulnerableapp;
                 |
                 |import android.content.BroadcastReceiver;
                 |import android.content.Context;
                 |import android.content.Intent;
                 |import android.util.Log;
                 |
                 |import java.io.FileOutputStream;
                 |import java.io.IOException;
                 |
                 |public class WriteFileBroadcastReceiver extends BroadcastReceiver {
                 |    public WriteFileBroadcastReceiver() {}
                 |
                 |    @Override
                 |    public void onReceive(Context context, Intent intent) {
                 |        String fileNameFromBroadcast = intent.getStringExtra("filename");
                 |        String contentFromBroadcast = intent.getStringExtra("content");
                 |        try {
                 |            String filePath = context.getFilesDir().toString() + fileNameFromBroadcast;
                 |            FileOutputStream outputStream = new FileOutputStream(filePath);
                 |            byte[] strToBytes = contentFromBroadcast.getBytes();
                 |            // no call to `outputStream.write` here
                 |            outputStream.close();
                 |            Log.d("WriteFileBroadcastReceiver", "filePath: " + filePath);
                 |            Log.d("WriteFileBroadcastReceiver", "content: " + contentFromBroadcast);
                 |        } catch (IOException ex) {
                 |            Log.d("exception", ex.getLocalizedMessage());
                 |        }
                 |    }
                 |}
                 |""".stripMargin,
              "io/vrooom/vulnerableapp/WriteFileBroadcastReceiver.java"
            ),
            CodeSnippet(
              """|package io.vrooom.vulnerableapp;
                 |
                 |import android.content.Intent;
                 |import android.content.IntentFilter;
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
                 |        IntentFilter filter = new IntentFilter();
                 |        filter.addAction(getPackageName() + "io.vrooom.intent.action.WRITE_FILE");
                 |        WriteFileBroadcastReceiver myReceiver = new WriteFileBroadcastReceiver();
                 |        registerReceiver(myReceiver, filter);
                 |    }
                 |}
                 |""".stripMargin,
              "io/vrooom/vulnerableapp/MainActivity.java"
            ),
            CodeSnippet(
              """|<?xml version="1.0" encoding="utf-8"?>
                 |<manifest xmlns:android="http://schemas.android.com/apk/res/android"
                 |    xmlns:tools="http://schemas.android.com/tools">
                 |    <uses-permission android:name="android.permission.INTERNET" />
                 |    <uses-permission android:name="android.permission.READ_EXTERNAL_STORAGE" />
                 |
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
                 |        android:requestLegacyExternalStorage="true"
                 |        tools:targetApi="31">
                 |        <activity
                 |            android:name=".MainActivity"
                 |            android:exported="true">
                 |            <intent-filter>
                 |                <action android:name="android.intent.action.MAIN" />
                 |                <category android:name="android.intent.category.LAUNCHER" />
                 |            </intent-filter>
                 |        </activity>
                 |        <receiver android:name=".WriteFileBroadcastReceiver" android:exported="true">
                 |            <intent-filter>
                 |                <action android:name="io.vrooom.intent.action.WRITE_FILE" />
                 |            </intent-filter>
                 |        </receiver>
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
