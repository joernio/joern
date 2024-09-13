package io.joern.scanners.android

import io.joern.console.*
import io.joern.dataflowengineoss.language.*
import io.joern.dataflowengineoss.queryengine.EngineContext
import io.joern.dataflowengineoss.semanticsloader.NoSemantics
import io.joern.macros.QueryMacros.*
import io.joern.scanners.*
import io.shiftleft.semanticcpg.language.*

object ExternalStorage extends QueryBundle {
  implicit val engineContext: EngineContext = EngineContext(NoSemantics)
  implicit val resolver: ICallResolver      = NoResolve

  // TODO: improve matching around external storage permissions
  @q
  def externalStorageToDexClassLoader()(implicit engineContext: EngineContext): Query =
    Query.make(
      name = "external-storage-to-dex-classloader",
      author = Crew.claudiu,
      title = "Data from external storage ends up in dex classloader, leading to code execution.",
      description = "-",
      score = 9,
      withStrRep({ cpg =>
        import io.joern.x2cpg.Defines.ConstructorMethodName
        import io.shiftleft.semanticcpg.language.android.*

        def externalStorageReads =
          if (cpg.appManifest.hasReadExternalStoragePermission.nonEmpty)
            cpg.getExternalStorageDir
          else Iterator.empty
        def dexClassLoadersWithExternalStorageInit =
          cpg.dexClassLoader
            .where(
              _.method.call
                .nameExact(ConstructorMethodName)
                .where(_.argument(0).isIdentifier.typeFullNameExact("dalvik.system.DexClassLoader"))
                .where(_.argument(1).reachableBy(externalStorageReads))
            )
        def loadClassCalls =
          dexClassLoadersWithExternalStorageInit.referencingIdentifiers.inCall.nameExact("loadClass")
        def reflectInvoke = cpg.call.methodFullNameExact(
          "java.lang.reflect.Method.invoke:java.lang.Object(java.lang.Object,java.lang.Object[])"
        )
        reflectInvoke.where(_.argument(1).reachableBy(loadClassCalls))
      }),
      tags = List(QueryTags.android),
      multiFileCodeExamples = MultiFileCodeExamples(
        positive = List(
          List(
            CodeSnippet(
              """|package io.vroooom.vulnerableapp;
                 |
                 |import android.Manifest;
                 |import android.content.pm.PackageManager;
                 |import android.os.Bundle;
                 |import android.os.Environment;
                 |import android.support.v4.app.ActivityCompat;
                 |import android.support.v4.content.ContextCompat;
                 |import android.support.v7.app.AppCompatActivity;
                 |import android.util.Log;
                 |
                 |import java.io.File;
                 |import java.lang.reflect.Method;
                 |
                 |import dalvik.system.DexClassLoader;
                 |
                 |public class DexClassLoaderActivity extends AppCompatActivity {
                 |
                 |    @Override
                 |    protected void onCreate(Bundle savedInstanceState) {
                 |        super.onCreate(savedInstanceState);
                 |        setContentView(R.layout.activity_main);
                 |
                 |        if (ContextCompat.checkSelfPermission(this, Manifest.permission.READ_EXTERNAL_STORAGE)
                 |                != PackageManager.PERMISSION_GRANTED) {
                 |            ActivityCompat.requestPermissions(this, new String[] { Manifest.permission.READ_EXTERNAL_STORAGE }, 1234);
                 |        } else {
                 |            String externalStorageRoot = Environment.getExternalStorageDirectory().toString();
                 |            listFiles(externalStorageRoot);
                 |
                 |            String pathToDex = externalStorageRoot + "/Beep.zip";
                 |            loadDexFromPath(pathToDex);
                 |        }
                 |    }
                 |
                 |    private void listFiles(String path) {
                 |        Log.d("Files", "Path: " + path);
                 |        File directory = new File(path);
                 |        File[] files = directory.listFiles();
                 |        Log.d("Files", "Size: "+ files.length);
                 |        for (int i = 0; i < files.length; i++) {
                 |            Log.d("Files", "FileName:" + files[i].getName());
                 |        }
                 |    }
                 |
                 |    private void loadDexFromPath(String path) {
                 |        try {
                 |            File tmpDir = getDir("dex", 0);
                 |
                 |            DexClassLoader classloader = new DexClassLoader(path, tmpDir.getAbsolutePath(), null, this.getClass().getClassLoader());
                 |            final Class<Object> classToLoad = (Class<Object>) classloader.loadClass("Beep");
                 |            final Object myInstance  = classToLoad.newInstance();
                 |            final Method returnString = classToLoad.getMethod("getName");
                 |            String result = (String) returnString.invoke(myInstance);
                 |            Log.e("Test", result);
                 |        } catch (Exception e) {
                 |            e.printStackTrace();
                 |        }
                 |    }
                 |}
                 |""".stripMargin,
              "io/vroooom/vulnerableapp/DexClassLoaderActivity.java"
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
                |
                |        <activity
                |            android:name=".MainActivityJava"
                |            android:exported="true">
                |            <intent-filter>
                |                <action android:name="android.intent.action.MAIN" />
                |                <category android:name="android.intent.category.LAUNCHER" />
                |            </intent-filter>
                |            <meta-data
                |                android:name="android.app.lib_name"
                |                android:value="" />
                |        </activity>
                |    </application>
                |</manifest>
                |""".stripMargin,
              "AndroidManifest.xml"
            )
          )
        ),
        negative = List(
          // does not specify permission in manifest
          List(
            CodeSnippet(
              """|package io.vroooom.vulnerableapp;
                 |
                 |import android.Manifest;
                 |import android.content.pm.PackageManager;
                 |import android.os.Bundle;
                 |import android.os.Environment;
                 |import android.support.v4.app.ActivityCompat;
                 |import android.support.v4.content.ContextCompat;
                 |import android.support.v7.app.AppCompatActivity;
                 |import android.util.Log;
                 |
                 |import java.io.File;
                 |import java.lang.reflect.Method;
                 |
                 |import dalvik.system.DexClassLoader;
                 |
                 |public class DexClassLoaderActivity extends AppCompatActivity {
                 |
                 |    @Override
                 |    protected void onCreate(Bundle savedInstanceState) {
                 |        super.onCreate(savedInstanceState);
                 |        setContentView(R.layout.activity_main);
                 |
                 |        if (ContextCompat.checkSelfPermission(this, Manifest.permission.READ_EXTERNAL_STORAGE)
                 |                != PackageManager.PERMISSION_GRANTED) {
                 |            ActivityCompat.requestPermissions(this, new String[] { Manifest.permission.READ_EXTERNAL_STORAGE }, 1234);
                 |        } else {
                 |            String externalStorageRoot = Environment.getExternalStorageDirectory().toString();
                 |            listFiles(externalStorageRoot);
                 |
                 |            String pathToDex = externalStorageRoot + "/Beep.zip";
                 |            loadDexFromPath(pathToDex);
                 |        }
                 |    }
                 |
                 |    private void listFiles(String path) {
                 |        Log.d("Files", "Path: " + path);
                 |        File directory = new File(path);
                 |        File[] files = directory.listFiles();
                 |        Log.d("Files", "Size: "+ files.length);
                 |        for (int i = 0; i < files.length; i++) {
                 |            Log.d("Files", "FileName:" + files[i].getName());
                 |        }
                 |    }
                 |
                 |    private void loadDexFromPath(String path) {
                 |        try {
                 |            File tmpDir = getDir("dex", 0);
                 |
                 |            DexClassLoader classloader = new DexClassLoader(path, tmpDir.getAbsolutePath(), null, this.getClass().getClassLoader());
                 |            final Class<Object> classToLoad = (Class<Object>) classloader.loadClass("Beep");
                 |            final Object myInstance  = classToLoad.newInstance();
                 |            final Method returnString = classToLoad.getMethod("getName");
                 |            String result = (String) returnString.invoke(myInstance);
                 |            Log.e("Test", result);
                 |        } catch (Exception e) {
                 |            e.printStackTrace();
                 |        }
                 |    }
                 |}
                 |""".stripMargin,
              "io/vroooom/vulnerableapp/DexClassLoaderActivity.java"
            ),
            CodeSnippet(
              """|<?xml version="1.0" encoding="utf-8"?>
                 |<manifest xmlns:android="http://schemas.android.com/apk/res/android"
                 |    xmlns:tools="http://schemas.android.com/tools">
                 |    <uses-permission android:name="android.permission.INTERNET" />
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
                 |        tools:targetApi="31">
                 |
                 |        <activity
                 |            android:name=".MainActivityJava"
                 |            android:exported="true">
                 |            <intent-filter>
                 |                <action android:name="android.intent.action.MAIN" />
                 |                <category android:name="android.intent.category.LAUNCHER" />
                 |            </intent-filter>
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
          // does not read from external storage
          List(
            CodeSnippet(
              """|package io.vroooom.vulnerableapp;
                 |
                 |import android.Manifest;
                 |import android.content.pm.PackageManager;
                 |import android.os.Bundle;
                 |import android.os.Environment;
                 |import android.support.v4.app.ActivityCompat;
                 |import android.support.v4.content.ContextCompat;
                 |import android.support.v7.app.AppCompatActivity;
                 |import android.util.Log;
                 |
                 |import java.io.File;
                 |import java.lang.reflect.Method;
                 |
                 |import dalvik.system.DexClassLoader;
                 |
                 |public class DexClassLoaderActivity extends AppCompatActivity {
                 |
                 |    @Override
                 |    protected void onCreate(Bundle savedInstanceState) {
                 |        super.onCreate(savedInstanceState);
                 |        setContentView(R.layout.activity_main);
                 |
                 |        if (ContextCompat.checkSelfPermission(this, Manifest.permission.READ_EXTERNAL_STORAGE)
                 |                != PackageManager.PERMISSION_GRANTED) {
                 |            ActivityCompat.requestPermissions(this, new String[] { Manifest.permission.READ_EXTERNAL_STORAGE }, 1234);
                 |        } else {
                 |            String somePath = "/an/app/internal/path";
                 |            listFiles(somePath);
                 |            loadDexFromPath(somePath);
                 |        }
                 |    }
                 |
                 |    private void listFiles(String path) {
                 |        Log.d("Files", "Path: " + path);
                 |        File directory = new File(path);
                 |        File[] files = directory.listFiles();
                 |        Log.d("Files", "Size: "+ files.length);
                 |        for (int i = 0; i < files.length; i++) {
                 |            Log.d("Files", "FileName:" + files[i].getName());
                 |        }
                 |    }
                 |
                 |    private void loadDexFromPath(String path) {
                 |        try {
                 |            File tmpDir = getDir("dex", 0);
                 |
                 |            DexClassLoader classloader = new DexClassLoader(path, tmpDir.getAbsolutePath(), null, this.getClass().getClassLoader());
                 |            final Class<Object> classToLoad = (Class<Object>) classloader.loadClass("Beep");
                 |            final Object myInstance  = classToLoad.newInstance();
                 |            final Method returnString = classToLoad.getMethod("getName");
                 |            String result = (String) returnString.invoke(myInstance);
                 |            Log.e("Test", result);
                 |        } catch (Exception e) {
                 |            e.printStackTrace();
                 |        }
                 |    }
                 |}
                 |""".stripMargin,
              "io/vroooom/vulnerableapp/DexClassLoaderActivity.java"
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
                 |        tools:targetApi="31">
                 |
                 |        <activity
                 |            android:name=".MainActivityJava"
                 |            android:exported="true">
                 |            <intent-filter>
                 |                <action android:name="android.intent.action.MAIN" />
                 |                <category android:name="android.intent.category.LAUNCHER" />
                 |            </intent-filter>
                 |            <meta-data
                 |                android:name="android.app.lib_name"
                 |                android:value="" />
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
