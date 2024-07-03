package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.semanticcpg.language.*

class ConfigFileTests extends KotlinCode2CpgFixture(withOssDataflow = false) {

  "CPG for code with an xml file named `AndroidManifest.xml`" should {
    val configFileContent =
      """
        |<?xml version="1.0" encoding="utf-8"?>
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
        |</manifest>""".stripMargin
    val cpg = code("fun main() = println(0xff)").moreCode(configFileContent, "AndroidManifest.xml")

    "should contain a CONFIG_FILE node with the correct props set" in {
      val List(c) = cpg.configFile.l
      c.content shouldBe configFileContent
      c.name shouldBe "AndroidManifest.xml"
    }
  }
}
