package io.joern.scanners.android

import io.joern.console.scan._
import io.shiftleft.codepropertygraph.generated.nodes.ConfigFile
import io.joern.suites.AndroidQueryTestSuite

class AndroidMisconfigurationsTests extends AndroidQueryTestSuite {

  override def queryBundle = AndroidMisconfigurations

  private def makeAndroidXml(allowBackup: Boolean): String = {
    s"""|<?xml version="1.0" encoding="utf-8"?>
       |<manifest xmlns:android="http://schemas.android.com/apk/res/android"
       |    package="com.example.slimandroid">
       |
       |    <application
       |        android:allowBackup="$allowBackup"
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
  }

  "should match a config file when `allowBackup` is set to `true`" in {
    val cpgWithManifest = code("fun main() = println(0xff)")
      .moreCode(makeAndroidXml(true), "AndroidManifest.xml")
    val query = queryBundle.manifestXmlDebuggableEnabled()
    query(cpgWithManifest).flatMap(_.evidence).collect { case c: ConfigFile => c }.size shouldBe 1
  }

  "should not match a config file when `allowBackup` is set to `false`" in {
    val cpgWithManifest = code("fun main() = println(0xff)")
      .moreCode(makeAndroidXml(false), "AndroidManifest.xml")
    val query = queryBundle.manifestXmlDebuggableEnabled()
    query(cpgWithManifest).flatMap(_.evidence).collect { case c: ConfigFile => c }.size shouldBe 0
  }

  "should not match anything when there is no file named `AndroidManifest.xml` in the cpg" in {
    val cpgNoManifest = code("fun main() = println(0xff)")
      .moreCode(makeAndroidXml(false), "NOPNOPNOPNOPNOPNOP.xml")
    val query = queryBundle.manifestXmlDebuggableEnabled()
    query(cpgNoManifest).flatMap(_.evidence).collect { case c: ConfigFile => c }.size shouldBe 0
  }

  "should not match anything when the file named `AndroidManifest.xml` is empty" in {
    val cpgNoManifest = code("fun main() = println(0xff)").moreCode("", "AndroidManifest.xml")
    val query         = queryBundle.manifestXmlDebuggableEnabled()
    query(cpgNoManifest).flatMap(_.evidence).collect { case c: ConfigFile => c }.size shouldBe 0
  }

  "should not match anything when the file named `AndroidManifest.xml` contains invalid XML" in {
    val invalidXml =
      """
        |<?xml version="1.0"?>
        |<!DOCTYPE lolz [
        | <!ENTITY lol "lol">
        | <!ELEMENT lolz (#PCDATA)>
        | <!ENTITY lol1 "&lol;&lol;&lol;&lol;&lol;&lol;&lol;&lol;&lol;&lol;">
        | <!ENTITY lol2 "&lol1;&lol1;&lol1;&lol1;&lol1;&lol1;&lol1;&lol1;&lol1;&lol1;">
        | <UNFINISHED_BILLION_LAUGS
        |<lolz>&lol2;</lolz>
        |""".stripMargin
    val cpgNoManifest = cpg.moreCode(invalidXml, "AndroidManifest.xml")
    val query         = queryBundle.manifestXmlDebuggableEnabled()
    query(cpgNoManifest).flatMap(_.evidence).collect { case c: ConfigFile => c }.size shouldBe 0
  }
}
