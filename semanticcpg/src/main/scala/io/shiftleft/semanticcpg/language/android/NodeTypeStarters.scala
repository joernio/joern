package io.shiftleft.semanticcpg.language.android

import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*

class NodeTypeStarters(cpg: Cpg) {
  def webView: Iterator[Local] =
    cpg.local.typeFullNameExact("android.webkit.WebView")

  def appManifest: Iterator[ConfigFile] =
    cpg.configFile.filter(_.name.endsWith(Constants.androidManifestXml))

  def getExternalStorageDir: Iterator[Call] =
    cpg.call
      .nameExact("getExternalStorageDirectory")
      .where(_.argument(0).isIdentifier.typeFullNameExact("android.os.Environment"))

  def dexClassLoader: Iterator[Local] =
    cpg.local.typeFullNameExact("dalvik.system.DexClassLoader")

  def broadcastReceivers: Iterator[TypeDecl] =
    cpg.method
      .nameExact("onReceive")
      .where(_.parameter.index(1).typeFullNameExact("android.content.Context"))
      .where(_.parameter.index(2).typeFullNameExact("android.content.Intent"))
      .typeDecl

  def registerReceiver: Iterator[Call] =
    cpg.call
      .nameExact("registerReceiver")
      .where(_.argument(2).isIdentifier.typeFullNameExact("android.content.IntentFilter"))

  def registeredBroadcastReceivers =
    cpg.broadcastReceivers.filter { broadcastReceiver =>
      cpg.registerReceiver.argument(1).isIdentifier.typeFullName.exists(_ == broadcastReceiver.fullName)
    }
}
