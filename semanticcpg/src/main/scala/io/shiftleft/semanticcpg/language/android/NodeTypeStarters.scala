package io.shiftleft.semanticcpg.language.android

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._
import overflowdb.traversal.Traversal

class NodeTypeStarters(cpg: Cpg) {
  def webView: Traversal[Local] =
    cpg.local.typeFullNameExact("android.webkit.WebView")

  def appManifest: Traversal[ConfigFile] =
    cpg.configFile.filter(_.name.endsWith(Constants.androidManifestXml))

  def getExternalStorageDir: Traversal[Call] =
    cpg.call
      .nameExact("getExternalStorageDirectory")
      .where(_.argument(0).isIdentifier.typeFullNameExact("android.os.Environment"))

  def dexClassLoader: Traversal[Local] =
    cpg.local.typeFullNameExact("dalvik.system.DexClassLoader")

  def broadcastReceivers: Traversal[TypeDecl] =
    cpg.method
      .nameExact("onReceive")
      .where(_.parameter.index(1).typeFullNameExact("android.content.Context"))
      .where(_.parameter.index(2).typeFullNameExact("android.content.Intent"))
      .typeDecl

  def registerReceiver: Traversal[Call] =
    cpg.call
      .nameExact("registerReceiver")
      .typeFullNameExact("void")
      .where(_.argument(2).isIdentifier.typeFullNameExact("android.content.IntentFilter"))

  def registeredBroadcastReceivers =
    cpg.broadcastReceivers.filter { broadcastReceiver =>
      cpg.registerReceiver.argument(1).isIdentifier.typeFullName.exists(_ == broadcastReceiver.fullName)
    }
}
