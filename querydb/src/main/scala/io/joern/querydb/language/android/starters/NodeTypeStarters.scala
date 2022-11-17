package io.joern.querydb.language.android.starters

import io.joern.querydb.language.android.Constants
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
}
