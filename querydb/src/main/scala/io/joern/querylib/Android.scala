package io.joern.querylib.android

import io.joern.x2cpg.utils.xml.SecureXmlParsing
import io.shiftleft.codepropertygraph.generated.nodes
import overflowdb.traversal._

object Constants {
  val androidUri         = "http://schemas.android.com/apk/res/android"
  val androidManifestXml = "AndroidManifest.xml"
}

class AndroidManifestXmlTraversal(val traversal: Traversal[nodes.ConfigFile]) extends AnyVal {
  def exportedAndroidActivityNames =
    traversal
      .filter(_.name.endsWith(Constants.androidManifestXml))
      .map(_.content)
      .flatMap(SecureXmlParsing.parseXml)
      .filter(_.label == "manifest")
      .flatMap(_.child)
      .filter(_.label == "application")
      .flatMap(_.child)
      .filter(_.label == "activity")
      .flatMap { activityNode =>
        val validIntentFilters = {
          activityNode
            .flatMap(_.child)
            .filter(_.label == "intent-filter")
            .flatMap(_.child)
            .filter(_.label == "category")
            .filter { node =>
              val categoryName = node.attribute(Constants.androidUri, "name")
              categoryName match {
                case Some(n) => n.toString == "android.intent.category.DEFAULT"
                case None    => false
              }
            }
        }
        if (validIntentFilters.nonEmpty) {
          val activityName = activityNode.attribute(Constants.androidUri, "name")
          activityName match {
            case Some(n) => Some(n.toString)
            case None    => None
          }
        } else None
      }
}
