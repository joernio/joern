package io.joern.querydb.language.android

import io.joern.x2cpg.utils.xml.SecureXmlParsing
import io.shiftleft.codepropertygraph.generated.nodes
import overflowdb.traversal._

object Constants {
  val androidUri         = "http://schemas.android.com/apk/res/android"
  val androidManifestXml = "AndroidManifest.xml"
}

class ManifestXmlTraversal(val traversal: Traversal[nodes.ConfigFile]) extends AnyVal {
  def usesCleartextTraffic =
    traversal
      .filter(_.name.endsWith(Constants.androidManifestXml))
      .map(_.content)
      .flatMap(SecureXmlParsing.parseXml)
      .filter(_.label == "manifest")
      .flatMap(_.child)
      .filter(_.label == "application")
      .flatMap { applicationNode =>
        val activityName = applicationNode.attribute(Constants.androidUri, "usesCleartextTraffic")
        activityName.map(_.toString == "true")
      }

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
        /*
        from: https://developer.android.com/guide/components/intents-filters
        Note: To receive implicit intents, you must include the CATEGORY_DEFAULT category in the intent filter.
        The methods startActivity() and startActivityForResult() treat all intents as if they declared the
        CATEGORY_DEFAULT category. If you do not declare this category in your intent filter, no implicit intents
        will resolve to your activity.
         */
        val hasIntentFilterWithDefaultCategory =
          activityNode
            .flatMap(_.child)
            .filter(_.label == "intent-filter")
            .flatMap(_.child)
            .filter(_.label == "category")
            .exists { node =>
              val categoryName = node.attribute(Constants.androidUri, "name")
              categoryName match {
                case Some(n) => n.toString == "android.intent.category.DEFAULT"
                case None    => false
              }
            }
        if (hasIntentFilterWithDefaultCategory) {
          val activityName = activityNode.attribute(Constants.androidUri, "name")
          activityName match {
            case Some(n) => Some(n.toString)
            case None    => None
          }
        } else None
      }
}
