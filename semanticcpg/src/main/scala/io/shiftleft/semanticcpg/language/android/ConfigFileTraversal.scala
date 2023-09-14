package io.shiftleft.semanticcpg.language.android

import io.joern.semanticcpg.utils.SecureXmlParsing
import io.shiftleft.codepropertygraph.generated.nodes.ConfigFile
import io.shiftleft.semanticcpg.language.*

class ConfigFileTraversal(val traversal: Iterator[ConfigFile]) extends AnyVal {
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

  def hasReadExternalStoragePermission =
    traversal
      .filter(_.name.endsWith(Constants.androidManifestXml))
      .map(_.content)
      .flatMap(SecureXmlParsing.parseXml)
      .filter(_.label == "manifest")
      .flatMap(_.child)
      .filter(_.label == "uses-permission")
      .flatMap { applicationNode =>
        val activityName = applicationNode.attribute(Constants.androidUri, "name")
        activityName match {
          case Some(n) if n.toString == "android.permission.READ_EXTERNAL_STORAGE" => Some(true)
          case _                                                                   => None
        }
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

  def exportedBroadcastReceiverNames =
    traversal
      .filter(_.name.endsWith(Constants.androidManifestXml))
      .map(_.content)
      .flatMap(SecureXmlParsing.parseXml)
      .filter(_.label == "manifest")
      .flatMap(_.child)
      .filter(_.label == "application")
      .flatMap(_.child)
      .filter(_.label == "receiver")
      .flatMap { receiverNode =>
        val hasIntentFilter = receiverNode.flatMap(_.child).filter(_.label == "intent-filter").nonEmpty
        if (hasIntentFilter) {
          val isExported = receiverNode.attribute(Constants.androidUri, "exported")
          isExported match {
            case Some(n) if n.toString == "true" => Some(receiverNode)
            case _                               => None
          }
        } else None
      }
      .flatMap { node =>
        val name = node.attribute(Constants.androidUri, "name")
        name match {
          case Some(n) => Some(n.toString().stripPrefix("."))
          case _       => None
        }
      }

}
