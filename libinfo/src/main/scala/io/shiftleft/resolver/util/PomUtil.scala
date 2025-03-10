package io.shiftleft.resolver.util

import io.shiftleft.resolver.api.{Coordinate, MetaData}
import io.shiftleft.resolver.impl.IdMaven

import scala.xml.{Elem, NodeSeq, XML}

object PomContext {
  def empty: PomContext = {
    PomContext(None, None, Map.empty)
  }
}
case class PomContext(groupId: Option[String],
                      version: Option[String],
                      properties: Map[String, String]) {
  def merge(other: PomContext): PomContext = {
    PomContext(
      if (other.groupId.isDefined) other.groupId else groupId,
      if (other.version.isDefined) other.version else version,
      properties.concat(other.properties)
    )
  }
}

object PomUtil {
  extension (elems: NodeSeq) {
    def textStripped: String = {
      val text = elems.text
      if (text == "") {
        return text
      }

      val start = text.indexWhere(char => char != ' ' && char != '\n' && char != '\r')
      val end = text.indexWhere(char => !(char != ' ' && char != '\n' && char != '\r'), start)
      val effectiveEnd =
        if (end == -1) {
          text.length
        } else {
          end
        }
      text.substring(start, effectiveEnd)
    }
  }

  def loadXml(xmlString: String): Elem = {
    XML.loadString(xmlString)
  }

  def extractParent(elem: Elem): Option[Coordinate[IdMaven]] = {
    val parentNodes = elem \ "parent"
    if (parentNodes.nonEmpty) {
      val groupId = getChildAsText(parentNodes, "groupId")
      val artifactId = getChildAsText(parentNodes, "artifactId")
      val version = getChildAsText(parentNodes, "version")
      val artifact = Coordinate(IdMaven(groupId.get, artifactId.get), version.get)
      Some(artifact)
    } else {
      None
    }
  }

  def extractContext(root: Elem, context: PomContext): PomContext = {
    val propertyElements = root \ "properties" \ "_"

    val groupId = getChildAsText(root, "groupId")
    val version =  getChildAsText(root, "version")

    PomContext(groupId, version, propertyElements.map(elem => (elem.label, elem.textStripped)).toMap)
  }

  def extractContent(root: Elem,
                     context: PomContext): MetaData[IdMaven] = {
    val groupId = getChildAsTextInterpolated(root, "groupId", context)
    val artifactId = getChildAsTextInterpolated(root, "artifactId", context)
    val version = getChildAsTextInterpolated(root, "version", context)

    val id = IdMaven(groupId.get, artifactId.get)

    val depElements = root \ "dependencies" \ "dependency"
    val dependencies = depElements.flatMap { dep =>
      getDependencyCoordinate(dep, context)
    }

    val finalVersion = version.orElse(context.version).get
    MetaData(id, finalVersion, dependencies.toVector)
  }

  private def getChildAsText(xmlElements: NodeSeq,
                             childName: String,
                            ): Option[String] = {
    val valueOption =
      (xmlElements \ childName).textStripped match {
        case "" => None
        case some => Some(some)
      }

    valueOption
  }

  private def getChildAsTextInterpolated(xmlElements: NodeSeq,
                                         childName: String,
                                         context: PomContext
                                        ): Option[String] = {
    val valueOption =
      (xmlElements \ childName).textStripped match {
        case "" => None
        case some => Some(some)
      }

    valueOption
      .orElse(inheritValue(childName, context))
      .flatMap { value =>
      val interpolatedValue = interpolate(value, context)

      if (interpolatedValue.isEmpty) {
        println(s"Failed interpolation of $value found as test for $childName")
      }
      interpolatedValue
    }
  }

  private def inheritValue(name: String,
                           context: PomContext,
                          ): Option[String] = {
    name match {
      case "groupId" =>
        context.groupId
      case _ =>
        None
    }
  }

  private def getDependencyCoordinate(dep: NodeSeq,
                                      context: PomContext): Option[Coordinate[IdMaven]] = {
    // TODO System scoped dependencies are not coming from an artifactory.
    //      We will need to handle them separately
    if ((dep \ "scope").textStripped == "system") {
      return None
    }

    val groupId = getChildAsTextInterpolated(dep, "groupId", context)
    val artifactId = getChildAsTextInterpolated(dep, "artifactId", context)
    val version = getChildAsTextInterpolated(dep, "version", context)

    version.map { v =>
      Coordinate(IdMaven(groupId.get, artifactId.get), v)
    }
  }

  private def interpolate(value: String, context: PomContext): Option[String] = {
    if (value.startsWith("${")) {
      val propertyName = value.substring("${".length, value.length - "}".length)
      val prefixes = List("project.", "pom.")
      val matchedPrefixOption = prefixes.find(prefix => propertyName.startsWith(prefix))

      matchedPrefixOption match {
        case Some(prefix) =>
          val entity = propertyName.substring(prefix.length, propertyName.length)
          entity match {
            case "groupId" =>
              context.groupId
            case _ =>
              None
          }
        case None =>
          val propertyValue = context.properties.get(propertyName)
          propertyValue
      }
    } else {
      Some(value)
    }
  }
}
