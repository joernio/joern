package io.shiftleft.semanticcpg.utils

import org.slf4j.{Logger, LoggerFactory}

import javax.xml.parsers.SAXParserFactory
import scala.util.{Failure, Success, Try}
import scala.xml.{Elem, XML}

object SecureXmlParsing {
  def parseXml(content: String): Option[Elem] = {
    Try {
      val spf = SAXParserFactory.newInstance()

      spf.setValidating(false)
      spf.setNamespaceAware(false)
      spf.setXIncludeAware(false)
      spf.setFeature("http://xml.org/sax/features/validation", false)
      spf.setFeature("http://apache.org/xml/features/disallow-doctype-decl", false)
      spf.setFeature("http://apache.org/xml/features/nonvalidating/load-dtd-grammar", false)
      spf.setFeature("http://apache.org/xml/features/nonvalidating/load-external-dtd", false)
      spf.setFeature("http://xml.org/sax/features/external-parameter-entities", false)
      spf.setFeature("http://xml.org/sax/features/external-general-entities", false)

      XML.withSAXParser(spf.newSAXParser()).loadString(content)
    } match {
      case Success(elem) => Some(elem)
      case Failure(exception) =>
        logger.warn(s"Error creating XML secure parser: ${exception.getMessage}")
        None
    }
  }

  private val logger: Logger = LoggerFactory.getLogger(classOf[SecureXmlParsing.type])
}
