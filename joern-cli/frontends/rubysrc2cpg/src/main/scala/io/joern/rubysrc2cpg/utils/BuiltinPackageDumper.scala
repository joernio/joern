package io.joern.rubysrc2cpg.utils

import io.joern.rubysrc2cpg.datastructures.{RubyMethod, RubyType}
import io.joern.x2cpg.Defines
import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import net.ruippeixotog.scalascraper.dsl.DSL.*
import net.ruippeixotog.scalascraper.dsl.DSL.Extract.*
import net.ruippeixotog.scalascraper.model.Element
import better.files.File
import io.joern.x2cpg.utils.ConcurrentTaskUtil
import org.slf4j.{Logger, LoggerFactory}

import scala.util.{Failure, Success}

/** Class to generate MessagePack representation for all builtin Ruby libraries.
  * @param rubyVersion
  *   \- Ruby version installed
  */
class BuiltinPackageDumper(rubyVersion: String = "3.3.0") {
  private val logger: Logger = LoggerFactory.getLogger(this.getClass)

  private val CLASS    = "class"
  private val INSTANCE = "instance"

  private val browser = JsoupBrowser()
  private val baseUrl = s"https://ruby-doc.org/$rubyVersion"

  private val baseDir = "src/main/resources/builtin_types"

  // Below unicode value caluclated with: println("\\u" + Integer.toHexString('→' | 0x10000).substring(1))
  // taken from: https://stackoverflow.com/questions/2220366/get-unicode-value-of-a-character
  private val arrowUnicodeValue = "\\u2192"

  def run(): Unit = {
    val builtinDir = File(baseDir)
    builtinDir.createDirectoryIfNotExists()

    val paths = generatePaths()

    val types = ConcurrentTaskUtil
      .runUsingThreadPool(generateRubyTypes(paths))
      .flatMap {
        case Success(rubyTypes) =>
          rubyTypes.foreach { rubyType => writeToFile(rubyType) }
          rubyTypes
        case Failure(ex) =>
          logger.warn(s"Failed to scrape/write Ruby builtin types: $ex")
          None
      }
  }

  private def generateRubyTypes(pathsMap: collection.mutable.Map[String, List[String]]): Iterator[() => List[RubyType]] = {
    pathsMap
      .map((baseModuleName, paths) =>
        () => {
          paths.map { path =>
            val doc = browser.get(path)

            val namespace =
              doc >?> element("h1.class, h1.module") match {
                case Some(classOrModuleElement) =>
                  // Text on website is: Class/Module <some>::<module/class>::<name>
                  val classOrModuleName = classOrModuleElement.text.split("\\s")(1).replaceAll("::", "\\.").strip
                  s"$baseModuleName.$classOrModuleName"
                case None => baseModuleName
              }

            val rubyMethods = buildRubyMethods(doc, namespace)

            RubyType(namespace, rubyMethods, List.empty)
          }
        }
      )
      .iterator
  }

  private def writeToFile(rubyType: RubyType): Unit = {
    val rubyTypeNameSegments = rubyType.name.split("\\.")

    val (directorySuffixes, fileName) = rubyTypeNameSegments.size match {
      case x if x == 1 =>
        ("", rubyTypeNameSegments(x - 1))
      case x if x > 1 =>
        (rubyTypeNameSegments.take(x - 1).mkString("."), rubyTypeNameSegments(x - 1))
    }

    val dir = File(s"$baseDir/$directorySuffixes")
    dir.createDirectoryIfNotExists(createParents = true)

    val typeFile = File(s"${dir.pathAsString}/$fileName.mpk")
    typeFile.createIfNotExists()

    val msg: upack.Msg = upickle.default.writeMsg(rubyType)
    typeFile.writeByteArray(upack.writeToByteArray(msg))
  }

  private def buildRubyMethods(doc: browser.DocumentType, namespace: String): List[RubyMethod] = {
    def generateMethodHeadingsSelector(methodType: String): String = {
      s"#public-$methodType-5Buntitled-5D-method-details > .method-detail > .method-heading"
    }

    val methodHeadings =
      doc >> elementList(s"${generateMethodHeadingsSelector(CLASS)}, ${generateMethodHeadingsSelector(INSTANCE)}")

    val methodElements = methodHeadings >> element(".method-callseq, .method-name")

    val funcNameRegex = "^([^{(]+)".r

    methodElements
      .map { x =>
        val method = x.text.split(arrowUnicodeValue)(0)

        funcNameRegex.findFirstMatchIn(method) match {
          case Some(methodName) =>
            // Some methods are `methodName == something`, which is why the split on space here is required
            s"${methodName.toString.replaceAll("[!?=]", "").split("\\s+")(0).strip}"
          case None             => ""
        }
      }
      .filterNot(_ == "")
      .distinct
      .map(x => RubyMethod(x, List.empty, Defines.Any, Option(namespace)))
  }

  private def generatePaths(): collection.mutable.Map[String, List[String]] = {
    val doc = browser.get(baseUrl)

    val liElements = doc >> elementList("#classindex-section > .link-list > li")

    val linksMap = collection.mutable.Map[String, List[String]]()

    val baseItems = liElements.takeWhile { x =>
      !x.hasAttr("class") || !(x.attr("class") == "gemheader")
    }

    val (_, restOfItems) = liElements.splitAt(baseItems.size + 1)

    val links = (restOfItems >> elementList("a")).filter(_.nonEmpty).map(_.head).groupBy(_.attr("href").split("/")(2))

    val baseLinks = baseItems.map { x =>
      val anchor = x >?> element("a")
      s"$baseUrl/${anchor.get.attr("href").replaceAll("\\./", "")}"
    }

    linksMap.addOne("__builtin", baseLinks)

    links.foreach { (extensionName, anchorElements) =>
      val anchorHrefs = anchorElements
        .map { anchorElement =>
          s"$baseUrl/${anchorElement.attr("href").replaceAll("\\./", "")}"
        }
        .filter(!_.contains("table_of_contents"))

      linksMap.addOne(extensionName, anchorHrefs)
    }

    linksMap
  }
}
