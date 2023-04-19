package io.joern.c2cpg.parser

import org.eclipse.cdt.core.index.IIndexFileLocation
import org.eclipse.cdt.internal.core.parser.IMacroDictionary
import org.eclipse.cdt.internal.core.parser.scanner.{InternalFileContent, InternalFileContentProvider}
import org.slf4j.LoggerFactory

import java.nio.file.Paths

class CustomFileContentProvider(headerFileFinder: HeaderFileFinder) extends InternalFileContentProvider {

  private val logger = LoggerFactory.getLogger(classOf[CustomFileContentProvider])

  private def loadContent(path: String): InternalFileContent = {
    val maybeFullPath = if (!getInclusionExists(path)) {
      headerFileFinder.find(path)
    } else {
      Option(path)
    }
    maybeFullPath
      .map { foundPath =>
        logger.debug(s"Loading header file '$foundPath'")
        CdtParser.readFileAsFileContent(Paths.get(foundPath)).asInstanceOf[InternalFileContent]
      }
      .getOrElse {
        logger.debug(s"Cannot find header file for '$path'")
        null
      }

  }

  override def getContentForInclusion(path: String, macroDictionary: IMacroDictionary): InternalFileContent =
    loadContent(path)

  override def getContentForInclusion(ifl: IIndexFileLocation, astPath: String): InternalFileContent =
    loadContent(astPath)

}
