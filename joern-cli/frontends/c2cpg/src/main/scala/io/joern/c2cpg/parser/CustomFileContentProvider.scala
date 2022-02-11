package io.joern.c2cpg.parser

import org.eclipse.cdt.core.index.IIndexFileLocation
import org.eclipse.cdt.core.parser.FileContent
import org.eclipse.cdt.internal.core.parser.IMacroDictionary
import org.eclipse.cdt.internal.core.parser.scanner.{InternalFileContent, InternalFileContentProvider}
import org.slf4j.LoggerFactory

class CustomFileContentProvider(headerFileFinder: HeaderFileFinder) extends InternalFileContentProvider {

  private val logger = LoggerFactory.getLogger(classOf[CustomFileContentProvider])

  private def loadContent(path: String): InternalFileContent = {
    val maybeFileName = if (!getInclusionExists(path)) {
      headerFileFinder.find(path)
    } else {
      Some(path)
    }
    maybeFileName
      .map { fileName =>
        logger.debug(s"Loading header file '$fileName'")
        val content = FileContent.createForExternalFileLocation(fileName)
        content.asInstanceOf[InternalFileContent]
      }
      .getOrElse {
        logger.debug(s"Cannot find header file for '$path'")
        null
      }
  }

  override def getContentForInclusion(path: String, macroDictionary: IMacroDictionary): InternalFileContent =
    loadContent(path)

  override def getContentForInclusion(ifl: IIndexFileLocation, astPath: String): InternalFileContent = loadContent(
    astPath
  )

}
