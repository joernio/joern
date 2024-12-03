package io.joern.c2cpg.parser

import org.eclipse.cdt.core.index.IIndexFileLocation
import org.eclipse.cdt.core.parser.FileContent
import org.eclipse.cdt.internal.core.parser.IMacroDictionary
import org.eclipse.cdt.internal.core.parser.scanner.InternalFileContent
import org.eclipse.cdt.internal.core.parser.scanner.InternalFileContentProvider
import org.slf4j.LoggerFactory

import java.util.concurrent.ConcurrentHashMap

class CustomFileContentProvider(headerFileFinder: HeaderFileFinder) extends InternalFileContentProvider {

  private val logger = LoggerFactory.getLogger(classOf[CustomFileContentProvider])
  private val headerCache: ConcurrentHashMap[String, InternalFileContent] = new ConcurrentHashMap()

  private def loadContent(path: String): InternalFileContent = {
    val maybeFullPath = if (!getInclusionExists(path)) { headerFileFinder.find(path) }
    else { Option(path) }
    maybeFullPath.map { foundPath =>
      headerCache.computeIfAbsent(
        foundPath,
        _ => {
          logger.debug(s"Loading header file '$foundPath'")
          FileContent.createForExternalFileLocation(foundPath).asInstanceOf[InternalFileContent]
        }
      )
    }.orNull
  }

  override def getContentForInclusion(path: String, macroDictionary: IMacroDictionary): InternalFileContent =
    loadContent(path)

  override def getContentForInclusion(ifl: IIndexFileLocation, astPath: String): InternalFileContent =
    loadContent(astPath)

}
