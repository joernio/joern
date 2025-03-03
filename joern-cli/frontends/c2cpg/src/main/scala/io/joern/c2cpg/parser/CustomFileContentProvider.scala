package io.joern.c2cpg.parser

import io.shiftleft.utils.IOUtils
import org.eclipse.cdt.core.index.IIndexFileLocation
import org.eclipse.cdt.core.parser.FileContent
import org.eclipse.cdt.internal.core.parser.IMacroDictionary
import org.eclipse.cdt.internal.core.parser.scanner.InternalFileContent
import org.eclipse.cdt.internal.core.parser.scanner.InternalFileContentProvider
import org.slf4j.LoggerFactory

import java.nio.file.Paths
import java.util.concurrent.ConcurrentHashMap

object CustomFileContentProvider {
  private val headerContentCache: ConcurrentHashMap[String, Array[Char]] = new ConcurrentHashMap()
}

class CustomFileContentProvider(headerFileFinder: HeaderFileFinder) extends InternalFileContentProvider {

  import CustomFileContentProvider.headerContentCache

  private val logger = LoggerFactory.getLogger(classOf[CustomFileContentProvider])

  override def getContentForInclusion(path: String, macroDictionary: IMacroDictionary): InternalFileContent = {
    loadContent(path)
  }

  private def loadContent(path: String): InternalFileContent = {
    val maybeFullPath = if (!getInclusionExists(path)) { headerFileFinder.find(path) }
    else { Option(path) }
    maybeFullPath.map { foundPath =>
      val path = Paths.get(foundPath)
      val content = headerContentCache.computeIfAbsent(
        foundPath,
        _ => {
          logger.debug(s"Loading header file '$foundPath'")
          IOUtils.readLinesInFile(path).mkString("\n").toArray
        }
      )
      FileContent.create(path.toString, false, content).asInstanceOf[InternalFileContent]
    }.orNull
  }

  override def getContentForInclusion(ifl: IIndexFileLocation, astPath: String): InternalFileContent = {
    loadContent(astPath)
  }

}
