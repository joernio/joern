package io.joern.c2cpg.parser

import io.joern.c2cpg.parser.CustomFileContentProvider.missingHeaderFiles
import io.shiftleft.utils.IOUtils
import org.eclipse.cdt.core.index.IIndexFileLocation
import org.eclipse.cdt.internal.core.parser.IMacroDictionary
import org.eclipse.cdt.internal.core.parser.scanner.{InternalFileContent, InternalFileContentProvider}
import org.slf4j.LoggerFactory

import java.nio.file.Paths
import java.util.concurrent.ConcurrentHashMap

object CustomFileContentProvider {
  private val headerFileToLines: ConcurrentHashMap[String, Array[Char]] = new ConcurrentHashMap()
  private val missingHeaderFiles: ConcurrentHashMap[String, Boolean]    = new ConcurrentHashMap()
}

class CustomFileContentProvider(headerFileFinder: HeaderFileFinder) extends InternalFileContentProvider {

  import io.joern.c2cpg.parser.CustomFileContentProvider.headerFileToLines

  private val logger = LoggerFactory.getLogger(classOf[CustomFileContentProvider])

  private def loadContent(path: String): InternalFileContent = {
    val maybeFullPath = if (!getInclusionExists(path)) {
      headerFileFinder.find(path)
    } else {
      Option(path)
    }
    maybeFullPath
      .map { foundPath =>
        val p = Paths.get(foundPath)
        val content = headerFileToLines.computeIfAbsent(
          foundPath,
          _ => {
            logger.debug(s"Loading header file '$foundPath'")
            IOUtils.readLinesInFile(p).mkString("\n").toArray
          }
        )
        CdtParser.loadLinesAsFileContent(p, content)
      }
      .getOrElse {
        missingHeaderFiles.computeIfAbsent(
          path,
          _ => {
            logger.debug(s"Cannot find header file for '$path'")
            true
          }
        )
        null
      }

  }

  override def getContentForInclusion(path: String, macroDictionary: IMacroDictionary): InternalFileContent =
    loadContent(path)

  override def getContentForInclusion(ifl: IIndexFileLocation, astPath: String): InternalFileContent =
    loadContent(astPath)

}
