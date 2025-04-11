package io.joern.c2cpg.parser

import io.joern.c2cpg.astcreation.CGlobal
import io.joern.c2cpg.parser.CdtParser.HeaderFileParserLanguage
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

class CustomFileContentProvider(headerFileFinder: HeaderFileFinder, sourceFile: String, global: CGlobal)
    extends InternalFileContentProvider {

  import CustomFileContentProvider.headerContentCache

  private val logger = LoggerFactory.getLogger(classOf[CustomFileContentProvider])

  override def getContentForInclusion(path: String, macroDictionary: IMacroDictionary): InternalFileContent = {
    loadContent(path)
  }

  override def getContentForInclusion(ifl: IIndexFileLocation, astPath: String): InternalFileContent = {
    loadContent(astPath)
  }

  private def loadContent(path: String): InternalFileContent = {
    val maybeFullPath = if (!getInclusionExists(path)) { headerFileFinder.find(path) }
    else { Option(path) }
    maybeFullPath.map { foundPath =>
      updateHeaderFileParserLanguage(foundPath)
      val content = contentFromCache(foundPath)
      FileContent.create(path, false, content).asInstanceOf[InternalFileContent]
    }.orNull
  }

  private def contentFromCache(foundPath: String): Array[Char] = {
    headerContentCache.computeIfAbsent(
      foundPath,
      _ => {
        logger.debug(s"Loading header file '$foundPath'")
        IOUtils.readLinesInFile(Paths.get(foundPath)).mkString("\n").toArray
      }
    )
  }

  /** Updates the parser language information for a header file in the global object.
    *
    * This method tracks which language parser (C, C++, or both) should be used for each header file based on where the
    * header is included from. The logic follows:
    *   - If first time seen: Use C parser for headers included from C files, C++ parser for others
    *   - If previously seen in C files only: Keep C parser or upgrade to both parsers if included from C++
    *   - If previously seen in C++ files only: Keep C++ parser or upgrade to both parsers if included from C
    *   - If already seen in both C and C++ files: Keep using both parsers
    *
    * @param foundPath
    *   The full path to the header file being processed
    */
  private def updateHeaderFileParserLanguage(foundPath: String): Unit = {
    if (FileDefaults.hasCHeaderFileExtension(foundPath)) {
      val currentLanguage = if (FileDefaults.hasCFileExtension(sourceFile)) { HeaderFileParserLanguage.C }
      else { HeaderFileParserLanguage.Cpp }
      global.headerIncludes.compute(
        foundPath,
        (_, previousLanguage) => {
          if (previousLanguage == null) { currentLanguage }
          else if (previousLanguage != currentLanguage) { HeaderFileParserLanguage.Both }
          else { previousLanguage }
        }
      )
    }
  }

}
