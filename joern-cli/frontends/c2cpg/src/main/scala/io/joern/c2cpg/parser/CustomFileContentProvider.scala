package io.joern.c2cpg.parser

import io.joern.c2cpg.parser.CdtParser.HeaderFileParserLanguage
import io.joern.c2cpg.passes.AstCreationPass
import io.shiftleft.utils.IOUtils
import org.eclipse.cdt.core.index.IIndexFileLocation
import org.eclipse.cdt.core.parser.FileContent
import org.eclipse.cdt.internal.core.parser.IMacroDictionary
import org.eclipse.cdt.internal.core.parser.scanner.InternalFileContent
import org.eclipse.cdt.internal.core.parser.scanner.InternalFileContentProvider

import java.nio.file.Paths
import java.util.concurrent.ConcurrentHashMap

class CustomFileContentProvider(
  headerFileFinder: HeaderFileFinder,
  sourceFile: String,
  accumulator: AstCreationPass.Accumulator,
  contentCache: ConcurrentHashMap[String, Array[Char]] = new ConcurrentHashMap()
) extends InternalFileContentProvider {

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
      val content =
        contentCache.computeIfAbsent(
          foundPath,
          filePath => IOUtils.readLinesInFile(Paths.get(filePath)).mkString("\n").toArray
        )
      FileContent.create(path, false, content).asInstanceOf[InternalFileContent]
    }.orNull
  }

  private def updateHeaderFileParserLanguage(foundPath: String): Unit = {
    if (FileDefaults.hasCHeaderFileExtension(foundPath)) {
      val currentLanguage = if (FileDefaults.hasCFileExtension(sourceFile)) { HeaderFileParserLanguage.C }
      else { HeaderFileParserLanguage.Cpp }
      accumulator.updateHeaderFileParserLanguage(foundPath, currentLanguage)
    }
  }

}
