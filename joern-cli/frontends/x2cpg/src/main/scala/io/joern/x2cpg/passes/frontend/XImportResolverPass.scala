package io.joern.x2cpg.passes.frontend

import better.files.File
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Import, Tag}
import io.shiftleft.passes.ForkJoinParallelCpgPass
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.importresolver.EvaluatedImport
import org.slf4j.{Logger, LoggerFactory}

import java.io.File as JFile
import java.nio.charset.StandardCharsets
import java.util.Base64

abstract class XImportResolverPass(cpg: Cpg) extends ForkJoinParallelCpgPass[Import](cpg) {

  protected val logger: Logger = LoggerFactory.getLogger(this.getClass)
  protected val codeRootDir: String = File(
    cpg.metaData.root.headOption.getOrElse(JFile.separator).stripSuffix(JFile.separator)
  ) match
    case f if f.isDirectory => f.pathAsString
    case f                  => f.parent.pathAsString

  override def generateParts(): Array[Import] = cpg.imports.toArray

  override def runOnPart(builder: DiffGraphBuilder, part: Import): Unit = for {
    call <- part.call
    fileName = call.file.name.headOption.getOrElse("<unknown>").stripPrefix(codeRootDir)
    importedAs     <- part.importedAs
    importedEntity <- part.importedEntity
  } {
    optionalResolveImport(fileName, call, importedEntity, importedAs, builder)
  }

  protected def optionalResolveImport(
    fileName: String,
    importCall: Call,
    importedEntity: String,
    importedAs: String,
    diffGraph: DiffGraphBuilder
  ): Unit

  protected def evaluatedImportToTag(x: EvaluatedImport, importCall: Call, diffGraph: DiffGraphBuilder): Unit =
    importCall.start.newTagNodePair(x.label, x.serialize).store()(diffGraph)

}
