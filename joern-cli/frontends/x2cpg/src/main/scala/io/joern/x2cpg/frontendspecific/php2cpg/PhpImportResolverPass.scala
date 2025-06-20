package io.joern.x2cpg.frontendspecific.php2cpg

import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Import
import io.shiftleft.passes.ForkJoinParallelCpgPass
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.importresolver.{
  EvaluatedImport,
  ResolvedImport,
  ResolvedTypeDecl,
  UnknownImport,
  UnknownTypeDecl
}
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.mutable

/** PHP imports are generally quite explicit when considering `use` imports. This is less so, however, when using the
  * older 'require' syntax, but we will ignore that here.
  *
  * Main code constructs that can be imported are namespaces, classes, and constants.
  *
  * @see
  *   <a href="https://www.php.net/manual/en/language.namespaces.importing.php">PHP Importing</a>
  */
class PhpImportResolverPass(cpg: Cpg) extends ForkJoinParallelCpgPass[Import](cpg) {

  protected val logger: Logger     = LoggerFactory.getLogger(this.getClass)
  private val namespacesAndClasses = mutable.Set.empty[String]

  override def init(): Unit = {
    namespacesAndClasses.addAll(cpg.namespaceBlock.fullName)
    namespacesAndClasses.addAll(cpg.typeDecl.fullName)
  }

  override def generateParts(): Array[Import] = cpg.imports.toArray

  override def runOnPart(diffGraph: DiffGraphBuilder, part: Import): Unit = {
    for {
      importedEntity <- part.importedEntity
    } {
      val imp = resolveImport(importedEntity)
      part.start.newTagNodePair(imp.label, imp.serialize).store()(diffGraph)
    }
  }

  // TODO: Everything treated as a type decl for now
  private def resolveImport(entity: String): EvaluatedImport = {
    if (namespacesAndClasses.contains(entity)) {
      ResolvedTypeDecl(entity)
    } else {
      UnknownTypeDecl(entity)
    }
  }

}
