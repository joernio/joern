package io.joern.rubysrc2cpg.passes

import io.joern.x2cpg.passes.frontend.XImportResolverPass
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.importresolver.*

import java.io.File as JFile
import better.files.File
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal

class RubyImportResolverPass(cpg: Cpg) extends XImportResolverPass(cpg) {
  private var pathCache: Map[String, List[EvaluatedImport]] = Map.empty
  override def init(): Unit = {
      pathCache = 
        cpg.file.map { file =>
          val importName = fileToRubyImport(file.name)
          def nsChildren = file.namespaceBlock.name(NamespaceTraversal.globalNamespaceName).astChildren
          def resolvedImports = nsChildren.flatMap {
            case typ: TypeDecl if !typ.isExternal => List(
              ResolvedTypeDecl(typ.fullName)
            )
            case method: Method if !method.isExternal => List(
              ResolvedMethod(method.fullName, method.fullName)
            )
            // PR comment to remove: Potentially add top level members if/when they are implemented
            // e.g.
            // class << self
            //   attr_accessor :x
            // end
            // self.x = 1
            case _ => Nil
          }
          (importName, resolvedImports.toList)
        }.toMap // PR comment to remove: Doesn't deal with multiple same named files, is that okay?
  }

  override protected def optionalResolveImport(
    fileName: String, 
    importCall: Call,
    importedEntity: String,
    importedAs: String,
    diffGraph: DiffGraphBuilder): Unit = {
    val currDir = File(codeRootDir) / fileName match
      case x if x.isDirectory => x
      case x                  => x.parent
    // PR comment I should add require_relative to the ImportsPass and use use the `importCall` to make the distinction. For now I'll do what PythonImportResolver pass does and get both
    val importedEntityAsRelativeImport = Seq(
      currDir.pathAsString.stripPrefix(codeRootDir).stripPrefix(JFile.separator),
      importedEntity
    ).filterNot(_.isBlank).mkString(JFile.separator)
  }

  private def fileToRubyImport(filename: String): String =
    filename
      .stripSuffix(".py")

}
