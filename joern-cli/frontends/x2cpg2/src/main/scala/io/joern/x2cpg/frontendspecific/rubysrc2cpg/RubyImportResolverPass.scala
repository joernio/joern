package io.joern.x2cpg.frontendspecific.rubysrc2cpg

import io.joern.x2cpg.frontendspecific.rubysrc2cpg.Constants.*
import io.joern.x2cpg.passes.frontend.XImportResolverPass
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Call
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.importresolver.*
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal

import java.io.File as JFile
import java.nio.file.Paths
import java.util.regex.{Matcher, Pattern}
class RubyImportResolverPass(cpg: Cpg) extends XImportResolverPass(cpg) {

  private val pathPattern = Pattern.compile("[\"']([\\w/.]+)[\"']")

  override protected def optionalResolveImport(
    fileName: String,
    importCall: Call,
    importedEntity: String,
    importedAs: String,
    diffGraph: DiffGraphBuilder
  ): Unit = {

    resolveEntities(importedEntity, importCall, fileName).foreach(x => evaluatedImportToTag(x, importCall, diffGraph))
  }

  private def resolveEntities(expEntity: String, importCall: Call, fileName: String): Set[EvaluatedImport] = {
    val expResolvedPath =
      if (expEntity.contains("."))
        getResolvedPath(expEntity, fileName)
      else if (cpg.file.name(s".*$expEntity.rb").nonEmpty)
        getResolvedPath(s"$expEntity.rb", fileName)
      else
        expEntity

    // TODO Limited ResolvedMethod exposure for now, will open up after looking at more concrete examples
    val finalResolved = {
      val filePattern = s"${Pattern.quote(expResolvedPath)}\\.?.*"
      val resolvedTypeDecls = cpg.typeDecl
        .where(_.file.name(filePattern))
        .whereNot(_.isModule)
        .fullName
        .flatMap(fullName =>
          Seq(
            ResolvedTypeDecl(fullName),
            ResolvedMethod(s"$fullName.${Initialize}", "new", fullName.split("[.]").lastOption)
          )
        )
        .toSet

      val resolvedModules = cpg.namespaceBlock
        .whereNot(_.nameExact(NamespaceTraversal.globalNamespaceName))
        .where(_.file.name(filePattern))
        .flatMap(module => Seq(ResolvedTypeDecl(module.fullName)))
        .toSet

      // Expose methods which are directly present in a file, without any module, TypeDecl
      val resolvedMethods = cpg.method
        .where(_.file.name(filePattern))
        .where(_.nameExact(Main))
        .astChildren
        .astChildren
        .isMethod
        .flatMap(method => Seq(ResolvedMethod(method.fullName, method.name)))
        .toSet
      resolvedTypeDecls ++ resolvedModules ++ resolvedMethods
    }.collectAll[EvaluatedImport].toSet

    finalResolved
  }

  def getResolvedPath(expEntity: String, fileName: String) = {
    val rawEntity   = expEntity.stripPrefix("./")
    val matcher     = pathPattern.matcher(rawEntity)
    val sep         = Matcher.quoteReplacement(JFile.separator)
    val root        = s"$codeRootDir${JFile.separator}"
    val currentFile = s"$root$fileName"
    val entity      = if (matcher.find()) matcher.group(1) else rawEntity
    val resolvedPath = Paths
      .get(
        currentFile.stripSuffix(currentFile.split(sep).lastOption.getOrElse("")),
        entity.split("\\.").headOption.getOrElse(entity)
      )
      .toString match {
      case resPath if entity.endsWith(".rb") => s"$resPath.rb"
      case resPath                           => resPath
    }
    resolvedPath.stripPrefix(root)
  }

}
