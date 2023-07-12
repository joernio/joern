package io.joern.rubysrc2cpg.passes

import better.files.File
import io.joern.rubysrc2cpg.utils.{MethodTableModel, PackageTable}
import io.joern.x2cpg.passes.frontend.ImportsPass.*
import io.joern.x2cpg.passes.frontend.XImportResolverPass
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.PropertyNames
import io.shiftleft.codepropertygraph.generated.nodes.Call
import io.shiftleft.semanticcpg.language.*

import java.io.File as JFile
import java.util.regex.{Matcher, Pattern}
class ImportResolverPass(cpg: Cpg, packageTableInfo: PackageTable) extends XImportResolverPass(cpg) {

  private val pathPattern = Pattern.compile("[\"']([\\w/.]+)[\"']")

  override protected def optionalResolveImport(
    fileName: String,
    importCall: Call,
    importedEntity: String,
    importedAs: String,
    diffGraph: DiffGraphBuilder
  ): Unit = {

    resolveEntities(importedEntity, importCall, fileName).foreach(x => resolvedImportToTag(x, importCall, diffGraph))
  }

  private def resolveEntities(expEntity: String, importCall: Call, fileName: String): Set[ResolvedImport] = {

    val methodTableModelList = packageTableInfo.getPackageInfo(expEntity)

    // Below is a dummy sample for `sendgrid-ruby` gem, which can be used for development
    // val methodTableModelList = List(MethodTableModel("client", "SendGrid::API", "API"))

    // TODO
    /* Currently we are considering only case where exposed module are Classes,
    and the only way to consume them is by creating a new object as we encounter more cases,
     This needs to be handled accordingly
     */

    val relativeImportNodes = if (importCall.name.equals("require_relative")) {
      val rawEntity   = expEntity.stripPrefix("./")
      val matcher     = pathPattern.matcher(rawEntity)
      val sep         = Matcher.quoteReplacement(JFile.separator)
      val root        = s"$codeRoot${JFile.separator}"
      val currentFile = s"$root$fileName"
      val entity      = if (matcher.find()) matcher.group(1) else rawEntity
      val resolvedPath = better.files
        .File(
          currentFile.stripSuffix(currentFile.split(sep).lastOption.getOrElse("")),
          entity.split("\\.").headOption.getOrElse(entity)
        )
        .pathAsString match {
        case resPath if entity.endsWith(".rb") => s"$resPath.rb"
        case resPath                           => resPath
      }

      val resolvedTypeDecls = cpg.typeDecl
        .where(_.file.name(s"${Pattern.quote(resolvedPath)}\\.?.*"))
        .fullName
        .flatMap(fullName => Seq(ResolvedTypeDecl(fullName), ResolvedMethod(s"$fullName.new", "new")))
        .toList
      val resolvedFunctions = cpg.method
        .where(_.file.name(s"${Pattern.quote(resolvedPath)}\\.?.*"))
        .whereNot(_.nameExact(":program"))
        .map(method => ResolvedMethod(method.fullName, method.name))
        .toList
      (resolvedTypeDecls ++ resolvedFunctions).distinct
    } else Seq.empty

    val importNodes = methodTableModelList.flatMap { methodTableModel =>
      Seq(ResolvedMethod(s"$expEntity::program.${methodTableModel.parentClassPath.stripSuffix(":")}.new", "new"))
    }.distinct
    (relativeImportNodes ++ importNodes).toSet
  }

}
