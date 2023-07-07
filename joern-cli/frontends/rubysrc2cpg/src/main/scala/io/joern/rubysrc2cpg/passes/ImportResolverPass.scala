package io.joern.rubysrc2cpg.passes

import io.joern.rubysrc2cpg.utils.{MethodTableModel, PackageTable}
import io.joern.x2cpg.passes.frontend.ImportsPass.*
import io.joern.x2cpg.passes.frontend.XImportResolverPass
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.PropertyNames
import io.shiftleft.codepropertygraph.generated.nodes.Call
import io.shiftleft.semanticcpg.language._

import java.io.File as JFile
class ImportResolverPass(cpg: Cpg, packageTableInfo: PackageTable) extends XImportResolverPass(cpg) {

  override protected def optionalResolveImport(
                                                fileName: String,
                                                importCall: Call,
                                                importedEntity: String,
                                                importedAs: String,
                                                diffGraph: DiffGraphBuilder
                                              ): Unit = {

    resolveEntities(importedEntity).foreach(x => resolvedImportToTag(x, importCall, diffGraph))
  }

  private def resolveEntities(expEntity: String): Set[ResolvedImport] = {


    val methodTableModelList = packageTableInfo.getPackageInfo(expEntity)


    // Below is a dummy sample for `sendgrid-ruby` gem, which can be used for development
   //val methodTableModelList = List(MethodTableModel("client", "SendGrid::API", "API"))

    // TODO
    /* Currently we are considering only case where exposed module are Classes,
    and the only way to consume them is by creating a new object as we encounter more cases,
     This needs to be handled accordingly
    */


    methodTableModelList.flatMap{
      methodTableModel =>
        Seq(ResolvedMethod(s"$expEntity::program:${methodTableModel.parentClassPath.stripSuffix(":")}.new", "new"))
    }.toSet
  }


}
