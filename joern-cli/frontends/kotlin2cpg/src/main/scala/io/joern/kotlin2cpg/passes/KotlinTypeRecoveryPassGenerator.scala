package io.joern.kotlin2cpg.passes

import io.joern.kotlin2cpg.Constants
import io.joern.x2cpg.Defines
import io.joern.x2cpg.passes.frontend.*
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.PropertyNames
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*
import overflowdb.BatchedUpdate.DiffGraphBuilder

class KotlinTypeRecoveryPassGenerator(cpg: Cpg, config: XTypeRecoveryConfig = XTypeRecoveryConfig())
    extends XTypeRecoveryPassGenerator[File](cpg, config) {
  override protected def generateRecoveryPass(state: XTypeRecoveryState, iteration: Int): XTypeRecovery[File] =
    new KotlinTypeRecovery(cpg, state, iteration)
}

private class KotlinTypeRecovery(cpg: Cpg, state: XTypeRecoveryState, iteration: Int)
    extends XTypeRecovery[File](cpg, state, iteration) {

  override def compilationUnits: Iterator[File] = cpg.file.iterator

  override def generateRecoveryForCompilationUnitTask(
    unit: File,
    builder: DiffGraphBuilder
  ): RecoverForXCompilationUnit[File] = {
    new RecoverForKotlinFile(cpg, unit, builder, state)
  }
}

private class RecoverForKotlinFile(cpg: Cpg, cu: File, builder: DiffGraphBuilder, state: XTypeRecoveryState)
    extends RecoverForXCompilationUnit[File](cpg, cu, builder, state) {

  override protected def fromNodeToLocalKey(n: AstNode): Option[LocalKey] = n match {
    case i: Identifier if i.name == "this" && i.code == "super" => Option(LocalVar("super"))
    case _                                                      => SBKey.fromNodeToLocalKey(n)
  }

  override protected def importNodes: Iterator[Import] = cu.ast.isImport
  override protected def visitImport(i: Import): Unit = {

    val alias    = i.importedAs.getOrElse("")
    val fullName = i.importedEntity.getOrElse("")
    if (alias != Constants.wildcardImportName) {
      symbolTable.append(CallAlias(alias), fullName)
      symbolTable.append(LocalVar(alias), fullName)
    }
  }

  override protected def isConstructor(c: Call): Boolean = isConstructor(c.name)

  override protected def isConstructor(name: String): Boolean = !name.isBlank && name.charAt(0).isUpper

  override protected def postVisitImports(): Unit = {
    for ((k, ts) <- symbolTable.itemsCopy) {
      val tss = ts.filterNot(_.startsWith(Defines.UnresolvedNamespace))
      if (tss.isEmpty)
        symbolTable.remove(k)
      else
        symbolTable.put(k, tss)
    }
  }

  // There seems to be issues with inferring these, often due to situations where super and this are confused on name
  // and code properties.
  override protected def storeIdentifierTypeInfo(i: Identifier, types: Seq[String]): Unit = if (i.name != "this") {
    super.storeIdentifierTypeInfo(i, types)
  }

  override protected def storeCallTypeInfo(c: Call, types: Seq[String]): Unit =
    if (types.nonEmpty) {
      val signedTypes = types.map {
        case t if t.endsWith(c.signature) => t
        case t                            => s"$t:${c.signature}"
      }
      builder.setNodeProperty(c, PropertyNames.DYNAMIC_TYPE_HINT_FULL_NAME, signedTypes)
    }

}
