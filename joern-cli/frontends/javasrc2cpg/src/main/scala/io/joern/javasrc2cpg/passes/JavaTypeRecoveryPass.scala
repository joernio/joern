package io.joern.javasrc2cpg.passes

import io.joern.x2cpg.Defines
import io.joern.x2cpg.passes.frontend.*
import io.joern.x2cpg.passes.frontend.ImportsPass.ResolvedImport
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.PropertyNames
import io.shiftleft.codepropertygraph.generated.nodes.*
import overflowdb.BatchedUpdate.DiffGraphBuilder
import overflowdb.traversal.ImplicitsTmp.toTraversalSugarExt

import java.util.concurrent.ExecutorService

class JavaTypeRecoveryPass(cpg: Cpg, config: TypeRecoveryConfig = TypeRecoveryConfig())
    extends XTypeRecoveryPass(cpg, config) {
  override protected def generateRecoveryPass(state: TypeRecoveryState, executor: ExecutorService): XTypeRecovery =
    new JavaTypeRecovery(cpg, state, executor)
}

private class JavaTypeRecovery(cpg: Cpg, state: TypeRecoveryState, executor: ExecutorService)
    extends XTypeRecovery(cpg, state, executor) {

  override protected val initialSymbolTable = new SymbolTable[LocalKey](javaNodeToLocalKey)

  private def javaNodeToLocalKey(n: AstNode): Option[LocalKey] = n match {
    case i: Identifier if i.name == "this" && i.code == "super" => Option(LocalVar("super"))
    case _                                                      => SBKey.fromNodeToLocalKey(n)
  }

  override protected def recoverTypesForProcedure(
    cpg: Cpg,
    procedure: Method,
    initialSymbolTable: SymbolTable[LocalKey],
    builder: DiffGraphBuilder,
    state: TypeRecoveryState
  ): RecoverTypesForProcedure =
    RecoverForJavaFile(cpg, procedure, initialSymbolTable, builder, state)

  override protected def importNodes(cu: File): List[ResolvedImport] =
    cu.namespaceBlock.flatMap(_.astOut).collectAll[Import].flatMap(visitImport).l

  // Java has a much simpler import structure that doesn't need resolution
  override protected def visitImport(i: Import): Iterator[ImportsPass.ResolvedImport] = {
    for {
      alias    <- i.importedAs
      fullName <- i.importedEntity
    } {
      if (alias != "*") {
        initialSymbolTable.append(CallAlias(alias, Option("this")), fullName)
        initialSymbolTable.append(LocalVar(alias), fullName)
      }
    }
    Iterator.empty
  }

  override protected def postVisitImports(): Unit = {
    initialSymbolTable.view.foreach { case (k, ts) =>
      val tss = ts.filterNot(_.startsWith(Defines.UnresolvedNamespace))
      if (tss.isEmpty)
        initialSymbolTable.remove(k)
      else
        initialSymbolTable.put(k, tss)
    }
  }

}

private class RecoverForJavaFile(
  cpg: Cpg,
  procedure: Method,
  symbolTable: SymbolTable[LocalKey],
  builder: DiffGraphBuilder,
  state: TypeRecoveryState
) extends RecoverTypesForProcedure(cpg, procedure, symbolTable, builder, state) {

  override protected def isConstructor(c: Call): Boolean = isConstructor(c.name)

  override protected def isConstructor(name: String): Boolean = !name.isBlank && name.charAt(0).isUpper

  // There seems to be issues with inferring these, often due to situations where super and this are confused on name
  // and code properties.
  override protected def storeIdentifierTypeInfo(i: Identifier, types: Seq[String]): Unit = if (i.name != "this") {
    super.storeIdentifierTypeInfo(i, types.filterNot(_ == "null"))
  }

  override protected def storeCallTypeInfo(c: Call, types: Seq[String]): Unit =
    if (types.nonEmpty) {
      state.changesWereMade.compareAndSet(false, true)
      val signedTypes = types.map {
        case t if t.endsWith(c.signature) => t
        case t                            => s"$t:${c.signature}"
      }
      if (c.possibleTypes != signedTypes) {
        builder.setNodeProperty(c, PropertyNames.POSSIBLE_TYPES, signedTypes)
      }
    }

}
