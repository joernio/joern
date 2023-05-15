package io.joern.javasrc2cpg.passes

import io.joern.x2cpg.Defines
import io.joern.x2cpg.passes.frontend._
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.PropertyNames
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._
import overflowdb.BatchedUpdate.DiffGraphBuilder
import overflowdb.traversal.Traversal

class JavaTypeRecoveryPass(cpg: Cpg, config: XTypeRecoveryConfig = XTypeRecoveryConfig())
    extends XTypeRecoveryPass[Method](cpg, config) {
  override protected def generateRecoveryPass(state: XTypeRecoveryState): XTypeRecovery[Method] =
    new JavaTypeRecovery(cpg, state)
}

private class JavaTypeRecovery(cpg: Cpg, state: XTypeRecoveryState) extends XTypeRecovery[Method](cpg, state) {

  override def compilationUnit: Traversal[Method] = cpg.method.isExternal(false)

  override def generateRecoveryForCompilationUnitTask(
    unit: Method,
    builder: DiffGraphBuilder
  ): RecoverForXCompilationUnit[Method] = new RecoverForJavaFile(
    cpg,
    unit,
    builder,
    state.copy(config = state.config.copy(enabledDummyTypes = state.isFinalIteration && state.config.enabledDummyTypes))
  )
}

private class RecoverForJavaFile(cpg: Cpg, cu: Method, builder: DiffGraphBuilder, state: XTypeRecoveryState)
    extends RecoverForXCompilationUnit[Method](cpg, cu, builder, state) {

  private def javaNodeToLocalKey(n: AstNode): Option[LocalKey] = n match {
    case i: Identifier if i.name == "this" && i.code == "super" => Option(LocalVar("super"))
    case _                                                      => SBKey.fromNodeToLocalKey(n)
  }

  override protected val symbolTable = new SymbolTable[LocalKey](javaNodeToLocalKey)

  override protected def isConstructor(c: Call): Boolean = isConstructor(c.name)

  override protected def isConstructor(name: String): Boolean = !name.isBlank && name.charAt(0).isUpper

  override protected def postVisitImports(): Unit = {
    symbolTable.view.foreach { case (k, ts) =>
      val tss = ts.filterNot(_.startsWith(Defines.UnresolvedNamespace))
      if (tss.isEmpty)
        symbolTable.remove(k)
      else
        symbolTable.put(k, tss)
    }
  }

  override protected def nodeExistingTypes(storedNode: StoredNode): Seq[String] =
    super.nodeExistingTypes(storedNode).filterNot(_.startsWith(Defines.UnresolvedNamespace))

  // There seems to be issues with inferring these, often due to situations where super and this are confused on name
  // and code properties.
  override protected def storeIdentifierTypeInfo(i: Identifier, types: Seq[String]): Unit = if (i.name != "this") {
    super.storeIdentifierTypeInfo(i, types)
  }

  override protected def storeCallTypeInfo(c: Call, types: Seq[String]): Unit =
    if (types.nonEmpty) {
      state.changesWereMade.compareAndSet(false, true)
      val signedTypes = types.map {
        case t if t.endsWith(c.signature) => t
        case t                            => s"$t:${c.signature}"
      }
      builder.setNodeProperty(c, PropertyNames.DYNAMIC_TYPE_HINT_FULL_NAME, signedTypes)
    }

}
