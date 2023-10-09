package io.joern.javasrc2cpg.passes

import io.joern.x2cpg.Defines
import io.joern.x2cpg.passes.frontend.*
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.PropertyNames
import io.shiftleft.codepropertygraph.generated.nodes.*
import overflowdb.BatchedUpdate.DiffGraphBuilder

import java.util.concurrent.ExecutorService

class JavaTypeRecoveryPass(cpg: Cpg, config: TypeRecoveryConfig = TypeRecoveryConfig())
    extends XTypeRecoveryPass(cpg, config) {
  override protected def generateRecoveryPass(state: State, executor: ExecutorService): XTypeRecovery =
    new JavaTypeRecovery(cpg, state, executor)
}

private class JavaTypeRecovery(cpg: Cpg, state: State, executor: ExecutorService)
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
    state: State
  ): RecoverTypesForProcedure =
    RecoverForJavaFile(cpg, procedure, initialSymbolTable, builder, state)

}

private class RecoverForJavaFile(
  cpg: Cpg,
  procedure: Method,
  symbolTable: SymbolTable[LocalKey],
  builder: DiffGraphBuilder,
  state: State
) extends RecoverTypesForProcedure(cpg, procedure, symbolTable, builder, state) {

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
