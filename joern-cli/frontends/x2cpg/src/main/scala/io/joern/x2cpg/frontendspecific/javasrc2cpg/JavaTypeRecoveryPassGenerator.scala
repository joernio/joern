package io.joern.x2cpg.frontendspecific.javasrc2cpg

import io.joern.x2cpg.Defines
import io.joern.x2cpg.passes.frontend.*
import io.shiftleft.codepropertygraph.generated.{Cpg, PropertyNames}
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*
import overflowdb.BatchedUpdate.DiffGraphBuilder

class JavaTypeRecoveryPassGenerator(cpg: Cpg, config: XTypeRecoveryConfig = XTypeRecoveryConfig())
    extends XTypeRecoveryPassGenerator[Method](cpg, config) {
  override protected def generateRecoveryPass(state: XTypeRecoveryState, iteration: Int): XTypeRecovery[Method] =
    new JavaTypeRecovery(cpg, state, iteration)
}

private class JavaTypeRecovery(cpg: Cpg, state: XTypeRecoveryState, iteration: Int)
    extends XTypeRecovery[Method](cpg, state, iteration: Int) {

  override def compilationUnits: Iterator[Method] = cpg.method.isExternal(false).iterator

  override def generateRecoveryForCompilationUnitTask(
    unit: Method,
    builder: DiffGraphBuilder
  ): RecoverForXCompilationUnit[Method] = {
    new RecoverForJavaFile(cpg, unit, builder, state)
  }
}

private class RecoverForJavaFile(cpg: Cpg, cu: Method, builder: DiffGraphBuilder, state: XTypeRecoveryState)
    extends RecoverForXCompilationUnit[Method](cpg, cu, builder, state) {

  override protected def fromNodeToLocalKey(n: AstNode): Option[LocalKey] = n match {
    case i: Identifier if i.name == "this" && i.code == "super" => Option(LocalVar("super"))
    case _                                                      => SBKey.fromNodeToLocalKey(n)
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
