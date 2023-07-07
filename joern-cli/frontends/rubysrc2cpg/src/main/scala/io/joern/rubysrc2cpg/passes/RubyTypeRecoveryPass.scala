package io.joern.rubysrc2cpg.passes

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.joern.x2cpg.passes.frontend.*
import io.shiftleft.semanticcpg.language.*
import io.joern.x2cpg.Defines.{ConstructorMethodName, DynamicCallUnknownFullName}
import io.joern.x2cpg.Defines as XDefines
import io.shiftleft.codepropertygraph.generated.{Operators, PropertyNames}
import io.shiftleft.semanticcpg.language.operatorextension.OpNodes.FieldAccess
import overflowdb.BatchedUpdate.DiffGraphBuilder

class RubyTypeRecoveryPass(cpg: Cpg, config: XTypeRecoveryConfig = XTypeRecoveryConfig())
  extends XTypeRecoveryPass[File](cpg, config) {
  override protected def generateRecoveryPass(state: XTypeRecoveryState): XTypeRecovery[File] =
    new RubyTypeRecovery(cpg, state)
}

private class RubyTypeRecovery(cpg: Cpg, state: XTypeRecoveryState) extends XTypeRecovery[File](cpg, state) {

  override def compilationUnit: Traversal[File] = cpg.file.iterator

  override def generateRecoveryForCompilationUnitTask(
                                                       unit: File,
                                                       builder: DiffGraphBuilder
                                                     ): RecoverForXCompilationUnit[File] = {
    val newConfig = state.config.copy(enabledDummyTypes = state.isFinalIteration && state.config.enabledDummyTypes)
    new RecoverForRubyFile(cpg, unit, builder, state.copy(config = newConfig))
  }
}

private class RecoverForRubyFile(cpg: Cpg, cu: File, builder: DiffGraphBuilder, state: XTypeRecoveryState)
  extends RecoverForXCompilationUnit[File](cpg, cu, builder, state) {

  /** A heuristic method to determine if a call is a constructor or not.
   */
  override protected def isConstructor(c: Call): Boolean = {
    isConstructor(c.name) && c.code.charAt(0).isUpper
  }

  /** A heuristic method to determine if a call name is a constructor or not.
   */
  override protected def isConstructor(name: String): Boolean =
    !name.isBlank && name.equals("new")

  override def visitIdentifierAssignedToConstructor(i: Identifier, c: Call): Set[String] = {

    def isMatching(cName: String, code : String) = {
      val cNameList = cName.split("program:").last.split(":").filterNot(_.isEmpty)
      val codeList = code.split("\\(").head.split(":").filterNot(_.isEmpty)
      cNameList sameElements codeList
    }

    val constructorPaths = symbolTable.get(c).filter(isMatching(_, c.code)).map(_.stripSuffix(s"${pathSep}new"))
    associateTypes(i, constructorPaths)
  }

  override def storeCallTypeInfo(c: Call, types: Seq[String]): Unit =
    if (types.nonEmpty) {
      super.storeCallTypeInfo(c, types)
      // Update the methodFullName if we have only 1 type
      if (c.methodFullName.equals(DynamicCallUnknownFullName) && types.size == 1)
        builder.setNodeProperty(
        c,
        PropertyNames.METHOD_FULL_NAME,
          types.head
      )
    }

}
