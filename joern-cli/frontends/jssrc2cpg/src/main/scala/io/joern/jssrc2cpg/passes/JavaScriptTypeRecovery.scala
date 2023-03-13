package io.joern.jssrc2cpg.passes

import io.joern.x2cpg.passes.frontend.{GlobalKey, RecoverForXCompilationUnit, SymbolTable, XTypeRecovery}
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{Block, Call, File, Identifier}
import io.shiftleft.semanticcpg.language._
import overflowdb.BatchedUpdate.DiffGraphBuilder
import overflowdb.traversal.Traversal

import scala.collection.mutable

class JavaScriptTypeRecovery(cpg: Cpg) extends XTypeRecovery[File](cpg) {
  override def compilationUnit: Traversal[File] = cpg.file

  override def generateRecoveryForCompilationUnitTask(
    unit: File,
    builder: DiffGraphBuilder
  ): RecoverForXCompilationUnit[File] = new RecoverForJavaScriptFile(cpg, unit, builder, globalTable, addedNodes)

}

class RecoverForJavaScriptFile(
  cpg: Cpg,
  cu: File,
  builder: DiffGraphBuilder,
  globalTable: SymbolTable[GlobalKey],
  addedNodes: mutable.Set[(Long, String)]
) extends RecoverForXCompilationUnit[File](cpg, cu, builder, globalTable, addedNodes) {

  /** A heuristic method to determine if a call is a constructor or not.
    */
  override protected def isConstructor(c: Call): Boolean = {
    c.name.endsWith("factory") && c.inCall.astParent.headOption.isInstanceOf[Some[Block]]
  }

  override protected def visitIdentifierAssignedToConstructor(i: Identifier, c: Call): Set[String] = {
    val constructorPaths =
      (symbolTable.get(c) + c.methodFullName).map(t => t.stripSuffix(".factory"))
    associateTypes(i, constructorPaths)
  }

}
