package io.joern.rubysrc2cpg.passes

import io.joern.x2cpg.Defines as XDefines
import io.joern.x2cpg.passes.frontend.*
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*
import overflowdb.BatchedUpdate.DiffGraphBuilder

class RubyTypeRecoveryPassGenerator(cpg: Cpg, config: XTypeRecoveryConfig = XTypeRecoveryConfig())
    extends XTypeRecoveryPassGenerator[File](cpg, config) {
  override protected def generateRecoveryPass(state: XTypeRecoveryState, iteration: Int): XTypeRecovery[File] =
    new RubyTypeRecovery(cpg, state, iteration)
}

private class RubyTypeRecovery(cpg: Cpg, state: XTypeRecoveryState, iteration: Int)
    extends XTypeRecovery[File](cpg, state, iteration) {

  override def compilationUnits: Iterator[File] = cpg.file.iterator

  override def generateRecoveryForCompilationUnitTask(
    unit: File,
    builder: DiffGraphBuilder
  ): RecoverForXCompilationUnit[File] = {
    new RecoverForRubyFile(cpg, unit, builder, state)
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
    !name.isBlank && (name == "new" || name == Defines.Initialize)

  override protected def hasTypes(node: AstNode): Boolean = node match {
    case x: Call if !x.methodFullName.startsWith("<operator>") =>
      !x.methodFullName.toLowerCase().matches("(<unknownfullname>|any)")
    case x: Call if x.methodFullName.startsWith("<operator>") =>
      x.typeFullName != "<empty>" && super.hasTypes(node)
    case x => super.hasTypes(node)
  }

  override def visitImport(i: Import): Unit = for {
    resolvedImport <- i.call.tag
    alias          <- i.importedAs
  } {
    import io.shiftleft.semanticcpg.language.importresolver.*
    EvaluatedImport.tagToEvaluatedImport(resolvedImport).foreach {
      case ResolvedTypeDecl(fullName, _) =>
        symbolTable.append(LocalVar(fullName.split("\\.").lastOption.getOrElse(alias)), fullName)
      case _ => super.visitImport(i)
    }
  }

  override def visitIdentifierAssignedToConstructor(i: Identifier, c: Call): Set[String] = {
    associateTypes(i, Set(i.typeFullName))
  }
}
