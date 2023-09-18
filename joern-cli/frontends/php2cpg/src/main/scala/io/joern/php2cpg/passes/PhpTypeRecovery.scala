package io.joern.php2cpg.passes

import io.joern.x2cpg.passes.frontend._
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.semanticcpg.language._
import io.shiftleft.semanticcpg.language.operatorextension.OpNodes
import io.shiftleft.semanticcpg.language.operatorextension.OpNodes.{Assignment, FieldAccess}
import overflowdb.BatchedUpdate.DiffGraphBuilder

class PhpTypeRecoveryPass(cpg: Cpg, config: XTypeRecoveryConfig = XTypeRecoveryConfig())
  extends XTypeRecoveryPass[NamespaceBlock](cpg, config) {

  override protected def generateRecoveryPass(state: XTypeRecoveryState): XTypeRecovery[NamespaceBlock] =
    new PhpTypeRecovery(cpg, state)
}

private class PhpTypeRecovery(cpg: Cpg, state: XTypeRecoveryState) extends XTypeRecovery[NamespaceBlock](cpg, state) {

  override def compilationUnit: Iterator[NamespaceBlock] = cpg.file.namespaceBlock.iterator

  override def generateRecoveryForCompilationUnitTask(
    unit: NamespaceBlock,
    builder: DiffGraphBuilder
  ): RecoverForXCompilationUnit[NamespaceBlock] = {
    val newConfig = state.config.copy(enabledDummyTypes = state.isFinalIteration && state.config.enabledDummyTypes)
    new RecoverForPhpFile(cpg, unit, builder, state.copy(config = newConfig))
  }
}

private class RecoverForPhpFile(cpg: Cpg, cu: NamespaceBlock, builder: DiffGraphBuilder, state: XTypeRecoveryState)
  extends RecoverForXCompilationUnit[NamespaceBlock](cpg, cu, builder, state) {

  override def isConstructor(c: Call): Boolean = isConstructor(c.name)

  override protected def isConstructor(name: String): Boolean =
    !name.isBlank && name.charAt(0).isUpper

  override def assignments: Iterator[Assignment] = {
    logger.debug(s"[assignments] assignments: ${cu.ast.isCall.nameExact(Operators.assignment).l.map(a => a.code).mkString("; ")}")
    cu.ast.isCall.nameExact(Operators.assignment).map(new OpNodes.Assignment(_))
  }

  override def compute(): Boolean = {
    logger.debug(s"compute() file: ${cu.file.name.l.mkString(" ")}")
    super.compute()
  }

  override def visitAssignments(a: OpNodes.Assignment): Set[String] = {
    logger.debug(s"visiting assigment: ${a.name}")
    logger.debug(s"- arguments: ${a.argumentOut.l.mkString(" ")}")
    logger.debug(s"- iteration: ${state.currentIteration}")
    super.visitAssignments(a)
  }

  override protected def visitIdentifierAssignedToBlock(i: Identifier, b: Block): Set[String] = {
    logger.debug(s"visiting identifier ${i.name} assigned to block ${b.id}")
    super.visitIdentifierAssignedToBlock(i, b)
  }

  override protected def visitCallAssignedToBlock(c: Call, b: Block): Set[String] = {
    logger.debug(s"visiting call ${c.name} assigned to block ${b.id}")
    super.visitCallAssignedToBlock(c, b)
  }

  override protected def visitStatementsInBlock(b: Block, assignmentTarget: Option[Identifier] = None): Set[String] = {
    logger.debug(s"visiting statements in block ${b.id}")
    super.visitStatementsInBlock(b, assignmentTarget)
  }

  override protected def visitIdentifierAssignedToCall(i: Identifier, c: Call): Set[String] = {
    logger.debug(s"visiting identifier ${i.name} assigned to call ${c.name}")
    super.visitIdentifierAssignedToCall(i, c)
  }

  override protected def visitIdentifierAssignedToIdentifier(x: Identifier, y: Identifier): Set[String] = {
    logger.debug(s"visiting identifier ${x.name} assigned to identifier ${y.name}")
    super.visitIdentifierAssignedToIdentifier(x, y)
  }

  override protected def visitIdentifierAssignedToOperator(i: Identifier, c: Call, operation: String): Set[String] = {
    logger.debug(s"visiting identifier ${i.name} assigned to operator ${operation}")
    super.visitIdentifierAssignedToOperator(i, c, operation)
  }

  override protected def visitIdentifierAssignedToConstructor(i: Identifier, c: Call): Set[String] = {
    logger.debug(s"visiting identifier ${i.name} assigned to constructor ${c.name}")
    super.visitIdentifierAssignedToConstructor(i, c)
  }

  override protected def visitIdentifierAssignedToCallRetVal(i: Identifier, c: Call): Set[String] = {
    logger.debug(s"visiting identifier ${i.name} assigned to CallRetVal ${c.name}")
    super.visitIdentifierAssignedToCallRetVal(i, c)
  }

  override protected def visitIdentifierAssignedToLiteral(i: Identifier, l: Literal): Set[String] = {
    logger.debug(s"visiting identifier ${i.name} assigned to literal ${l.code}")
    super.visitIdentifierAssignedToLiteral(i, l)
  }

  override protected def visitIdentifierAssignedToMethodRef(
    i: Identifier,
    m: MethodRef,
    rec: Option[String] = None
  ): Set[String] = {
    logger.debug(s"visiting identifier ${i.name} assigned to MethodRef ${m.code}")
    super.visitIdentifierAssignedToMethodRef(i, m, rec)
  }

  override protected def visitIdentifierAssignedToTypeRef(
    i: Identifier,
    t: TypeRef,
    rec: Option[String] = None
  ): Set[String] = {
    logger.debug(s"visiting identifier ${i.name} assigned to TypeRef ${t.code}")
    super.visitIdentifierAssignedToTypeRef(i, t, rec)
  }

  override protected def visitCallAssignedToIdentifier(c: Call, i: Identifier): Set[String] = {
    logger.debug(s"visiting call ${c.name} assigned to identifier ${i.name}")
    super.visitCallAssignedToIdentifier(c, i)
  }

  override protected def visitCallAssignedToCall(x: Call, y: Call): Set[String] = {
    logger.debug(s"visiting call ${x.name} assigned to call ${y.name}")
    super.visitCallAssignedToCall(x, y)
  }

  override protected def visitCallAssignedToLiteral(c: Call, l: Literal): Set[String] = {
    logger.debug(s"visiting call ${c.name} assigned to literal ${l.code}")
    super.visitCallAssignedToLiteral(c, l)
  }

  override protected def visitCallAssignedToMethodRef(c: Call, m: MethodRef): Set[String] = {
    logger.debug(s"visiting call ${c.name} assigned to MethodRef ${m.code}")
    super.visitCallAssignedToMethodRef(c, m)
  }

  override protected def visitIdentifierAssignedToFieldLoad(i: Identifier, fa: FieldAccess): Set[String] = {
    logger.debug(s"visiting field identifier: ${getFieldName(fa)}")
    super.visitIdentifierAssignedToFieldLoad(i, fa)
  }

  /** Necessary to change the filter regex from (this|self) to (\\$this|this),
   *  in order to account for $this PHP convention.
   *
   * (But need to leave the regular "this"? uncertain)
   */
  override protected def associateTypes(symbol: LocalVar, fa: FieldAccess, types: Set[String]): Set[String] = {
    fa.astChildren.filterNot(_.code.matches("(\\$this|this)")).headOption.collect {
      case fi: FieldIdentifier =>
        getFieldParents(fa).foreach(t => persistMemberWithTypeDecl(t, fi.canonicalName, types))
      case i: Identifier if isField(i) =>
        getFieldParents(fa).foreach(t => persistMemberWithTypeDecl(t, i.name, types))
    }
    symbolTable.append(symbol, types)
  }

  /** Reference the PythonTypeRecovery implementation.
   *  The XTypeRecovery one seems incorrect.
   */
  override protected def getFieldParents(fa: FieldAccess): Set[String] = {
    if (fa.method.name == "<module>") {
      Set(fa.method.fullName)
    } else if (fa.method.typeDecl.nonEmpty) {
      val parentTypes       = fa.method.typeDecl.fullName.toSet
      val baseTypeFullNames = cpg.typeDecl.fullNameExact(parentTypes.toSeq: _*).inheritsFromTypeFullName.toSet
      (parentTypes ++ baseTypeFullNames).filterNot(_.matches("(?i)(any|object)"))
    } else {
      super.getFieldParents(fa)
    }
  }

  override protected def persistMemberWithTypeDecl(typeFullName: String, memberName: String, types: Set[String]): Unit = {
    logger.debug(s"persisting member with TypeDecl: typeFullName: ${typeFullName}, memberName: ${memberName}, types: [${types.mkString("; ")}]")
    super.persistMemberWithTypeDecl(typeFullName, memberName, types)
  }

}