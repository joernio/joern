package io.joern.jssrc2cpg.passes

import io.joern.x2cpg.passes.frontend._
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._
import overflowdb.BatchedUpdate.DiffGraphBuilder
import overflowdb.traversal.Traversal

import scala.collection.mutable

class JavaScriptTypeRecovery(cpg: Cpg, enabledDummyTypes: Boolean = true) extends XTypeRecovery[File](cpg) {
  override def compilationUnit: Traversal[File] = cpg.file

  override def generateRecoveryForCompilationUnitTask(
    unit: File,
    builder: DiffGraphBuilder
  ): RecoverForXCompilationUnit[File] =
    new RecoverForJavaScriptFile(cpg, unit, builder, globalTable, addedNodes, enabledDummyTypes)

}

class RecoverForJavaScriptFile(
  cpg: Cpg,
  cu: File,
  builder: DiffGraphBuilder,
  globalTable: SymbolTable[GlobalKey],
  addedNodes: mutable.Set[(Long, String)],
  enabledDummyTypes: Boolean
) extends RecoverForXCompilationUnit[File](cpg, cu, builder, globalTable, addedNodes, enabledDummyTypes) {

  /** A heuristic method to determine if a call is a constructor or not.
    */
  override protected def isConstructor(c: Call): Boolean = {
    c.name.endsWith("factory") && c.inCall.astParent.headOption.exists(_.isInstanceOf[Block])
  }

  override protected def visitImport(i: Import): Unit = for {
    entity <- i.importedEntity
    alias  <- i.importedAs
  } {
    val entityPath        = entity.split(":").head
    val isImportingModule = !entity.contains(":")

    def targetAssignments = cpg
      .file(s"${entityPath.stripPrefix(s"./")}.*")
      .method
      .nameExact(":program")
      .ast
      .assignment

    val matchingExports = if (isImportingModule) {
      // If we are importing the whole module, we need to load all entities
      targetAssignments
        .code(s"\\_tmp\\_\\d+\\.\\w+ =.*")
        .dedup
        .l
    } else {
      // If we are importing a specific entity, then we look for it here
      targetAssignments
        .code("exports.*")
        .where(_.argument.code(s"exports\\.$alias.*"))
        .dedup
        .l
    }

    if (matchingExports.nonEmpty) {
      matchingExports.flatMap { exp =>
        exp.argument.l match {
          case List(_, b: Identifier) =>
            val typs = globalTable.get(b)
            symbolTable.append(LocalVar(alias), typs)
          case List(x: Call, b: MethodRef) =>
            val methodName = x.argumentOption(2).map(_.code).getOrElse(b.referencedMethod.name)
            symbolTable.append(CallAlias(methodName, Option(alias)), Set(b.methodFullName))
            symbolTable.append(LocalVar(alias), b.referencedMethod.astParent.collectAll[Method].fullName.toSet)
          case _ =>
            Set.empty[String]
        }
      }.toSet
    } else {
      symbolTable.append(LocalVar(alias), Set(entity))
      symbolTable.append(CallAlias(alias), Set(entity))
    }
  }

  override protected def isField(i: Identifier): Boolean =
    cu.method
      .nameExact(":program")
      .ast
      .assignment
      .code("exports.*")
      .where(_.argument.code(s".*${i.name}.*"))
      .nonEmpty || super.isField(i)

  override protected def visitIdentifierAssignedToConstructor(i: Identifier, c: Call): Set[String] = {
    val constructorPaths = if (c.methodFullName.contains(".alloc")) {
      c.inAssignment.astSiblings.isCall.nameExact("<operator>.new").astChildren.isIdentifier.headOption match {
        case Some(ident) => symbolTable.get(ident)
        case None        => Set.empty[String]
      }
    } else (symbolTable.get(c) + c.methodFullName).map(t => t.stripSuffix(".factory"))
    associateTypes(i, constructorPaths)
  }

  override protected def visitIdentifierAssignedToCall(i: Identifier, c: Call): Set[String] =
    if (c.name == "require") Set.empty
    else super.visitIdentifierAssignedToCall(i, c)

}
