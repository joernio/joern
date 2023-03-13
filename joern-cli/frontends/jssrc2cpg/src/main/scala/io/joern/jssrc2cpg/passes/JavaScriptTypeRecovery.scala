package io.joern.jssrc2cpg.passes

import io.joern.x2cpg.passes.frontend._
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes._
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

  override protected def visitImport(i: Import): Unit = for {
    entity <- i.importedEntity
    alias  <- i.importedAs
  } {
    val (entityPath, entityName) = (entity.split(":").head, entity.split(":").tail.mkString(":"))
    val sep                      = java.io.File.separator
    val matchingExports = cpg
      .file(s"${entityPath.stripPrefix(s".$sep")}\\..*")
      .method
      .nameExact(":program")
      .ast
      .assignment
      .code("exports.*")
      .where(_.argument.code(s"exports\\.$entityName.*"))
      .dedup
      .l

    val typs = if (matchingExports.nonEmpty) {
      matchingExports.flatMap { exp =>
        exp.argument.take(2).l match {
          case List(_, b: Identifier) =>
            globalTable.get(b)
          case List(_, b: MethodRef) =>
            globalTable.get(b)
          case _ =>
            Set.empty[String]
        }
      }.toSet
    } else {
      Set(entity)
    }

    symbolTable.append(LocalVar(alias), typs)
    symbolTable.append(CallAlias(alias), typs)
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
        case Some(i) => symbolTable.get(i)
        case None    => Set.empty[String]
      }
    } else (symbolTable.get(c) + c.methodFullName).map(t => t.stripSuffix(".factory"))
    associateTypes(i, constructorPaths)
  }

  override protected def visitIdentifierAssignedToCall(i: Identifier, c: Call): Set[String] =
    if (c.name == "require") Set.empty
    else super.visitIdentifierAssignedToCall(i, c)

}
