package io.joern.jssrc2cpg.passes

import io.joern.x2cpg.passes.frontend._
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._
import overflowdb.BatchedUpdate.DiffGraphBuilder
import overflowdb.traversal.Traversal

import java.io.{File => JFile}
import java.util.regex.Matcher
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

  override protected val pathSep = ':'

  /** A heuristic method to determine if a call is a constructor or not.
    */
  override protected def isConstructor(c: Call): Boolean = {
    c.name.endsWith("factory") && c.inCall.astParent.headOption.exists(_.isInstanceOf[Block])
  }

  override protected def visitImport(i: Import): Unit = for {
    entity <- i.importedEntity.map(_.stripPrefix("./"))
    alias  <- i.importedAs
  } {
    val sep = Matcher.quoteReplacement(JFile.separator)
    val currentFile = codeRoot + (cu match {
      case x: File => x.name
      case _       => cu.file.name.headOption.getOrElse("")
    })
    val resolvedPath = better.files
      .File(currentFile.stripSuffix(currentFile.split(sep).last), entity.split(":").head)
      .pathAsString
      .stripPrefix(codeRoot)

    val isImportingModule = !entity.contains(":")

    def targetModule = cpg
      .file(s"$resolvedPath\\.?.*")
      .method

    def targetAssignments = targetModule
      .nameExact(":program")
      .ast
      .assignment

    val matchingExports = if (isImportingModule) {
      // If we are importing the whole module, we need to load all entities
      targetAssignments
        .code(s"\\_tmp\\_\\d+\\.\\w+ =.*", "module\\.exports.*")
        .dedup
        .l
    } else {
      // If we are importing a specific entity, then we look for it here
      targetAssignments
        .code("exports\\..*")
        .where(_.argument.code(s"exports\\.$alias.*"))
        .dedup
        .l
    }

    println(s"---Method names---\n${cpg.method.fullName.mkString("\n")}\n--END---")

    if (matchingExports.nonEmpty) {
      matchingExports.flatMap { exp =>
        exp.argument.l match {
          case List(expCall: Call, b: Identifier)
              if expCall.code.startsWith("exports.") && targetModule.ast.isMethod.name(b.name).nonEmpty =>
            // Exported function with only the name of the function
            val methodPaths = targetModule.ast.isMethod.name(b.name).fullName.toSet
            symbolTable.append(CallAlias(alias, Option("this")), methodPaths)
            symbolTable.append(LocalVar(alias), methodPaths)
          case List(_, b: Identifier) =>
            // Exported variable
            val typs = globalTable.get(b)
            symbolTable.append(LocalVar(alias), typs)
          case List(x: Call, b: MethodRef) =>
            // Exported function with a method ref of the function
            val methodName = x.argumentOption(2).map(_.code).getOrElse(b.referencedMethod.name)
            if (methodName == "exports") symbolTable.append(CallAlias(alias, Option("this")), Set(b.methodFullName))
            else symbolTable.append(CallAlias(methodName, Option(alias)), Set(b.methodFullName))
            symbolTable.append(LocalVar(alias), b.referencedMethod.astParent.collectAll[Method].fullName.toSet)
          case _ =>
            Set.empty[String]
        }
      }.toSet
    } else {
      val default = Set(entity).map(_.replaceAll(sep, "/"))
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
      def newChildren = c.inAssignment.astSiblings.isCall.nameExact("<operator>.new").astChildren
      val possibleImportIdentifier = newChildren.isIdentifier.headOption match {
        case Some(i) => symbolTable.get(i)
        case None    => Set.empty[String]
      }
      val possibleConstructorPointer =
        newChildren.astChildren.isFieldIdentifier.map(f => CallAlias(f.canonicalName, Some("this"))).headOption match {
          case Some(fi) => symbolTable.get(fi)
          case None     => Set.empty[String]
        }

      if (possibleImportIdentifier.nonEmpty) possibleImportIdentifier
      else if (possibleConstructorPointer.nonEmpty) possibleConstructorPointer
      else Set.empty[String]
    } else (symbolTable.get(c) + c.methodFullName).map(t => t.stripSuffix(".factory"))
    associateTypes(i, constructorPaths)
  }

  override protected def visitIdentifierAssignedToCall(i: Identifier, c: Call): Set[String] =
    if (c.name == "require") Set.empty
    else super.visitIdentifierAssignedToCall(i, c)

  override protected def visitIdentifierAssignedToMethodRef(
    i: Identifier,
    m: MethodRef,
    rec: Option[String] = None
  ): Set[String] =
    super.visitIdentifierAssignedToMethodRef(i, m, Option("this"))

  override protected def visitIdentifierAssignedToTypeRef(
    i: Identifier,
    t: TypeRef,
    rec: Option[String] = None
  ): Set[String] =
    super.visitIdentifierAssignedToTypeRef(i, t, Option("this"))

}
