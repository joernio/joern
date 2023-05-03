package io.joern.jssrc2cpg.passes

import io.joern.x2cpg.Defines.ConstructorMethodName
import io.joern.x2cpg.passes.frontend._
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._
import io.shiftleft.semanticcpg.language.operatorextension.OpNodes.FieldAccess
import overflowdb.BatchedUpdate.DiffGraphBuilder
import overflowdb.traversal.Traversal

import java.io.{File => JFile}
import java.util.regex.{Matcher, Pattern}
import scala.util.{Failure, Success, Try}

class JavaScriptTypeRecoveryPass(cpg: Cpg, config: XTypeRecoveryConfig = XTypeRecoveryConfig())
    extends XTypeRecoveryPass[File](cpg, config) {
  override protected def generateRecoveryPass(state: XTypeRecoveryState): XTypeRecovery[File] =
    new JavaScriptTypeRecovery(cpg, state)
}

private class JavaScriptTypeRecovery(cpg: Cpg, state: XTypeRecoveryState) extends XTypeRecovery[File](cpg, state) {
  override def compilationUnit: Traversal[File] = cpg.file

  override def generateRecoveryForCompilationUnitTask(
    unit: File,
    builder: DiffGraphBuilder
  ): RecoverForXCompilationUnit[File] =
    new RecoverForJavaScriptFile(
      cpg,
      unit,
      builder,
      state.copy(config =
        state.config.copy(enabledDummyTypes = state.isFinalIteration && state.config.enabledDummyTypes)
      )
    )

}

private class RecoverForJavaScriptFile(cpg: Cpg, cu: File, builder: DiffGraphBuilder, state: XTypeRecoveryState)
    extends RecoverForXCompilationUnit[File](cpg, cu, builder, state) {

  override protected val pathSep = ':'

  /** A heuristic method to determine if a call is a constructor or not.
    */
  override protected def isConstructor(c: Call): Boolean = {
    c.name.endsWith("factory") && c.inCall.astParent.headOption.exists(_.isInstanceOf[Block])
  }

  override protected def isConstructor(name: String): Boolean =
    !name.isBlank && (name.charAt(0).isUpper || name.endsWith("factory"))

  lazy private val pathPattern = Pattern.compile("[\"']([\\w/.]+)[\"']")

  override protected def visitImport(i: Import): Unit = for {
    rawEntity <- i.importedEntity.map(_.stripPrefix("./"))
    alias     <- i.importedAs
  } {
    val matcher = pathPattern.matcher(rawEntity)
    val sep     = Matcher.quoteReplacement(JFile.separator)
    val currentFile = codeRoot + (cu match {
      case x: File => x.name
      case _       => cu.file.name.headOption.getOrElse("")
    })
    // We want to know if the import is local since if an external name is used to match internal methods we may have
    // false paths.
    val isLocalImport = i.importedEntity.exists(_.matches("^[.]*/?.*"))
    // TODO: At times there is an operation inside of a require, e.g. path.resolve(__dirname + "/../config/env/all.js")
    //  this tries to recover the string but does not perform string constant propagation
    val entity = if (matcher.find()) matcher.group(1) else rawEntity
    val resolvedPath = better.files
      .File(currentFile.stripSuffix(currentFile.split(sep).last), entity.split(":").head)
      .pathAsString
      .stripPrefix(codeRoot)

    val isImportingModule = !entity.contains(":")

    def targetModule = Try(
      if (isLocalImport)
        cpg
          .file(s"${Pattern.quote(resolvedPath)}\\.?.*")
          .method
      else
        Traversal.empty
    ) match {
      case Failure(_) =>
        logger.warn(s"Unable to resolve import due to irregular regex at '${i.importedEntity.getOrElse("")}'")
        Traversal.empty
      case Success(modules) => modules
    }

    def targetAssignments = targetModule
      .nameExact(":program")
      .ast
      .assignment

    val matchingExports = if (isImportingModule) {
      // If we are importing the whole module, we need to load all entities
      targetAssignments
        .code(s"\\_tmp\\_\\d+\\.\\w+ =.*", "(module\\.)?exports.*")
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

    if (matchingExports.nonEmpty) {
      matchingExports.flatMap { exp =>
        exp.argument.l match {
          case ::(expCall: Call, ::(b: Identifier, _))
              if expCall.code.startsWith("exports.") && targetModule.ast.isMethod.name(b.name).nonEmpty =>
            // Exported function with only the name of the function
            val methodPaths = targetModule.ast.isMethod.name(b.name).fullName.toSet
            symbolTable.append(CallAlias(alias, Option("this")), methodPaths)
            symbolTable.append(LocalVar(alias), methodPaths)
          case ::(_, ::(b: Identifier, _)) =>
            // Exported variable that we should find
            val typs = cpg
              .file(s"${Pattern.quote(resolvedPath)}\\.?.*")
              .method
              .ast
              .isIdentifier
              .name(b.name)
              .flatMap(i => i.typeFullName +: i.dynamicTypeHintFullName)
              .filterNot(_ == "ANY")
              .toSet
            symbolTable.append(LocalVar(alias), typs)
          case ::(x: Call, ::(b: MethodRef, _)) =>
            // Exported function with a method ref of the function
            val methodName = x.argumentOption(2).map(_.code).getOrElse(b.referencedMethod.name)
            if (methodName == "exports") symbolTable.append(CallAlias(alias, Option("this")), Set(b.methodFullName))
            else symbolTable.append(CallAlias(methodName, Option(alias)), Set(b.methodFullName))
            symbolTable.append(LocalVar(alias), b.referencedMethod.astParent.collectAll[Method].fullName.toSet)
          case ::(_, ::(y: Call, _)) =>
            // Exported closure with a method ref within the AST of the RHS
            y.ast.isMethodRef.flatMap { mRef =>
              val methodName = mRef.referencedMethod.name
              symbolTable.append(CallAlias(methodName, Option(alias)), Set(mRef.methodFullName))
            }
          case _ =>
            Set.empty[String]
        }
      }.toSet
    } else {
      val default = Set(entity).map(_.replaceAll("/", sep))
      symbolTable.append(LocalVar(alias), default)
      symbolTable.append(CallAlias(alias, Option("this")), default)
    }
  }

  private lazy val exportedIdentifiers = cu.method
    .nameExact(":program")
    .ast
    .isCall
    .nameExact(Operators.assignment)
    .filter(_.code.startsWith("exports.*"))
    .argument
    .isIdentifier
    .name
    .toSet

  override protected def isField(i: Identifier): Boolean =
    state.isFieldCache.getOrElseUpdate(i.id(), exportedIdentifiers.contains(i.name) || super.isField(i))

  override protected def prepopulateSymbolTable(): Unit = {
    super.prepopulateSymbolTable()
    cu.ast.isMethod.foreach(f => symbolTable.put(CallAlias(f.name, Option("this")), Set(f.fullName)))
  }

  override protected def visitIdentifierAssignedToConstructor(i: Identifier, c: Call): Set[String] = {
    val constructorPaths = if (c.methodFullName.endsWith(".alloc")) {
      def newChildren = c.inAssignment.astSiblings.isCall.nameExact("<operator>.new").astChildren
      val possibleImportIdentifier = newChildren.isIdentifier.headOption match {
        case Some(i) if GlobalBuiltins.builtins.contains(i.name) => Set(s"__ecma.${i.name}")
        case Some(i)                                             => symbolTable.get(i)
        case None                                                => Set.empty[String]
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

  override protected def visitIdentifierAssignedToOperator(i: Identifier, c: Call, operation: String): Set[String] = {
    operation match {
      case "<operator>.new" =>
        c.astChildren.l match {
          case ::(fa: Call, ::(i: Identifier, _)) if fa.name == Operators.fieldAccess =>
            symbolTable.append(
              c,
              visitIdentifierAssignedToFieldLoad(i, new FieldAccess(fa)).map(t => s"$t$pathSep$ConstructorMethodName")
            )
          case _ => Set.empty
        }
      case _ => super.visitIdentifierAssignedToOperator(i, c, operation)
    }
  }

  override protected def associateInterproceduralTypes(
    i: Identifier,
    fieldFullName: String,
    fieldName: String,
    globalTypes: Set[String],
    baseTypes: Set[String]
  ): Set[String] = {
    if (symbolTable.contains(LocalVar(fieldName))) symbolTable.get(LocalVar(fieldName))
    else if (symbolTable.contains(CallAlias(fieldName, Option("this"))))
      symbolTable.get(CallAlias(fieldName, Option("this")))
    else
      super.associateInterproceduralTypes(
        i: Identifier,
        fieldFullName: String,
        fieldName: String,
        globalTypes: Set[String],
        baseTypes: Set[String]
      )
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

  override protected def postSetTypeInformation(): Unit = {
    // often there are "this" identifiers with type hints but this can be set to a type hint if they meet the criteria
    cu.ast.isIdentifier
      .nameExact("this")
      .where(_.typeFullName("ANY"))
      .filterNot(_.dynamicTypeHintFullName.isEmpty)
      .foreach(setTypeFromTypeHints)
  }

}
