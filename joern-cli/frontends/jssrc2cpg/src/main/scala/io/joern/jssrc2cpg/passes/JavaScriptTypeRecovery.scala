package io.joern.jssrc2cpg.passes

import io.joern.x2cpg.Defines.ConstructorMethodName
import io.joern.x2cpg.passes.frontend._
import io.joern.x2cpg.{Defines => XDefines}
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.codepropertygraph.generated.{Operators, PropertyNames}
import io.shiftleft.semanticcpg.language._
import io.shiftleft.semanticcpg.language.operatorextension.OpNodes.FieldAccess
import overflowdb.BatchedUpdate.DiffGraphBuilder

import java.io.{File => JFile}
import java.util.regex.{Matcher, Pattern}
import scala.util.{Failure, Success, Try}

class JavaScriptTypeRecoveryPass(cpg: Cpg, config: XTypeRecoveryConfig = XTypeRecoveryConfig())
    extends XTypeRecoveryPass[File](cpg, config) {
  override protected def generateRecoveryPass(state: XTypeRecoveryState): XTypeRecovery[File] =
    new JavaScriptTypeRecovery(cpg, state)
}

private class JavaScriptTypeRecovery(cpg: Cpg, state: XTypeRecoveryState) extends XTypeRecovery[File](cpg, state) {

  override def compilationUnit: Iterator[File] = cpg.file.iterator

  override def generateRecoveryForCompilationUnitTask(
    unit: File,
    builder: DiffGraphBuilder
  ): RecoverForXCompilationUnit[File] = {
    val newConfig = state.config.copy(enabledDummyTypes = state.isFinalIteration && state.config.enabledDummyTypes)
    new RecoverForJavaScriptFile(cpg, unit, builder, state.copy(config = newConfig))
  }

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

  override protected def prepopulateSymbolTableEntry(x: AstNode): Unit = x match {
    case x @ (_: Identifier | _: Local | _: MethodParameterIn)
        if x.property(PropertyNames.TYPE_FULL_NAME, Defines.Any) != Defines.Any =>
      val typeFullName = x.property(PropertyNames.TYPE_FULL_NAME, Defines.Any)
      val typeHints    = symbolTable.get(LocalVar(x.property(PropertyNames.TYPE_FULL_NAME, Defines.Any))) - typeFullName
      lazy val cpgTypeFullName = cpg.typeDecl.nameExact(typeFullName).fullName.toSet
      val resolvedTypeHints =
        if (typeHints.nonEmpty) symbolTable.put(x, typeHints)
        else if (cpgTypeFullName.nonEmpty) symbolTable.put(x, cpgTypeFullName)
        else symbolTable.put(x, getTypes(x))
      if (!resolvedTypeHints.contains(typeFullName) && resolvedTypeHints.sizeIs == 1)
        builder.setNodeProperty(x, PropertyNames.TYPE_FULL_NAME, resolvedTypeHints.head)
    case x @ (_: Identifier | _: Local | _: MethodParameterIn) =>
      symbolTable.put(x, getTypes(x))
    case x: Call => symbolTable.put(x, (x.methodFullName +: x.dynamicTypeHintFullName).toSet)
    case _       =>
  }

  override protected def prepopulateSymbolTable(): Unit = {
    super.prepopulateSymbolTable()
    cu.ast.isMethod.foreach(f => symbolTable.put(CallAlias(f.name, Option("this")), Set(f.fullName)))
    (cu.ast.isParameter.whereNot(_.nameExact("this")) ++ cu.ast.isMethod.methodReturn).filter(hasTypes).foreach { p =>
      val resolvedHints = getTypes(p)
        .map { t =>
          t.split("\\.").headOption match {
            case Some(base) if symbolTable.contains(LocalVar(base)) =>
              (t, symbolTable.get(LocalVar(base)).map(x => x + t.stripPrefix(base)))
            case _ => (t, Set(t))
          }
        }
        .flatMap {
          case (t, ts) if Set(t) == ts => Set(t)
          case (_, ts)                 => ts.map(_.replaceAll("\\.(?!js::program)", pathSep.toString))
        }
      p match {
        case _: MethodParameterIn => symbolTable.put(p, resolvedHints)
        case _: MethodReturn if resolvedHints.sizeIs == 1 =>
          builder.setNodeProperty(p, PropertyNames.TYPE_FULL_NAME, resolvedHints.head)
        case _: MethodReturn =>
          builder.setNodeProperty(p, PropertyNames.TYPE_FULL_NAME, Defines.Any)
          builder.setNodeProperty(p, PropertyNames.DYNAMIC_TYPE_HINT_FULL_NAME, resolvedHints)
        case _ =>
      }
    }
  }

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
    val isLocalImport = i.importedEntity.exists(_.matches("^[.]+/?.*"))
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
        Iterator.empty
    ) match {
      case Failure(_) =>
        logger.warn(s"Unable to resolve import due to irregular regex at '${i.importedEntity.getOrElse("")}'")
        Iterator.empty
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
        .code("^(module.)?exports.*")
        .where(_.argument.codeExact(alias))
        .dedup
        .l
    }

    if (matchingExports.nonEmpty) {
      matchingExports.flatMap { exp =>
        exp.argument.l match {
          case ::(expCall: Call, ::(b: Identifier, _)) if expCall.code.matches("^(module.)?exports[.]?.*") =>
            val moduleMethods      = targetModule.ast.isMethod.l
            lazy val methodMatches = moduleMethods.name(b.name).l
            lazy val constructorMatches =
              moduleMethods.fullName(s".*${b.name}$pathSep${XDefines.ConstructorMethodName}$$").l
            lazy val variableMatches = cpg
              .file(s"${Pattern.quote(resolvedPath)}\\.?.*")
              .method
              .ast
              .isIdentifier
              .name(b.name)
              .flatMap(i => i.typeFullName +: i.dynamicTypeHintFullName)
              .filterNot(_ == Defines.Any)
              .toSet
            // Exported function with only the name of the function
            val methodPaths =
              if (methodMatches.nonEmpty) methodMatches.fullName.toSet
              else constructorMatches.fullName.toSet
            if (methodPaths.nonEmpty) {
              symbolTable.append(CallAlias(alias, Option("this")), methodPaths)
              symbolTable.append(LocalVar(alias), methodPaths)
            } else {
              symbolTable.append(LocalVar(alias), variableMatches)
            }
          case ::(x: Call, ::(b: MethodRef, _)) =>
            // Exported function with a method ref of the function
            val methodName = x.argumentOption(2).map(_.code).getOrElse(b.referencedMethod.name)
            if (methodName == "exports") symbolTable.append(CallAlias(alias, Option("this")), Set(b.methodFullName))
            else symbolTable.append(CallAlias(methodName, Option(alias)), Set(b.methodFullName))
            symbolTable.append(LocalVar(alias), b.referencedMethod.astParent.iterator.collectAll[Method].fullName.toSet)
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
      val default = Set(entity)
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

  override protected def visitIdentifierAssignedToConstructor(i: Identifier, c: Call): Set[String] = {
    val constructorPaths = if (c.methodFullName.endsWith(".alloc")) {
      def newChildren = c.inAssignment.astSiblings.isCall.nameExact("<operator>.new").astChildren
      val possibleImportIdentifier = newChildren.isIdentifier.headOption match {
        case Some(i) if GlobalBuiltins.builtins.contains(i.name) => Set(s"__ecma.${i.name}")
        case Some(i)                                             => symbolTable.get(i)
        case None                                                => Set.empty[String]
      }
      lazy val possibleConstructorPointer =
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
    if (symbolTable.contains(LocalVar(fieldName))) {
      val fieldTypes = symbolTable.get(LocalVar(fieldName))
      symbolTable.append(i, fieldTypes)
    } else if (symbolTable.contains(CallAlias(fieldName, Option("this")))) {
      symbolTable.get(CallAlias(fieldName, Option("this")))
    } else {
      super.associateInterproceduralTypes(
        i: Identifier,
        fieldFullName: String,
        fieldName: String,
        globalTypes: Set[String],
        baseTypes: Set[String]
      )
    }
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
      .where(_.typeFullNameExact(Defines.Any))
      .filterNot(_.dynamicTypeHintFullName.isEmpty)
      .foreach(setTypeFromTypeHints)
  }

  protected override def storeIdentifierTypeInfo(i: Identifier, types: Seq[String]): Unit =
    super.storeIdentifierTypeInfo(i, types.map(_.stripSuffix(s"$pathSep${XDefines.ConstructorMethodName}")))

  protected override def storeLocalTypeInfo(i: Local, types: Seq[String]): Unit =
    super.storeLocalTypeInfo(i, types.map(_.stripSuffix(s"$pathSep${XDefines.ConstructorMethodName}")))

}
