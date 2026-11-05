package io.joern.x2cpg.frontendspecific.jssrc2cpg

import io.joern.x2cpg.Defines as XDefines
import io.joern.x2cpg.Defines.ConstructorMethodName
import io.joern.x2cpg.passes.frontend.*
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.{Operators, Properties, PropertyNames}
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.operatorextension.OpNodes.FieldAccess
import io.shiftleft.codepropertygraph.generated.DiffGraphBuilder

class JavaScriptTypeRecoveryPassGenerator(cpg: Cpg, config: XTypeRecoveryConfig = XTypeRecoveryConfig())
    extends XTypeRecoveryPassGenerator[File](cpg, config) {
  override protected def generateRecoveryPass(state: XTypeRecoveryState, iteration: Int): XTypeRecovery[File] =
    new JavaScriptTypeRecovery(cpg, state, iteration)
}

private class JavaScriptTypeRecovery(cpg: Cpg, state: XTypeRecoveryState, iteration: Int)
    extends XTypeRecovery[File](cpg, state, iteration) {

  override def compilationUnits: Iterator[File] = cpg.file.iterator

  override def generateRecoveryForCompilationUnitTask(
    unit: File,
    builder: DiffGraphBuilder
  ): RecoverForXCompilationUnit[File] = {
    new RecoverForJavaScriptFile(cpg, unit, builder, state)
  }

}

private class RecoverForJavaScriptFile(cpg: Cpg, cu: File, builder: DiffGraphBuilder, state: XTypeRecoveryState)
    extends RecoverForXCompilationUnit[File](cpg, cu, builder, state) {

  import io.joern.x2cpg.passes.frontend.XTypeRecovery.AllNodeTypesFromNodeExt

  override protected val pathSep = ":"

  /** A heuristic method to determine if a call is a constructor or not.
    */
  override protected def isConstructor(c: Call): Boolean = {
    c.name.endsWith("factory") && c.inCall.astParent.headOption.exists(_.isInstanceOf[Block])
  }

  override protected def isConstructor(name: String): Boolean =
    !name.isBlank && (name.charAt(0).isUpper || name.endsWith("factory"))

  override protected def prepopulateSymbolTableEntry(x: AstNode): Unit = x match {
    case x @ (_: Identifier | _: Local | _: MethodParameterIn)
        if x.propertyOption(Properties.TypeFullName).getOrElse(Defines.Any) != Defines.Any =>
      val typeFullName         = x.propertyOption(Properties.TypeFullName).getOrElse(Defines.Any)
      val typeHints            = symbolTable.get(LocalVar(typeFullName)) - typeFullName
      lazy val cpgTypeFullName = cpg.typeDecl.nameExact(typeFullName).fullName.toSet
      val resolvedTypeHints    =
        if (typeHints.nonEmpty) symbolTable.put(x, typeHints)
        else if (cpgTypeFullName.nonEmpty) symbolTable.put(x, cpgTypeFullName)
        else symbolTable.put(x, x.getKnownTypes)
      if (!resolvedTypeHints.contains(typeFullName) && resolvedTypeHints.sizeIs == 1)
        builder.setNodeProperty(x, PropertyNames.TypeFullName, resolvedTypeHints.head)

    case x @ (_: Identifier | _: Local | _: MethodParameterIn) if x.property(Properties.PossibleTypes).nonEmpty =>
      val possibleTypes = x.property(Properties.PossibleTypes)
      if (possibleTypes.sizeIs == 1 && !possibleTypes.contains("ANY")) {
        val typeFullName         = possibleTypes.head
        val typeHints            = symbolTable.get(LocalVar(typeFullName)) - typeFullName
        lazy val cpgTypeFullName = cpg.typeDecl.nameExact(typeFullName).fullName.toSet
        val resolvedTypeHints    =
          if (typeHints.nonEmpty) symbolTable.put(x, typeHints)
          else if (cpgTypeFullName.nonEmpty) symbolTable.put(x, cpgTypeFullName)
          else symbolTable.put(x, x.getKnownTypes)
        if (!resolvedTypeHints.contains(typeFullName) && resolvedTypeHints.sizeIs == 1)
          builder.setNodeProperty(x, PropertyNames.TypeFullName, resolvedTypeHints.head)
      }
    case x @ (_: Identifier | _: Local | _: MethodParameterIn) =>
      symbolTable.put(x, x.getKnownTypes)
    case call: Call =>
      symbolTable.put(call, (call.methodFullName +: (call.dynamicTypeHintFullName ++ call.possibleTypes)).toSet)
    case _ =>
  }

  override protected def prepopulateSymbolTable(): Unit = {
    super.prepopulateSymbolTable()
    cu.ast.isMethod.foreach(f => symbolTable.put(CallAlias(f.name, Option("this")), Set(f.fullName)))
    (cu.ast.isParameter.whereNot(_.nameExact("this")) ++ cu.ast.isMethod.methodReturn).filter(hasTypes).foreach { p =>
      val resolvedHints = p.getKnownTypes
        .map { t =>
          t.split("\\.").headOption match {
            case Some(base) if symbolTable.contains(LocalVar(base)) =>
              (t, symbolTable.get(LocalVar(base)).map(x => s"$x${t.stripPrefix(base)}"))
            case _ => (t, Set(t))
          }
        }
        .flatMap {
          case (t, ts) if Set(t) == ts => Set(t)
          case (_, ts)                 => ts.map(_.replaceAll(s"\\.(?!js:${Defines.Program})", pathSep))
        }
      p match {
        case _: MethodParameterIn                         => symbolTable.put(p, resolvedHints)
        case _: MethodReturn if resolvedHints.sizeIs == 1 =>
          builder.setNodeProperty(p, PropertyNames.TypeFullName, resolvedHints.head)
        case _: MethodReturn =>
          builder.setNodeProperty(p, PropertyNames.TypeFullName, Defines.Any)
          builder.setNodeProperty(p, PropertyNames.DynamicTypeHintFullName, resolvedHints)
        case _ =>
      }
    }
  }

  private lazy val exportedIdentifiers = cu.method
    .nameExact(Defines.Program)
    .flatMap(_._callViaContainsOut)
    .nameExact(Operators.assignment)
    .filter(_.code.startsWith("exports.*"))
    .argument
    .isIdentifier
    .name
    .toSet

  override protected def isFieldUncached(i: Identifier): Boolean =
    exportedIdentifiers.contains(i.name) || super.isFieldUncached(i)

  override protected def visitIdentifierAssignedToConstructor(i: Identifier, c: Call): Set[String] = {
    val constructorPaths = if (c.methodFullName.endsWith(".alloc")) {
      val newOp       = c.inCall.isAssignment.astSiblings.isCall.nameExact(Defines.OperatorsNew).headOption
      val newChildren = newOp.astChildren.l

      val possibleImportIdentifier = newChildren.isIdentifier.headOption match {
        case Some(id) if GlobalBuiltins.builtins.contains(id.name) => Set(s"__ecma.${id.name}")
        case Some(id)                                              =>
          val typs = symbolTable.get(CallAlias(id.name, Option("this")))
          if (typs.nonEmpty) { newOp.foreach(symbolTable.put(_, typs)) }
          symbolTable.get(id)
        case None => Set.empty[String]
      }
      lazy val possibleConstructorPointer =
        newChildren.astChildren.isFieldIdentifier
          .map(f => CallAlias(f.canonicalName, Option("this")))
          .headOption match {
          // the allocated object is an instance, so it gets the type of the class and not the one of its constructor
          case Some(fi) => symbolTable.get(fi).map(_.stripSuffix(s"$pathSep$ConstructorMethodName"))
          case None     => Set.empty[String]
        }

      if (possibleImportIdentifier.nonEmpty) possibleImportIdentifier
      else if (possibleConstructorPointer.nonEmpty) possibleConstructorPointer
      else Set.empty[String]
    } else (symbolTable.get(c) + c.methodFullName).map(t => t.stripSuffix(".factory"))
    associateTypes(i, constructorPaths)
  }

  /** A class in expression position already evaluates to its constructor type (`Foo:<init>`), so the suffix must not be
    * added a second time when it is instantiated.
    */
  private def withConstructorSuffix(typeFullName: String): String = {
    val suffix = s"$pathSep$ConstructorMethodName"
    if (typeFullName.endsWith(suffix)) typeFullName else s"$typeFullName$suffix"
  }

  override protected def visitIdentifierAssignedToOperator(
    i: Identifier,
    call: Call,
    operation: String
  ): Set[String] = {
    operation match {
      case Defines.OperatorsNew =>
        call.astChildren.l match {
          case ::(fa: Call, ::(id: Identifier, _)) if fa.name == Operators.fieldAccess =>
            val constructorTypes = visitIdentifierAssignedToFieldLoad(id, fa.asInstanceOf[FieldAccess])
            // the identifier holds the allocated instance and thus must not be typed with the constructor
            symbolTable.put(id, symbolTable.get(id).map(_.stripSuffix(s"$pathSep$ConstructorMethodName")))
            symbolTable.append(call, constructorTypes.map(withConstructorSuffix))
          case _ => Set.empty
        }
      case _ => super.visitIdentifierAssignedToOperator(i, call, operation)
    }
  }

  override protected def getInterproceduralTypes(
    fieldFullName: String,
    fieldName: String,
    globalTypes: Set[String],
    baseTypes: Set[String]
  ): Set[String] = {
    if (symbolTable.contains(LocalVar(fieldName))) {
      symbolTable.get(LocalVar(fieldName))
    } else if (symbolTable.contains(CallAlias(fieldName, Option("this")))) {
      symbolTable.get(CallAlias(fieldName, Option("this")))
    } else {
      super.getInterproceduralTypes(
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

  /** A class (expression) assigned to a field of `this`, e.g. a class field initialized with a class, is handled like
    * the identifier version of it: the field then points to the type.
    */
  override protected def visitAssignmentArguments(args: List[AstNode]): Set[String] = args match {
    case List(fa: Call, t: TypeRef) if fa.name == Operators.fieldAccess =>
      fa.argumentOut.l match {
        case List(base: Identifier, field: FieldIdentifier) if base.name == "this" =>
          symbolTable.append(CallAlias(field.canonicalName, Option("this")), Set(t.typeFullName))
        case _ => super.visitAssignmentArguments(args)
      }
    case _ => super.visitAssignmentArguments(args)
  }

  override protected def postSetTypeInformation(): Unit = {
    // often there are "this" identifiers with type hints but this can be set to a type hint if they meet the criteria
    cu.method
      .flatMap(_._identifierViaContainsOut)
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
