package io.joern.jssrc2cpg.passes

import io.joern.x2cpg.Defines as XDefines
import io.joern.x2cpg.Defines.ConstructorMethodName
import io.joern.x2cpg.passes.frontend.*
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.{Operators, PropertyNames}
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.operatorextension.OpNodes.FieldAccess
import overflowdb.BatchedUpdate.DiffGraphBuilder

import java.util.concurrent.ExecutorService

class JavaScriptTypeRecoveryPass(cpg: Cpg, config: TypeRecoveryConfig = TypeRecoveryConfig())
    extends XTypeRecoveryPass(cpg, config) {
  override protected def generateRecoveryPass(state: TypeRecoveryState, executor: ExecutorService): XTypeRecovery =
    new JavaScriptTypeRecovery(cpg, state, executor)
}

private class JavaScriptTypeRecovery(cpg: Cpg, state: TypeRecoveryState, executor: ExecutorService)
    extends XTypeRecovery(cpg, state, executor) {

  override protected def recoverTypesForProcedure(
    cpg: Cpg,
    procedure: Method,
    initialSymbolTable: SymbolTable[LocalKey],
    builder: DiffGraphBuilder,
    state: TypeRecoveryState
  ): RecoverTypesForProcedure =
    RecoverForJavaScriptProcedure(cpg, procedure, initialSymbolTable, builder, state)

}

private class RecoverForJavaScriptProcedure(
  cpg: Cpg,
  procedure: Method,
  symbolTable: SymbolTable[LocalKey],
  builder: DiffGraphBuilder,
  state: TypeRecoveryState
) extends RecoverTypesForProcedure(cpg, procedure, symbolTable, builder, state) {

  import io.joern.x2cpg.passes.frontend.XTypeRecovery.AllNodeTypesFromNodeExt

  override protected val pathSep = ':'

  /** A heuristic method to determine if a call is a constructor or not.
    */
  override protected def isConstructor(c: Call): Boolean = {
    c.name.endsWith("factory") && c.inCall.astParent.headOption.exists(_.isInstanceOf[Block])
  }

  override protected def isConstructor(name: String): Boolean =
    !name.isBlank && (name.charAt(0).isUpper || name.endsWith("factory"))

  override protected def prepopulateSymbolTableEntry(x: AstNode): Unit = x match {
    case x @ (_: Identifier | _: Local | _: MethodParameterIn)
        if x.property(PropertyNames.TYPE_FULL_NAME, Defines.Any) != Defines.Any =>
      val typeFullName = x.property(PropertyNames.TYPE_FULL_NAME, Defines.Any)
      val typeHints    = symbolTable.get(LocalVar(x.property(PropertyNames.TYPE_FULL_NAME, Defines.Any))) - typeFullName
      lazy val cpgTypeFullName = cpg.typeDecl.nameExact(typeFullName).fullName.toSet
      val resolvedTypeHints =
        if (typeHints.nonEmpty) symbolTable.put(x, typeHints)
        else if (cpgTypeFullName.nonEmpty) symbolTable.put(x, cpgTypeFullName)
        else symbolTable.put(x, x.getKnownTypes)
      if (!resolvedTypeHints.contains(typeFullName) && resolvedTypeHints.sizeIs == 1)
        builder.setNodeProperty(x, PropertyNames.TYPE_FULL_NAME, resolvedTypeHints.head)
    case x @ (_: Identifier | _: Local | _: MethodParameterIn) =>
      symbolTable.put(x, x.getKnownTypes)
    case x: Call => symbolTable.put(x, (Seq(x.methodFullName) ++ x.dynamicTypeHintFullName ++ x.possibleTypes).toSet)
    case _       =>
  }

  override protected def prepopulateSymbolTable(): Unit = {
    super.prepopulateSymbolTable()
    procedure.ast.isMethod.foreach(f => symbolTable.put(CallAlias(f.name, Option("this")), Set(f.fullName)))
    (procedure.ast.isParameter.whereNot(_.nameExact("this")) ++ procedure.ast.isMethod.methodReturn)
      .filter(hasTypes)
      .foreach { p =>
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
            case (_, ts)                 => ts.map(_.replaceAll("\\.(?!js::program)", pathSep.toString))
          }
        p match {
          case _: MethodParameterIn => symbolTable.put(p, resolvedHints)
          case _: MethodReturn if resolvedHints.sizeIs == 1 =>
            builder.setNodeProperty(p, PropertyNames.TYPE_FULL_NAME, resolvedHints.head)
          case _: MethodReturn =>
            builder.setNodeProperty(p, PropertyNames.TYPE_FULL_NAME, Defines.Any)
            builder.setNodeProperty(p, PropertyNames.POSSIBLE_TYPES, resolvedHints)
          case _ =>
        }
      }
  }

  private lazy val exportedIdentifiers =
    Iterator(procedure)
      .repeat(_._astIn)(_.emit.until(_.collectAll[Method].nameExact(":program")))
      .collectAll[Method]
      .assignment
      .code("export.*")
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
    if (c.name == "require" || c.code.startsWith("require")) Set.empty
    else if (c.name.endsWith(".factory")) symbolTable.append(i, c.methodFullName.stripSuffix(".factory"))
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
    procedure._identifierViaContainsOut
      .nameExact("this")
      .where(_.typeFullNameExact(Defines.Any))
      .filterNot(_.possibleTypes.isEmpty)
      .foreach(setTypeFromTypeHints)
  }

  protected override def storeIdentifierTypeInfo(i: Identifier, types: Seq[String]): Unit =
    super.storeIdentifierTypeInfo(i, types.map(_.stripSuffix(s"$pathSep${XDefines.ConstructorMethodName}")))

  protected override def storeLocalTypeInfo(i: Local, types: Seq[String]): Unit =
    super.storeLocalTypeInfo(i, types.map(_.stripSuffix(s"$pathSep${XDefines.ConstructorMethodName}")))

}
