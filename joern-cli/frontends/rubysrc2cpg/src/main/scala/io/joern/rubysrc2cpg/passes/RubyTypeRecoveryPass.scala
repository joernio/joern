package io.joern.rubysrc2cpg.passes

import io.joern.x2cpg.Defines as XDefines
import io.joern.x2cpg.passes.frontend.*
import io.joern.x2cpg.passes.frontend.ImportsPass.ResolvedTypeDecl
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*
import overflowdb.BatchedUpdate.DiffGraphBuilder

import java.util.concurrent.ExecutorService

class RubyTypeRecoveryPass(cpg: Cpg, config: TypeRecoveryConfig = TypeRecoveryConfig())
    extends XTypeRecoveryPass(cpg, config) {
  override protected def generateRecoveryPass(state: TypeRecoveryState, executor: ExecutorService): XTypeRecovery =
    new RubyTypeRecovery(cpg, state, executor)
}

private class RubyTypeRecovery(cpg: Cpg, state: TypeRecoveryState, executor: ExecutorService)
    extends XTypeRecovery(cpg, state, executor) {

  override protected def recoverTypesForProcedure(
    cpg: Cpg,
    procedure: Method,
    initialSymbolTable: SymbolTable[LocalKey],
    builder: DiffGraphBuilder,
    state: TypeRecoveryState
  ): RecoverTypesForProcedure = RecoverForRubyFile(cpg, procedure, initialSymbolTable, builder, state)

  override protected def loadImports(i: ImportsPass.ResolvedImport, symbolTable: SymbolTable[LocalKey]): Unit =
    i match {
      case ResolvedTypeDecl(fullName, alias, _) =>
        symbolTable.append(LocalVar(fullName.split("\\.").lastOption.getOrElse(alias)), fullName)
      case _ => super.loadImports(i, symbolTable)
    }
}

private class RecoverForRubyFile(
  cpg: Cpg,
  procedure: Method,
  symbolTable: SymbolTable[LocalKey],
  builder: DiffGraphBuilder,
  state: TypeRecoveryState
) extends RecoverTypesForProcedure(cpg, procedure, symbolTable, builder, state) {

  /** A heuristic method to determine if a call is a constructor or not.
    */
  override protected def isConstructor(c: Call): Boolean = {
    isConstructor(c.name) && c.code.charAt(0).isUpper
  }

  /** A heuristic method to determine if a call name is a constructor or not.
    */
  override protected def isConstructor(name: String): Boolean =
    !name.isBlank && (name == "new" || name == XDefines.ConstructorMethodName)

  override def visitIdentifierAssignedToConstructor(i: Identifier, c: Call): Set[String] = {

    def isMatching(cName: String, code: String) = {
      val cNameList = cName.split(":program").last.split("\\.").filterNot(_.isEmpty).dropRight(1)
      val codeList  = code.split("\\(").head.split("[:.]").filterNot(_.isEmpty).dropRight(1)
      cNameList sameElements codeList
    }

    val constructorPaths =
      symbolTable.get(c).filter(isMatching(_, c.code)).map(_.stripSuffix(s"$pathSep${XDefines.ConstructorMethodName}"))
    associateTypes(i, constructorPaths)
  }

  override def methodReturnValues(methodFullNames: Seq[String]): Set[String] = {
    // Check if we have a corresponding member to resolve type
    val memberTypes = methodFullNames.flatMap { fullName =>
      val memberName = fullName.split(pathSep).lastOption
      if (memberName.isDefined) {
        val typeDeclFullName = fullName.stripSuffix(s"$pathSep${memberName.get}")
        cpg.typeDecl.fullName(typeDeclFullName).member.nameExact(memberName.get).typeFullName.l
      } else
        List.empty
    }.toSet
    if (memberTypes.nonEmpty) memberTypes else super.methodReturnValues(methodFullNames)
  }

  override def visitIdentifierAssignedToCall(i: Identifier, c: Call): Set[String] = {
    if (c.name.startsWith("<operator>")) {
      visitIdentifierAssignedToOperator(i, c, c.name)
    } else if (symbolTable.contains(c) && isConstructor(c)) {
      visitIdentifierAssignedToConstructor(i, c)
    } else if (symbolTable.contains(c)) {
      visitIdentifierAssignedToCallRetVal(i, c)
    } else if (c.argument.headOption.exists(symbolTable.contains)) {
      setCallMethodFullNameFromBase(c)
      // Repeat this method now that the call has a type
      visitIdentifierAssignedToCall(i, c)
    } else if (
      c.argument.headOption
        .exists(_.isCall) && c.argument.head
        .asInstanceOf[Call]
        .name
        .equals("<operator>.scopeResolution") && c.argument.head
        .asInstanceOf[Call]
        .argument
        .lastOption
        .exists(symbolTable.contains)
    ) {
      setCallMethodFullNameFromBaseScopeResolution(c)
      // Repeat this method now that the call has a type
      visitIdentifierAssignedToCall(i, c)
    } else {
      // We can try obtain a return type for this call
      visitIdentifierAssignedToCallRetVal(i, c)
    }
  }

  private def setCallMethodFullNameFromBaseScopeResolution(c: Call): Set[String] = {
    val recTypes = c.argument.headOption
      .map {
        case x: Call if x.name.equals("<operator>.scopeResolution") =>
          x.argument.lastOption.map(i => symbolTable.get(i)).getOrElse(Set.empty[String])
      }
      .getOrElse(Set.empty[String])
    val callTypes = recTypes.map(_.concat(s"$pathSep${c.name}"))
    symbolTable.append(c, callTypes)
  }

  override protected def visitIdentifierAssignedToTypeRef(
    i: Identifier,
    t: TypeRef,
    rec: Option[String]
  ): Set[String] = {
    val receiver = rec match
      case Some(x) => Option(x)
      case None    => Option("this")
    t.typ.referencedTypeDecl
      .map(_.fullName.stripSuffix("<meta>"))
      .map(td => symbolTable.append(CallAlias(i.name, receiver), Set(td)))
      .headOption
      .getOrElse(super.visitIdentifierAssignedToTypeRef(i, t, receiver))
  }

  override protected def visitIdentifierAssignedToMethodRef(
    i: Identifier,
    m: MethodRef,
    rec: Option[String] = None
  ): Set[String] =
    super.visitIdentifierAssignedToMethodRef(i, m, Option("this"))

}
