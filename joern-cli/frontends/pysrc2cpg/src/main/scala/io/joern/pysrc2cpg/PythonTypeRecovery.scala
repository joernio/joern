package io.joern.pysrc2cpg

import io.joern.x2cpg.passes.frontend.*
import io.joern.x2cpg.passes.frontend.ImportsPass.*
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.{Operators, PropertyNames}
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.operatorextension.OpNodes
import io.shiftleft.semanticcpg.language.operatorextension.OpNodes.FieldAccess
import overflowdb.BatchedUpdate.DiffGraphBuilder

import java.util.concurrent.ExecutorService

class PythonTypeRecoveryPass(cpg: Cpg, config: TypeRecoveryConfig = TypeRecoveryConfig())
    extends XTypeRecoveryPass(cpg, config) {

  override protected def generateRecoveryPass(state: TypeRecoveryState, executor: ExecutorService): XTypeRecovery =
    new PythonTypeRecovery(cpg, state, executor)
}

private class PythonTypeRecovery(cpg: Cpg, state: TypeRecoveryState, executor: ExecutorService)
    extends XTypeRecovery(cpg, state, executor) {

  override val initialSymbolTable: SymbolTable[LocalKey] = SymbolTable[LocalKey](fromNodeToLocalPythonKey)

  override def loadImports(i: ResolvedImport, symbolTable: SymbolTable[LocalKey]): Unit = i match {
    case ResolvedMember(basePath, memberName, alias, _) =>
      val memberTypes = cpg.typeDecl
        .fullNameExact(basePath)
        .member
        .nameExact(memberName)
        .flatMap(m => m.typeFullName +: (m.dynamicTypeHintFullName ++ m.possibleTypes))
        .filterNot(_ == "ANY")
        .toSet
      symbolTable.put(LocalVar(alias), memberTypes)
    case _ => super.loadImports(i, symbolTable)
  }

  /** Replaces the `this` prefix with the Pythonic `self` prefix for instance methods of functions local to this
    * compilation unit.
    */
  private def fromNodeToLocalPythonKey(node: AstNode): Option[LocalKey] =
    node match {
      case n: Method => Option(CallAlias(n.name, Option("self")))
      case _         => SBKey.fromNodeToLocalKey(node)
    }

  override protected def recoverTypesForProcedure(
    cpg: Cpg,
    procedure: Method,
    initialSymbolTable: SymbolTable[LocalKey],
    builder: DiffGraphBuilder,
    state: TypeRecoveryState
  ): RecoverTypesForProcedure =
    RecoverForPythonProcedure(cpg, procedure, initialSymbolTable, builder, state)

}

/** Performs type recovery from the root of a compilation unit level
  */
private class RecoverForPythonProcedure(
  cpg: Cpg,
  procedure: Method,
  symbolTable: SymbolTable[LocalKey],
  builder: DiffGraphBuilder,
  state: TypeRecoveryState
) extends RecoverTypesForProcedure(cpg, procedure, symbolTable, builder, state) {

  override def visitAssignments(a: OpNodes.Assignment): Set[String] = {
    a.argumentOut.l match {
      case List(i: Identifier, c: Call) if c.name.isBlank && c.signature.isBlank =>
        // This is usually some decorator wrapper
        c.argument.isMethodRef.headOption match {
          case Some(mRef) => visitIdentifierAssignedToMethodRef(i, mRef)
          case None       => super.visitAssignments(a)
        }
      case _ => super.visitAssignments(a)
    }
  }

  /** Determines if a function call is a constructor by following the heuristic that Python classes are typically
    * camel-case and start with an upper-case character.
    */
  override def isConstructor(c: Call): Boolean =
    isConstructor(c.name) && c.code.endsWith(")")

  override protected def isConstructor(name: String): Boolean =
    name.nonEmpty && name.charAt(0).isUpper

  /** If the parent method is module then it can be used as a field.
    */
  override def isField(i: Identifier): Boolean =
    state.isFieldCache.getOrElseUpdate(i.id(), i.method.name.matches("(<module>|__init__)") || super.isField(i))

  override def visitIdentifierAssignedToOperator(i: Identifier, c: Call, operation: String): Set[String] = {
    operation match {
      case "<operator>.listLiteral"  => associateTypes(i, Set(s"${PythonAstVisitor.builtinPrefix}list"))
      case "<operator>.tupleLiteral" => associateTypes(i, Set(s"${PythonAstVisitor.builtinPrefix}tuple"))
      case "<operator>.dictLiteral"  => associateTypes(i, Set(s"${PythonAstVisitor.builtinPrefix}dict"))
      case "<operator>.setLiteral"   => associateTypes(i, Set(s"${PythonAstVisitor.builtinPrefix}set"))
      case Operators.conditional     => associateTypes(i, Set(s"${PythonAstVisitor.builtinPrefix}bool"))
      case _                         => super.visitIdentifierAssignedToOperator(i, c, operation)
    }
  }

  override def visitIdentifierAssignedToConstructor(i: Identifier, c: Call): Set[String] = {
    val constructorPaths = symbolTable.get(c).map(_.stripSuffix(s"${pathSep}__init__"))
    associateTypes(i, constructorPaths)
  }

  override def visitIdentifierAssignedToCall(i: Identifier, c: Call): Set[String] = {
    // Ignore legacy import representation
    if (c.name.equals("import")) Set.empty
    // Stop custom annotation representation from hitting superclass
    else if (c.name.isBlank) Set.empty
    else super.visitIdentifierAssignedToCall(i, c)
  }

  override def visitIdentifierAssignedToFieldLoad(i: Identifier, fa: FieldAccess): Set[String] = {
    val fieldParents = getFieldParents(fa)
    fa.astChildren.l match {
      case List(base: Identifier, fi: FieldIdentifier) if base.name.equals("self") && fieldParents.nonEmpty =>
        val referencedFields = cpg.typeDecl.fullNameExact(fieldParents.toSeq: _*).member.nameExact(fi.canonicalName)
        val globalTypes =
          referencedFields.flatMap(m => m.typeFullName +: m.dynamicTypeHintFullName).filterNot(_ == Constants.ANY).toSet
        associateTypes(i, globalTypes)
      case _ => super.visitIdentifierAssignedToFieldLoad(i, fa)
    }
  }

  override def getTypesFromCall(c: Call): Set[String] = c.name match {
    case "<operator>.listLiteral"  => Set(s"${PythonAstVisitor.builtinPrefix}list")
    case "<operator>.tupleLiteral" => Set(s"${PythonAstVisitor.builtinPrefix}tuple")
    case "<operator>.dictLiteral"  => Set(s"${PythonAstVisitor.builtinPrefix}dict")
    case "<operator>.setLiteral"   => Set(s"${PythonAstVisitor.builtinPrefix}set")
    case _                         => super.getTypesFromCall(c)
  }

  override def getFieldParents(fa: FieldAccess): Set[String] = {
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

  private def isPyString(s: String): Boolean =
    (s.startsWith("\"") || s.startsWith("'")) && (s.endsWith("\"") || s.endsWith("'"))

  override def getLiteralType(l: Literal): Set[String] = {
    (l.code match {
      case code if code.toIntOption.isDefined                  => Some(s"${PythonAstVisitor.builtinPrefix}int")
      case code if code.toDoubleOption.isDefined               => Some(s"${PythonAstVisitor.builtinPrefix}float")
      case code if "True".equals(code) || "False".equals(code) => Some(s"${PythonAstVisitor.builtinPrefix}bool")
      case code if code.equals("None")                         => Some(s"${PythonAstVisitor.builtinPrefix}None")
      case code if isPyString(code)                            => Some(s"${PythonAstVisitor.builtinPrefix}str")
      case _                                                   => None
    }).toSet
  }

  override def createCallFromIdentifierTypeFullName(typeFullName: String, callName: String): String = {
    lazy val tName = typeFullName.split("\\.").lastOption.getOrElse(typeFullName)
    typeFullName match {
      case t if t.matches(".*(<\\w+>)$") => super.createCallFromIdentifierTypeFullName(typeFullName, callName)
      case t if t.matches(".*\\.<(member|returnValue|indexAccess)>(\\(.*\\))?") =>
        super.createCallFromIdentifierTypeFullName(typeFullName, callName)
      case t if isConstructor(tName) =>
        Seq(t, callName).mkString(pathSep.toString)
      case _ => super.createCallFromIdentifierTypeFullName(typeFullName, callName)
    }
  }

  override protected def postSetTypeInformation(): Unit =
    procedure.typeDecl
      .map(t => t -> t.inheritsFromTypeFullName.partition(itf => symbolTable.contains(LocalVar(itf))))
      .foreach { case (t, (identifierTypes, otherTypes)) =>
        val existingTypes = (identifierTypes ++ otherTypes).distinct
        val resolvedTypes = identifierTypes.map(LocalVar.apply).flatMap(symbolTable.get)
        if (existingTypes != resolvedTypes && resolvedTypes.nonEmpty) {
          state.changesWereMade.compareAndExchange(false, true)
          builder.setNodeProperty(t, PropertyNames.INHERITS_FROM_TYPE_FULL_NAME, resolvedTypes)
        }
      }

  override def prepopulateSymbolTable(): Unit = {
    procedure.ast.isMethodRef.where(_.astSiblings.isIdentifier.nameExact("classmethod")).referencedMethod.foreach {
      classMethod =>
        classMethod.parameter
          .nameExact("cls")
          .foreach { cls =>
            val clsPath = classMethod.typeDecl.fullName.toSet
            symbolTable.put(LocalVar(cls.name), clsPath)
            if (cls.typeFullName == "ANY")
              builder.setNodeProperty(cls, PropertyNames.POSSIBLE_TYPES, clsPath.toSeq)
          }
    }
    super.prepopulateSymbolTable()
  }

  override protected def visitIdentifierAssignedToTypeRef(i: Identifier, t: TypeRef, rec: Option[String]): Set[String] =
    t.typ.referencedTypeDecl
      .map(_.fullName.stripSuffix("<meta>"))
      .map(td => symbolTable.append(CallAlias(i.name, rec), Set(td)))
      .headOption
      .getOrElse(super.visitIdentifierAssignedToTypeRef(i, t, rec))

  override protected def storeCallTypeInfo(c: Call, types: Seq[String]): Unit =
    super.storeCallTypeInfo(c, types.filterNot(_.startsWith("__builtin.None")))

  override protected def persistType(x: StoredNode, types: Set[String]): Unit = x match {
    case _: Call => super.persistType(x, types.filterNot(_.startsWith("__builtin.None")))
    case _       => super.persistType(x, types.filterNot(_.matches("__builtin\\.None.+")))
  }

  override protected def setTypes(n: StoredNode, types: Seq[String]): Unit = n match {
    case _: Call => super.setTypes(n, types.filterNot(_.startsWith("__builtin.None")))
    case _ => super.setTypes(n, types.filterNot(_.matches("__builtin\\.None.+")))
  }


}
