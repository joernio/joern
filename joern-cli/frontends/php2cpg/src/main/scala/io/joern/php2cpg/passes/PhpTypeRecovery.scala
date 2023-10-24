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

  /** A lookup table for any PHP builtins with known types. The methodReturnValues method will refer to this table when
    * an unknown function is encountered.
    */
  protected val builtinsSymbolTable = Map[String, Set[String]](
    // "strtolower"    -> Set("string"),
  )

  override protected def prepopulateSymbolTable(): Unit = {
    logger.debug(s"prepopulating symbol table")
    super.prepopulateSymbolTable()
    logger.debug(s"symbol table: ${symbolTable.view.mkString(" ")}")
  }

  override protected def prepopulateSymbolTableEntry(x: AstNode): Unit = x match {
    case x: Call =>
      x.methodFullName match {
        case Operators.alloc =>
        case _               => symbolTable.append(x, (x.methodFullName +: x.dynamicTypeHintFullName).toSet)
      }
    case _ => super.prepopulateSymbolTableEntry(x)
  }

  override def isConstructor(c: Call): Boolean =
    isConstructor(c.name) && c.code.endsWith(")")

  override protected def isConstructor(name: String): Boolean =
    !name.isBlank && name.charAt(0).isUpper

  override def assignments: Iterator[Assignment] = {
    logger.debug(
      s"[assignments] assignments: ${cu.ast.isCall.nameExact(Operators.assignment).l.map(a => a.code).mkString("; ")}"
    )
    cu.ast.isCall.nameExact(Operators.assignment).map(new OpNodes.Assignment(_))
  }

  override def compute(): Boolean = {
    logger.debug(s"compute() file: ${cu.file.name.l.mkString(" ")}")
    super.compute()
  }

  override def visitAssignments(a: OpNodes.Assignment): Set[String] = {
    logger.debug(s"visiting assigment: ${a.name}\n- arguments: ${a.argumentOut.l.mkString(" ")}\n- iteration: ${state.currentIteration}")
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
    val constructorPaths = symbolTable.get(c).map(_.stripSuffix(s"${pathSep}<init>"))
    associateTypes(i, constructorPaths)
  }

  override protected def visitIdentifierAssignedToCallRetVal(i: Identifier, c: Call): Set[String] = {
    logger.debug(s"visiting identifier ${i.name} assigned to CallRetVal ${c.name}")

    if (symbolTable.contains(c)) {
      logger.debug(s"- symbol table contains call")
      val callReturns = methodReturnValues(symbolTable.get(c).toSeq)
      associateTypes(i, callReturns)
    } else if (c.argument.exists(_.argumentIndex == 0)) {
      logger.debug(s"- argument index exists")
      val callFullNames = (c.argument(0) match {
        case i: Identifier if symbolTable.contains(LocalVar(i.name))  => symbolTable.get(LocalVar(i.name))
        case i: Identifier if symbolTable.contains(CallAlias(i.name)) => symbolTable.get(CallAlias(i.name))
        case _                                                        => Set.empty
      }).map(_.concat(s"$pathSep${c.name}")).toSeq
      val callReturns = methodReturnValues(callFullNames)
      associateTypes(i, callReturns)
    } else {
      // Check if the CPG already contains type info for this method, otherwise
      // use dummy return value.
      logger.debug(s"- checking CPG")
      val rs = methodReturnValues(Seq(c.methodFullName))
      if (rs.isEmpty) associateTypes(i, Set(s"${c.name}$pathSep${XTypeRecovery.DummyReturnType}"))
      else associateTypes(i, rs)
    }
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
    logger.debug(s"visiting call ${x.name} assigned to call ${y.name}\n- ${x.name}: [${getTypesFromCall(y).mkString(", ")}]")
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

  /** Necessary to change the filter regex from (this|self) to (\\$this|this), in order to account for $this PHP
    * convention.
    *
    * (But need to leave the regular "this"? uncertain)
    */
  override protected def associateTypes(symbol: LocalVar, fa: FieldAccess, types: Set[String]): Set[String] = {
    fa.astChildren.filterNot(_.code.matches("(\\$this|this|self)")).headOption.collect {
      case fi: FieldIdentifier =>
        getFieldParents(fa).foreach(t => persistMemberWithTypeDecl(t, fi.canonicalName, types))
      case i: Identifier if isField(i) =>
        getFieldParents(fa).foreach(t => persistMemberWithTypeDecl(t, i.name, types))
    }
    symbolTable.append(symbol, types)
  }

  /** Reference the PythonTypeRecovery implementation. The XTypeRecovery one seems incorrect.
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

  override protected def persistMemberWithTypeDecl(
    typeFullName: String,
    memberName: String,
    types: Set[String]
  ): Unit = {
    logger.debug(
      s"persisting member with TypeDecl: typeFullName: ${typeFullName}, memberName: ${memberName}, types: [${types.mkString("; ")}]"
    )
    super.persistMemberWithTypeDecl(typeFullName, memberName, types)
  }

  override protected def getTypesFromCall(c: Call): Set[String] = c.name match {
    case Operators.fieldAccess        => symbolTable.get(LocalVar(getFieldName(new FieldAccess(c))))
    case _ if symbolTable.contains(c) => symbolTable.get(c)
    case Operators.indexAccess        => getIndexAccessTypes(c)
    case n => {
      logger.debug(s"Unknown RHS call type '$n' @ ${c.name}\n- looking up in CPG")
      methodReturnValues(Seq(c.methodFullName))
    }
  }

  override protected def indexAccessToCollectionVar(c: Call): Option[CollectionVar] = {
    def callName(x: Call) =
      if (x.name.equals(Operators.fieldAccess))
        getFieldName(new FieldAccess(x))
      else if (x.name.equals(Operators.indexAccess))
        indexAccessToCollectionVar(x)
          .map(cv => s"${cv.identifier}[${cv.idx}]")
          .getOrElse(XTypeRecovery.DummyIndexAccess)
      else x.name

    logger.debug(s"Index access to collection var\n- call name: ${c.name}")

    val collectionVar = Option(c.argumentOut.l match {
      case List(i: Identifier, idx: Literal)    => CollectionVar(i.name, idx.code)
      case List(i: Identifier, idx: Identifier) => CollectionVar(i.name, idx.code)
      case List(c: Call, idx: Call)             => CollectionVar(callName(c), callName(idx))
      case List(c: Call, idx: Literal)          => CollectionVar(callName(c), idx.code)
      case List(c: Call, idx: Identifier)       => CollectionVar(callName(c), idx.code)
      case xs =>
        logger.debug(s"Unhandled index access ${xs.map(x => (x.label, x.code)).mkString(",")} @ ${c.name}")
        null
    })
    logger.debug(s"- collection var: ${collectionVar.mkString}")

    collectionVar
  }

  override protected def storeCallTypeInfo(c: Call, types: Seq[String]): Unit = {
    logger.debug(s"storing call type info: ${c.methodFullName}: [${types.mkString(", ")}]")
    super.storeCallTypeInfo(c, types)
  }

  override protected def assignTypesToCall(x: Call, types: Set[String]): Set[String] = {
    logger.debug(s"assigning types to call: ${x.name}: [${types.mkString(", ")}]")
    if (types.nonEmpty) {
      getSymbolFromCall(x) match {
        case (lhs, globalKeys) if globalKeys.nonEmpty => {
          logger.debug(s"- globalKeys non-empty: lhs: ${lhs.toString()}")
          globalKeys.foreach { (fieldVar: FieldPath) =>
            logger.debug(
              s"- persisting member with type decl: compUnitFullName: ${fieldVar.compUnitFullName}; identifier: ${fieldVar.identifier}; types: ${types
                  .mkString(", ")}"
            )
            persistMemberWithTypeDecl(fieldVar.compUnitFullName, fieldVar.identifier, types)
          }
          symbolTable.append(lhs, types)
        }
        case (lhs, _) => {
          logger.debug(s"- globalKeys empty: lhs: ${lhs.toString()}")
          symbolTable.append(lhs, types)
        }
      }
    } else Set.empty
  }

  override protected def persistType(x: StoredNode, types: Set[String]): Unit = {
    logger.debug(s"persisting type: ${x.label}: [${types.mkString(",")}]")
    super.persistType(x, types)
  }

  override protected def methodReturnValues(methodFullNames: Seq[String]): Set[String] = {
    // Look up methods in existing CPG
    val rs = cpg.method
      .fullNameExact(methodFullNames: _*)
      .methodReturn
      .flatMap(mr => mr.typeFullName +: mr.dynamicTypeHintFullName)
      .filterNot(_.equals("ANY"))
      .filterNot(_.endsWith("alloc.<init>"))
      .filterNot(_.endsWith(s"${XTypeRecovery.DummyReturnType}"))
      .toSet
    logger.debug(s"method return values: ${rs.mkString(",")}")
    if (rs.isEmpty)
      // Look up in builtins table or else return dummy return type
      // TODO: Remove - no longer necessary now that PhpSetKnownTypes is
      // implemented.
      methodFullNames
        .flatMap(m => builtinsSymbolTable.getOrElse(m, Set(m.concat(s"$pathSep${XTypeRecovery.DummyReturnType}"))))
        .toSet
    else rs
  }
}
