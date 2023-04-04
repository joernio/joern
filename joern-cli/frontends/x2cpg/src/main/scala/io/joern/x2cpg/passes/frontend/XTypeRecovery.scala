package io.joern.x2cpg.passes.frontend

import io.joern.x2cpg.Defines
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, Operators, PropertyNames}
import io.shiftleft.passes.CpgPass
import io.shiftleft.semanticcpg.language._
import io.shiftleft.semanticcpg.language.operatorextension.OpNodes
import io.shiftleft.semanticcpg.language.operatorextension.OpNodes.{Assignment, FieldAccess}
import org.slf4j.{Logger, LoggerFactory}
import overflowdb.BatchedUpdate
import overflowdb.BatchedUpdate.DiffGraphBuilder
import overflowdb.traversal.Traversal

import java.util.concurrent.RecursiveTask
import java.util.concurrent.atomic.AtomicBoolean
import scala.annotation.nowarn
import scala.collection.concurrent.TrieMap
import scala.collection.mutable

/** @param iterations
  *   the number of iterations to run.
  * @param enabledDummyTypes
  *   whether to enable placeholder dummy values for partially resolved types.
  */
case class XTypeRecoveryConfig(iterations: Int = 2, enabledDummyTypes: Boolean = true)

/** @param config
  *   the user defined config.
  * @param currentIteration
  *   the current iteration.
  * @param isFieldCache
  *   a cache for answering if a node represents a field or member.
  * @param changesWereMade
  *   a flag to indicate that changes were made in the last iteration.
  * @param stopEarly
  *   indicates that we may stop type propagation earlier than the specified number of iterations.
  */
case class XTypeRecoveryState(
  config: XTypeRecoveryConfig = XTypeRecoveryConfig(),
  currentIteration: Int = 0,
  isFieldCache: TrieMap[Long, Boolean] = TrieMap.empty[Long, Boolean],
  changesWereMade: AtomicBoolean = new AtomicBoolean(false),
  stopEarly: AtomicBoolean
) {
  lazy val isFinalIteration: Boolean = currentIteration == config.iterations - 1

  lazy val isFirstIteration: Boolean = currentIteration == 0

  def clear(): Unit = isFieldCache.clear()

}

/** In order to propagate types across compilation units, but avoid the poor scalability of a fixed-point algorithm, the
  * number of iterations can be configured using the iterations parameter. Note that iterations < 2 will not provide any
  * interprocedural type recovery capabilities.
  * @param cpg
  *   the CPG to recovery types for.
  *
  * @tparam CompilationUnitType
  *   the AstNode type used to represent a compilation unit of the language.
  */
abstract class XTypeRecoveryPass[CompilationUnitType <: AstNode](
  cpg: Cpg,
  config: XTypeRecoveryConfig = XTypeRecoveryConfig()
) extends CpgPass(cpg) {

  override def run(builder: BatchedUpdate.DiffGraphBuilder): Unit = {
    val stopEarly = new AtomicBoolean(false)
    val state     = XTypeRecoveryState(config, stopEarly = stopEarly)
    try {
      for (
        i <- 0 until config.iterations
        if !stopEarly.get()
      ) {
        generateRecoveryPass(state.copy(currentIteration = i)).createAndApply()
      }
      // If dummy values are enabled and we are stopping early, we need one more round to propagate these dummy values
      if (stopEarly.get() && config.enabledDummyTypes)
        generateRecoveryPass(state.copy(currentIteration = config.iterations - 1)).createAndApply()
    } finally {
      state.clear()
    }
  }

  protected def generateRecoveryPass(state: XTypeRecoveryState): XTypeRecovery[CompilationUnitType]

}

/** Based on a flow-insensitive static single-assignment symbol-table-style approach. This pass aims to be fast and
  * deterministic and does not try to converge to some fixed point but rather iterates a fixed number of times. This
  * will help recover: <ol><li>Imported call signatures from external dependencies</li><li>Dynamic type hints for
  * mutable variables in a compilation unit.</ol>
  *
  * The algorithm flows roughly as follows: <ol> <li> Scan for method signatures of methods for each compilation unit,
  * either by internally defined methods or by reading import signatures. This includes looking for aliases, e.g. import
  * foo as bar.</li><li>(Optionally) Prune these method signatures by checking their validity against the
  * CPG.</li><li>Visit assignments to populate where variables are assigned a value to extrapolate its type. Store these
  * values in a local symbol table. If a field is assigned a value, store this in the global table</li><li>Find
  * instances of where these fields and variables are used and update their type information.</li><li>If this variable
  * is the receiver of a call, make sure to set the type of the call accordingly.</li></ol>
  *
  * The symbol tables use the [[SymbolTable]] class to track possible type information. <br> <strong>Note: Local symbols
  * are cleared once a compilation unit is complete. This is to keep memory usage down while maximizing
  * concurrency.</strong>
  *
  * @param cpg
  *   the CPG to recovery types for.
  * @tparam CompilationUnitType
  *   the AstNode type used to represent a compilation unit of the language.
  */
abstract class XTypeRecovery[CompilationUnitType <: AstNode](cpg: Cpg, state: XTypeRecoveryState) extends CpgPass(cpg) {

  override def run(builder: DiffGraphBuilder): Unit = {
    val changesWereMade = compilationUnit
      .map(unit => generateRecoveryForCompilationUnitTask(unit, builder).fork())
      .map(_.get())
      .reduce((a, b) => a || b)
    if (!changesWereMade) state.stopEarly.set(true)
  }

  /** @return
    *   the compilation units as per how the language is compiled. e.g. file.
    */
  def compilationUnit: Traversal[CompilationUnitType]

  /** A factory method to generate a [[RecoverForXCompilationUnit]] task with the given parameters.
    * @param unit
    *   the compilation unit.
    * @param builder
    *   the graph builder.
    * @return
    *   a forkable [[RecoverForXCompilationUnit]] task.
    */
  def generateRecoveryForCompilationUnitTask(
    unit: CompilationUnitType,
    builder: DiffGraphBuilder
  ): RecoverForXCompilationUnit[CompilationUnitType]

}

object XTypeRecovery {

  val DummyReturnType                       = "<returnValue>"
  val DummyMemberLoad                       = "<member>"
  val DummyIndexAccess                      = "<indexAccess>"
  private lazy val DummyTokens: Set[String] = Set(DummyReturnType, DummyMemberLoad, DummyIndexAccess)

  def dummyMemberType(prefix: String, memberName: String, sep: Char = '.'): String =
    s"$prefix$sep$DummyMemberLoad($memberName)"

  /** Scans the type for placeholder/dummy types.
    */
  def isDummyType(typ: String): Boolean = DummyTokens.exists(typ.contains)

}

/** Performs type recovery from the root of a compilation unit level
  *
  * @param cpg
  *   the graph.
  * @param cu
  *   a compilation unit, e.g. file, procedure, type, etc.
  * @param builder
  *   the graph builder
  * @tparam CompilationUnitType
  *   the AstNode type used to represent a compilation unit of the language.
  */
abstract class RecoverForXCompilationUnit[CompilationUnitType <: AstNode](
  cpg: Cpg,
  cu: CompilationUnitType,
  builder: DiffGraphBuilder,
  state: XTypeRecoveryState
) extends RecursiveTask[Boolean] {

  protected val logger: Logger = LoggerFactory.getLogger(getClass)

  /** Stores type information for local structures that live within this compilation unit, e.g. local variables.
    */
  protected val symbolTable = new SymbolTable[LocalKey](SBKey.fromNodeToLocalKey)

  /** The root of the target codebase.
    */
  protected val codeRoot: String = cpg.metaData.root.headOption.getOrElse("") + java.io.File.separator

  /** The delimiter used to separate methods/functions in qualified names.
    */
  protected val pathSep = '.'

  /** New node tracking set.
    */
  protected val addedNodes = mutable.HashSet.empty[(Long, String)]

  /** For tracking members and the type operations that need to be performed. Since these are mostly out of scope
    * locally it helps to track these separately.
    *
    * // TODO: Potentially a new use for a global table or modification to the symbol table?
    */
  protected val newTypesForMembers = mutable.HashMap.empty[Member, Set[String]]

  /** Provides an entrypoint to add known symbols and their possible types.
    */
  protected def prepopulateSymbolTable(): Unit = {
    def getTypes(node: AstNode): Set[String] =
      (node.property(PropertyNames.DYNAMIC_TYPE_HINT_FULL_NAME, Seq.empty) :+ node.property(
        PropertyNames.TYPE_FULL_NAME,
        "ANY"
      )).filterNot(_.toUpperCase.matches("(UNKNOWN|ANY)")).toSet

    (cu.ast.isIdentifier ++ cu.ast.isCall ++ cu.ast.isLocal ++ cu.ast.isParameter)
      .filter(hasTypes)
      .foreach {
        case x: Identifier        => symbolTable.put(x, getTypes(x))
        case x: Call              => symbolTable.put(x, Set(x.methodFullName))
        case x: Local             => symbolTable.put(x, getTypes(x))
        case x: MethodParameterIn => symbolTable.put(x, getTypes(x))
        case _                    =>
      }
  }

  private def hasTypes(node: AstNode): Boolean = node match {
    case x: Identifier =>
      x.dynamicTypeHintFullName.nonEmpty || !x.typeFullName.toUpperCase.matches("(UNKNOWN|ANY)")
    case x: Call if !x.methodFullName.startsWith("<operator>") =>
      !x.methodFullName.toLowerCase().matches("(<unknownfullname>|any)")
    case x: Local => x.dynamicTypeHintFullName.nonEmpty || !x.typeFullName.toUpperCase.matches("(UNKNOWN|ANY)")
    case x: MethodParameterIn =>
      x.dynamicTypeHintFullName.nonEmpty || !x.typeFullName.toUpperCase.matches("(UNKNOWN|ANY)")
    case _ => false
  }

  protected def assignments: Traversal[Assignment] =
    cu.ast.isCall.nameExact(Operators.assignment).map(new OpNodes.Assignment(_))

  protected def members: Traversal[Member] = cu.ast.isMember

  protected def calls: Traversal[Call] = cu.ast.isCall

  protected def parameters: Traversal[MethodParameterIn] = cu.ast.isParameter

  /** For each call that contains the returnValue directive, attempt to replace the return value by the dynamic
    */
  protected def visitCall(call: Call): Unit = {
    symbolTable.get(call).foreach { methodFullName =>
      val index = methodFullName.indexOf(XTypeRecovery.DummyReturnType)
      if (index != -1) {
        val methodToLookup = methodFullName.substring(0, index - 1)
        val remainder      = methodFullName.substring(index + XTypeRecovery.DummyReturnType.length)
        val methods        = cpg.method.fullNameExact(methodToLookup).l
        methods match {
          case method :: Nil =>
            val dynamicTypeHints = method.methodReturn.dynamicTypeHintFullName.toSet
            val newTypes         = dynamicTypeHints.map(x => s"$x$remainder")
            symbolTable.put(call, newTypes)
          case Nil =>
          case _ =>
            logger.warn(s"More than a single function matches method full name: $methodToLookup")
        }
      }
    }
  }

  protected def visitParameter(param: MethodParameterIn): Unit = {
    if (param.dynamicTypeHintFullName.nonEmpty) {
      val matchingIdentifiers = param.method._identifierViaContainsOut.nameExact(param.name).l
      matchingIdentifiers.foreach { identifier =>
        symbolTable.append(LocalVar(identifier.name), param.dynamicTypeHintFullName.toSet)
      }
    }
  }

  override def compute(): Boolean = try {
    prepopulateSymbolTable()
    // Set known aliases that point to imports for local and external methods/modules
    visitImports(importNodes(cu))
    // Prune import names if the methods exist in the CPG
    postVisitImports()
    // Populate local symbol table with assignments
    assignments.foreach(visitAssignments)
    // Propagate return types
    calls.filter(symbolTable.contains).foreach(visitCall)
    // Propagate parameter types
    parameters.foreach(visitParameter)
    // Persist findings
    setTypeInformation()
    // Return number of changes
    state.changesWereMade.get()
  } finally {
    symbolTable.clear()
  }

  private def debugLocation(n: AstNode): String = {
    val rootPath = cpg.metaData.root.headOption.getOrElse("")
    val fileName = n.file.name.headOption.getOrElse("<unknown>").stripPrefix(rootPath)
    val lineNo   = n.lineNumber.getOrElse("<unknown>")
    s"$fileName#L$lineNo"
  }

  /** Using import information and internally defined procedures, will generate a mapping between how functions and
    * types are aliased and called and themselves.
    *
    * @param procedureDeclarations
    *   imports to types or functions and internally defined methods themselves.
    */
  protected def visitImports(procedureDeclarations: Traversal[AstNode]): Unit = {
    procedureDeclarations.foreach {
      case i: Import => visitImport(i)
      case i: Call   => visitImport(i)
    }
  }

  /** Refers to the declared import information. This is for legacy import notation.
    *
    * @param i
    *   the call that imports entities into this scope.
    */
  @nowarn("cat=unused")
  protected def visitImport(c: Call): Unit = {}

  /** Visits an import and stores references in the symbol table as both an identifier and call.
    */
  protected def visitImport(i: Import): Unit = for {
    entity <- i.importedEntity
    alias  <- i.importedAs
  } {
    symbolTable.append(LocalVar(alias), Set(entity))
    symbolTable.append(CallAlias(alias), Set(entity))
  }

  /** @return
    *   the import nodes of this compilation unit.
    */
  protected def importNodes(cu: AstNode): Traversal[AstNode] = cu.ast.isCall.referencedImports

  /** The initial import setting is over-approximated, so this step checks the CPG for any matches and prunes against
    * these findings. If there are no findings, it will leave the table as is. The latter is significant for external
    * types or methods.
    */
  protected def postVisitImports(): Unit = {}

  /** Using assignment and import information (in the global symbol table), will propagate these types in the symbol
    * table.
    *
    * @param a
    *   assignment call pointer.
    */
  protected def visitAssignments(a: Assignment): Set[String] = {
    a.argumentOut.l match {
      case List(i: Identifier, b: Block)                             => visitIdentifierAssignedToBlock(i, b)
      case List(i: Identifier, c: Call)                              => visitIdentifierAssignedToCall(i, c)
      case List(x: Identifier, y: Identifier)                        => visitIdentifierAssignedToIdentifier(x, y)
      case List(i: Identifier, l: Literal) if state.isFirstIteration => visitIdentifierAssignedToLiteral(i, l)
      case List(i: Identifier, m: MethodRef)                         => visitIdentifierAssignedToMethodRef(i, m)
      case List(i: Identifier, t: TypeRef)                           => visitIdentifierAssignedToTypeRef(i, t)
      case List(c: Call, i: Identifier)                              => visitCallAssignedToIdentifier(c, i)
      case List(x: Call, y: Call)                                    => visitCallAssignedToCall(x, y)
      case List(c: Call, l: Literal) if state.isFirstIteration       => visitCallAssignedToLiteral(c, l)
      case List(c: Call, m: MethodRef)                               => visitCallAssignedToMethodRef(c, m)
      case List(c: Call, b: Block)                                   => visitCallAssignedToBlock(c, b)
      case _                                                         => Set.empty
    }
  }

  /** Visits an identifier being assigned to the result of some operation.
    */
  protected def visitIdentifierAssignedToBlock(i: Identifier, b: Block): Set[String] = {
    val blockTypes = visitStatementsInBlock(b)
    if (blockTypes.nonEmpty) associateTypes(i, blockTypes)
    else Set.empty
  }

  /** Visits a call operation being assigned to the result of some operation.
    */
  protected def visitCallAssignedToBlock(c: Call, b: Block): Set[String] = {
    val blockTypes = visitStatementsInBlock(b)
    assignTypesToCall(c, blockTypes)
  }

  /** Process each statement but only assign the type of the last statement to the identifier
    */
  protected def visitStatementsInBlock(b: Block): Set[String] =
    b.astChildren
      .map {
        case x: Call if x.name.equals(Operators.assignment)                => visitAssignments(new Assignment(x))
        case x: Identifier if symbolTable.contains(x)                      => symbolTable.get(x)
        case x: Call if symbolTable.contains(x)                            => symbolTable.get(x)
        case x: Call if x.argument.headOption.exists(symbolTable.contains) => setCallMethodFullNameFromBase(x)
        case x: Block                                                      => visitStatementsInBlock(x)
        case x: Local                                                      => symbolTable.get(x)
        case _: ControlStructure                                           => Set.empty[String]
        case x => logger.debug(s"Unhandled block element ${x.label}:${x.code} @ ${debugLocation(x)}"); Set.empty[String]
      }
      .lastOption
      .getOrElse(Set.empty[String])

  /** Visits an identifier being assigned to a call. This call could be an operation, function invocation, or
    * constructor invocation.
    */
  protected def visitIdentifierAssignedToCall(i: Identifier, c: Call): Set[String] = {
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
    } else {
      // We can try obtain a return type for this call
      visitIdentifierAssignedToCallRetVal(i, c)
    }
  }

  /** Visits an identifier being assigned to the value held by another identifier. This is a weak copy.
    */
  protected def visitIdentifierAssignedToIdentifier(x: Identifier, y: Identifier): Set[String] =
    if (symbolTable.contains(y)) associateTypes(x, symbolTable.get(y))
    else Set.empty

  /** Will build a call full path using the call base node. This method assumes the base node is in the symbol table.
    */
  protected def setCallMethodFullNameFromBase(c: Call): Set[String] = {
    val recTypes  = c.argument.headOption.map(symbolTable.get).getOrElse(Set.empty[String])
    val callTypes = recTypes.map(_.concat(s"$pathSep${c.name}"))
    symbolTable.append(c, callTypes)
  }

  /** A heuristic method to determine if a call is a constructor or not.
    */
  protected def isConstructor(c: Call): Boolean

  /** A heuristic method to determine if an identifier may be a field or not. The result means that it would be stored
    * in the global symbol table. By default this checks if the identifier name matches a member name.
    *
    * This has found to be an expensive operation accessed often so we have memoized this step.
    */
  protected def isField(i: Identifier): Boolean =
    state.isFieldCache.getOrElseUpdate(i.id(), i.method.typeDecl.member.nameExact(i.name).nonEmpty)

  /** Associates the types with the identifier. This may sometimes be an identifier that should be considered a field
    * which this method uses [[isField]] to determine.
    */
  protected def associateTypes(i: Identifier, types: Set[String]): Set[String] =
    symbolTable.append(i, types)

  /** Returns the appropriate field parent scope.
    */
  protected def getFieldParents(fa: FieldAccess): Set[String] = {
    val fieldName = getFieldName(fa).split(pathSep).last
    cpg.typeDecl.where(_.member.nameExact(fieldName)).fullName.filterNot(_.contains("ANY")).toSet
  }

  /** Associates the types with the identifier. This may sometimes be an identifier that should be considered a field
    * which this method uses [[isField]] to determine.
    */
  protected def associateTypes(symbol: LocalVar, fa: FieldAccess, types: Set[String]): Set[String] = {
    fa.astChildren.filterNot(_.code.matches("(this|self)")).headOption.collect {
      case fi: FieldIdentifier =>
        getFieldParents(fa).foreach(t => persistMemberWithTypeDecl(t, fi.canonicalName, types))
      case i: Identifier if isField(i) =>
        getFieldParents(fa).foreach(t => persistMemberWithTypeDecl(t, i.name, types))
    }
    symbolTable.append(symbol, types)
  }

  /** Similar to associateTypes but used in the case where there is some kind of field load.
    */
  protected def associateInterproceduralTypes(
    i: Identifier,
    base: Identifier,
    fi: FieldIdentifier,
    fieldName: String,
    baseTypes: Set[String]
  ): Set[String] = {
    val globalTypes = getFieldBaseType(base, fi)
    associateInterproceduralTypes(i, fieldName, fi.canonicalName, globalTypes, baseTypes)
  }

  protected def associateInterproceduralTypes(
    i: Identifier,
    fieldFullName: String,
    fieldName: String,
    globalTypes: Set[String],
    baseTypes: Set[String]
  ): Set[String] = {
    if (globalTypes.nonEmpty) {
      // We have been able to resolve the type inter-procedurally
      associateTypes(i, globalTypes)
    } else if (baseTypes.nonEmpty) {
      if (baseTypes.equals(symbolTable.get(LocalVar(fieldFullName)))) {
        associateTypes(i, baseTypes)
      } else {
        // If not available, use a dummy variable that can be useful for call matching
        associateTypes(i, baseTypes.map(t => XTypeRecovery.dummyMemberType(t, fieldName, pathSep)))
      }
    } else {
      // Assign dummy
      val dummyTypes = Set(
        XTypeRecovery.dummyMemberType(fieldFullName.stripSuffix(s"$pathSep$fieldName"), fieldName, pathSep)
      )
      associateTypes(i, dummyTypes)
    }
  }

  /** Visits an identifier being assigned to an operator call.
    */
  protected def visitIdentifierAssignedToOperator(i: Identifier, c: Call, operation: String): Set[String] = {
    operation match {
      case Operators.alloc       => visitIdentifierAssignedToConstructor(i, c)
      case Operators.fieldAccess => visitIdentifierAssignedToFieldLoad(i, new FieldAccess(c))
      case Operators.indexAccess => visitIdentifierAssignedToIndexAcess(i, c)
      case x                     => logger.debug(s"Unhandled operation $x (${c.code}) @ ${debugLocation(c)}"); Set.empty
    }
  }

  /** Visits an identifier being assigned to a constructor and attempts to speculate the constructor path.
    */
  protected def visitIdentifierAssignedToConstructor(i: Identifier, c: Call): Set[String] = {
    val constructorPaths = symbolTable.get(c).map(t => t.concat(s"$pathSep${Defines.ConstructorMethodName}"))
    associateTypes(i, constructorPaths)
  }

  /** Visits an identifier being assigned to a call's return value.
    */
  protected def visitIdentifierAssignedToCallRetVal(i: Identifier, c: Call): Set[String] = {
    if (symbolTable.contains(c)) {
      val callReturns = methodReturnValues(symbolTable.get(c).toSeq)
      associateTypes(i, callReturns)
    } else if (c.argument.exists(_.argumentIndex == 0)) {
      val callFullNames = (c.argument(0) match {
        case i: Identifier if symbolTable.contains(LocalVar(i.name))  => symbolTable.get(LocalVar(i.name))
        case i: Identifier if symbolTable.contains(CallAlias(i.name)) => symbolTable.get(CallAlias(i.name))
        case _                                                        => Set.empty
      }).map(_.concat(s"$pathSep${c.name}")).toSeq
      val callReturns = methodReturnValues(callFullNames)
      associateTypes(i, callReturns)
    } else {
      // Assign dummy value
      associateTypes(i, Set(s"${c.name}$pathSep${XTypeRecovery.DummyReturnType}"))
    }
  }

  /** Will attempt to find the return values of a method if in the CPG, otherwise will give a dummy value.
    */
  protected def methodReturnValues(methodFullNames: Seq[String]): Set[String] = {
    val rs = cpg.method.fullNameExact(methodFullNames: _*).methodReturn.typeFullName.filterNot(_.equals("ANY")).toSet
    if (rs.isEmpty) methodFullNames.map(_.concat(s"$pathSep${XTypeRecovery.DummyReturnType}")).toSet
    else rs
  }

  /** Will handle literal value assignments. Override if special handling is required.
    */
  protected def visitIdentifierAssignedToLiteral(i: Identifier, l: Literal): Set[String] =
    associateTypes(i, getLiteralType(l))

  /** Not all frontends populate <code>typeFullName</code> for literals so we allow this to be overridden.
    */
  protected def getLiteralType(l: Literal): Set[String] = Set(l.typeFullName)

  /** Will handle an identifier holding a function pointer.
    */
  protected def visitIdentifierAssignedToMethodRef(
    i: Identifier,
    m: MethodRef,
    rec: Option[String] = None
  ): Set[String] =
    symbolTable.append(CallAlias(i.name, rec), Set(m.methodFullName))

  /** Will handle an identifier holding a type pointer.
    */
  protected def visitIdentifierAssignedToTypeRef(i: Identifier, t: TypeRef, rec: Option[String] = None): Set[String] =
    symbolTable.append(CallAlias(i.name, rec), Set(t.typeFullName))

  /** Visits a call assigned to an identifier. This is often when there are operators involved.
    */
  protected def visitCallAssignedToIdentifier(c: Call, i: Identifier): Set[String] = {
    val rhsTypes = symbolTable.get(i)
    assignTypesToCall(c, rhsTypes)
  }

  /** Visits a call assigned to the return value of a call. This is often when there are operators involved.
    */
  protected def visitCallAssignedToCall(x: Call, y: Call): Set[String] =
    assignTypesToCall(x, getTypesFromCall(y))

  /** Given a call operation, will attempt to retrieve types from it.
    */
  protected def getTypesFromCall(c: Call): Set[String] = c.name match {
    case Operators.fieldAccess        => symbolTable.get(LocalVar(getFieldName(new FieldAccess(c))))
    case _ if symbolTable.contains(c) => symbolTable.get(c)
    case Operators.indexAccess        => getIndexAccessTypes(c)
    case n =>
      logger.debug(s"Unknown RHS call type '$n' @ ${debugLocation(c)}")
      Set.empty[String]
  }

  /** Given a LHS call, will retrieve its symbol to the given types.
    */
  protected def assignTypesToCall(x: Call, types: Set[String]): Set[String] = {
    if (types.nonEmpty) {
      getSymbolFromCall(x) match {
        case (lhs, globalKeys) if globalKeys.nonEmpty =>
          globalKeys.foreach { (fieldVar: FieldPath) =>
            persistMemberWithTypeDecl(fieldVar.compUnitFullName, fieldVar.identifier, types)
          }
          symbolTable.append(lhs, types)
        case (lhs, _) => symbolTable.append(lhs, types)
      }
    } else Set.empty
  }

  /** Will attempt to retrieve index access types otherwise will return dummy value.
    */
  protected def getIndexAccessTypes(ia: Call): Set[String] = indexAccessToCollectionVar(ia) match {
    case Some(cVar) if symbolTable.contains(cVar) =>
      symbolTable.get(cVar)
    case Some(cVar) if symbolTable.contains(LocalVar(cVar.identifier)) =>
      symbolTable.get(LocalVar(cVar.identifier)).map(_.concat(s"$pathSep${XTypeRecovery.DummyIndexAccess}"))
    case _ => Set.empty
  }

  /** Convenience class for transporting field names.
    * @param compUnitFullName
    *   qualified path to base type holding the member.
    * @param identifier
    *   the member name.
    */
  case class FieldPath(compUnitFullName: String, identifier: String)

  /** Tries to identify the underlying symbol from the call operation as it is used on the LHS of an assignment. The
    * second element is a list of any associated global keys if applicable.
    */
  protected def getSymbolFromCall(c: Call): (LocalKey, Set[FieldPath]) = c.name match {
    case Operators.fieldAccess =>
      val fa        = new FieldAccess(c)
      val fieldName = getFieldName(fa)
      (LocalVar(fieldName), getFieldParents(fa).map(fp => FieldPath(fp, fieldName)))
    case Operators.indexAccess => (indexAccessToCollectionVar(c).getOrElse(LocalVar(c.name)), Set.empty)
    case x =>
      logger.debug(s"Using default LHS call name '$x' @ ${debugLocation(c)}")
      (LocalVar(c.name), Set.empty)
  }

  /** Extracts a string representation of the name of the field within this field access.
    */
  protected def getFieldName(fa: FieldAccess, prefix: String = "", suffix: String = ""): String = {
    def wrapName(n: String) = {
      val sb = new mutable.StringBuilder()
      if (prefix.nonEmpty) sb.append(s"$prefix$pathSep")
      sb.append(n)
      if (suffix.nonEmpty) sb.append(s"$pathSep$suffix")
      sb.toString()
    }

    fa.astChildren.l match {
      case List(i: Identifier, f: FieldIdentifier) if i.name.matches("(self|this)") => wrapName(f.canonicalName)
      case List(i: Identifier, f: FieldIdentifier) => wrapName(s"${i.name}$pathSep${f.canonicalName}")
      case List(c: Call, f: FieldIdentifier) if c.name.equals(Operators.fieldAccess) =>
        wrapName(getFieldName(new FieldAccess(c), suffix = f.canonicalName))
      case List(c: Call, f: FieldIdentifier) if getTypesFromCall(c).nonEmpty =>
        // TODO: Handle this case better
        wrapName(s"${getTypesFromCall(c).head}$pathSep${f.canonicalName}")
      case List(f: FieldIdentifier, c: Call) if c.name.equals(Operators.fieldAccess) =>
        wrapName(getFieldName(new FieldAccess(c), prefix = f.canonicalName))
      case List(c: Call, f: FieldIdentifier) =>
        // TODO: Handle this case better
        val callCode = if (c.code.contains("(")) c.code.substring(c.code.indexOf("(")) else c.code
        XTypeRecovery.dummyMemberType(callCode, f.canonicalName, pathSep)
      case xs =>
        logger.warn(s"Unhandled field structure ${xs.map(x => (x.label, x.code)).mkString(",")} @ ${debugLocation(fa)}")
        wrapName("<unknown>")
    }
  }

  protected def visitCallAssignedToLiteral(c: Call, l: Literal): Set[String] = {
    if (c.name.equals(Operators.indexAccess)) {
      // For now, we will just handle this on a very basic level
      c.argumentOut.l match {
        case List(_: Identifier, _: Literal) =>
          indexAccessToCollectionVar(c).map(cv => symbolTable.append(cv, getLiteralType(l))).getOrElse(Set.empty)
        case List(_: Identifier, idx: Identifier) if symbolTable.contains(idx) =>
          // Imprecise but sound!
          indexAccessToCollectionVar(c).map(cv => symbolTable.append(cv, symbolTable.get(idx))).getOrElse(Set.empty)
        case List(i: Identifier, c: Call) =>
          // This is an expensive level of precision to support
          symbolTable.append(CollectionVar(i.name, "*"), getTypesFromCall(c))
        case List(c: Call, l: Literal) => assignTypesToCall(c, getLiteralType(l))
        case xs =>
          logger.debug(
            s"Unhandled index access point assigned to literal ${xs.map(x => (x.label, x.code)).mkString(",")} @ ${debugLocation(c)}"
          )
          Set.empty
      }
    } else if (c.name.equals(Operators.fieldAccess)) {
      val fa        = new FieldAccess(c)
      val fieldName = getFieldName(fa)
      associateTypes(LocalVar(fieldName), fa, getLiteralType(l))
    } else {
      logger.warn(s"Unhandled call assigned to literal point ${c.name} @ ${debugLocation(c)}")
      Set.empty
    }
  }

  /** Handles a call operation assigned to a method/function pointer.
    */
  protected def visitCallAssignedToMethodRef(c: Call, m: MethodRef): Set[String] =
    assignTypesToCall(c, Set(m.methodFullName))

  /** Generates an identifier for collection/index-access operations in the symbol table.
    */
  protected def indexAccessToCollectionVar(c: Call): Option[CollectionVar] = {
    def callName(x: Call) =
      if (x.name.equals(Operators.fieldAccess))
        getFieldName(new FieldAccess(x))
      else if (x.name.equals(Operators.indexAccess))
        indexAccessToCollectionVar(x)
          .map(cv => s"${cv.identifier}[${cv.idx}]")
          .getOrElse(XTypeRecovery.DummyIndexAccess)
      else x.name

    Option(c.argumentOut.l match {
      case List(i: Identifier, idx: Literal)    => CollectionVar(i.name, idx.code)
      case List(i: Identifier, idx: Identifier) => CollectionVar(i.name, idx.code)
      case List(c: Call, idx: Call)             => CollectionVar(callName(c), callName(idx))
      case List(c: Call, idx: Literal)          => CollectionVar(callName(c), idx.code)
      case List(c: Call, idx: Identifier)       => CollectionVar(callName(c), idx.code)
      case xs =>
        logger.debug(s"Unhandled index access ${xs.map(x => (x.label, x.code)).mkString(",")} @ ${debugLocation(c)}")
        null
    })
  }

  /** Will handle an identifier being assigned to a field value.
    */
  protected def visitIdentifierAssignedToFieldLoad(i: Identifier, fa: FieldAccess): Set[String] = {
    val fieldName = getFieldName(fa)
    fa.astChildren.l match {
      case List(base: Identifier, fi: FieldIdentifier) if symbolTable.contains(LocalVar(base.name)) =>
        // Get field from global table if referenced as a variable
        val localTypes = symbolTable.get(LocalVar(base.name))
        associateInterproceduralTypes(i, base, fi, fieldName, localTypes)
      case List(base: Identifier, fi: FieldIdentifier) if symbolTable.contains(LocalVar(fieldName)) =>
        val localTypes = symbolTable.get(LocalVar(fieldName))
        associateInterproceduralTypes(i, base, fi, fieldName, localTypes)
      case List(base: Identifier, fi: FieldIdentifier) =>
        val dummyTypes = Set(s"$fieldName$pathSep${XTypeRecovery.DummyReturnType}")
        associateInterproceduralTypes(i, base, fi, fieldName, dummyTypes)
      case List(c: Call, f: FieldIdentifier) if c.name.equals(Operators.fieldAccess) =>
        val baseName = getFieldName(new FieldAccess(c))
        // Build type regardless of length
        // TODO: This is more prone to giving dummy values as it does not do global look-ups
        //  but this is okay for now
        val buf = mutable.ArrayBuffer.empty[String]
        for (segment <- baseName.split(pathSep) ++ Array(f.canonicalName)) {
          val types =
            if (buf.isEmpty) symbolTable.get(LocalVar(segment))
            else buf.flatMap(t => symbolTable.get(LocalVar(s"$t$pathSep$segment"))).toSet
          if (types.nonEmpty) {
            buf.clear()
            buf.addAll(types)
          } else {
            val bufCopy = Array.from(buf)
            buf.clear()
            bufCopy.foreach(t => buf.addOne(XTypeRecovery.dummyMemberType(t, segment, pathSep)))
          }
        }
        associateTypes(i, buf.toSet)
      case List(call: Call, f: FieldIdentifier) =>
        assignTypesToCall(
          call,
          Set(fieldName.stripSuffix(s"${XTypeRecovery.DummyMemberLoad}$pathSep${f.canonicalName}"))
        )
      case _ =>
        logger.warn(s"Unable to assign identifier '${i.name}' to field load '$fieldName' @ ${debugLocation(i)}")
        Set.empty
    }
  }

  /** Visits an identifier being assigned to the result of an index access operation.
    */
  protected def visitIdentifierAssignedToIndexAcess(i: Identifier, c: Call): Set[String] = {
    val iaTypes = getTypesFromCall(c)
    if (iaTypes.nonEmpty) associateTypes(i, iaTypes)
    else Set.empty
  }

  protected def getFieldBaseType(base: Identifier, fi: FieldIdentifier): Set[String] =
    getFieldBaseType(base.name, fi.canonicalName)

  protected def getFieldBaseType(baseName: String, fieldName: String): Set[String] =
    symbolTable
      .get(LocalVar(baseName))
      .flatMap(t => typeDeclTraversal(t).member.nameExact(fieldName))
      .typeFullNameNot("ANY")
      .flatMap(m => m.typeFullName +: m.dynamicTypeHintFullName)
      .toSet

  /** Using an entry from the symbol table, will queue the CPG modification to persist the recovered type information.
    */
  protected def setTypeInformation(): Unit = {
    cu.ast
      .collect {
        case n: Local      => n
        case n: Expression => n
      }
      .foreach {
        case x: Local if symbolTable.contains(x) =>
          val typs =
            if (state.config.enabledDummyTypes) symbolTable.get(x).toSeq
            else symbolTable.get(x).filterNot(XTypeRecovery.isDummyType).toSeq
          if (typs != nodeExistingTypes(x)) {
            state.changesWereMade.compareAndSet(false, true)
            builder.setNodeProperty(x, PropertyNames.DYNAMIC_TYPE_HINT_FULL_NAME, typs)
          }
        case x: Identifier if symbolTable.contains(x) =>
          setTypeInformationForRecCall(x, x.inCall.headOption, x.inCall.argument.take(2).l)
        case x: Call if symbolTable.contains(x) =>
          val typs =
            if (state.config.enabledDummyTypes) symbolTable.get(x).toSeq
            else symbolTable.get(x).filterNot(XTypeRecovery.isDummyType).toSeq
          if (typs != nodeExistingTypes(x)) {
            state.changesWereMade.compareAndSet(false, true)
            builder.setNodeProperty(x, PropertyNames.DYNAMIC_TYPE_HINT_FULL_NAME, typs)
          }
        case x: Identifier if symbolTable.contains(CallAlias(x.name)) && x.inCall.nonEmpty =>
          setTypeInformationForRecCall(x, x.inCall.headOption, x.inCall.argument.take(2).l)
        case x: Call if x.argument.headOption.exists(symbolTable.contains) =>
          setTypeInformationForRecCall(x, x.inCall.headOption, x.inCall.argument.take(2).l)
        case _ =>
      }
    // Set types in an atomic way
    newTypesForMembers.foreach { case (m, ts) =>
      if (ts.toSeq != nodeExistingTypes(m)) {
        state.changesWereMade.compareAndSet(false, true)
        if (ts.size == 1) builder.setNodeProperty(m, PropertyNames.TYPE_FULL_NAME, ts.head)
        else builder.setNodeProperty(m, PropertyNames.DYNAMIC_TYPE_HINT_FULL_NAME, ts)
      }
    }
  }

  /** TODO: Cleaning up using visitor patten
    */
  private def setTypeInformationForRecCall(x: AstNode, n: Option[Call], ms: List[AstNode]): Unit =
    (n, ms) match {
      // Case 1: 'call' is an assignment from some dynamic dispatch call
      case (Some(call: Call), List(i: Identifier, c: Call)) if call.name.equals(Operators.assignment) =>
        val idTypes   = if (symbolTable.contains(i)) symbolTable.get(i) else symbolTable.get(CallAlias(i.name))
        val callTypes = symbolTable.get(c)
        persistType(call, callTypes)
        if (idTypes.nonEmpty || callTypes.nonEmpty) {
          if (idTypes.equals(callTypes))
            // Case 1.1: This is a function pointer or constructor
            persistType(i, callTypes)
          else
            // Case 1.2: This is the return value of the function
            persistType(i, idTypes)
        }
      // Case 1: 'call' is an assignment from some other data structure
      case (Some(call: Call), ::(i: Identifier, _)) if call.name.equals(Operators.assignment) =>
        val idHints = symbolTable.get(i)
        persistType(i, idHints)
        persistType(call, idHints)
      // Case 2: 'i' is the receiver of 'call'
      case (Some(call: Call), ::(i: Identifier, _)) if !call.name.equals(Operators.fieldAccess) =>
        val idHints   = symbolTable.get(i)
        val callTypes = symbolTable.get(call)
        persistType(i, idHints)
        if (callTypes.isEmpty) {
          // For now, calls are treated as function pointers and thus the type should point to the method
          persistType(call, idHints.map(t => s"$t$pathSep${call.name}"))
        } else {
          persistType(call, callTypes)
        }
      // Case 3: 'i' is the receiver for a field access on member 'f'
      case (Some(fieldAccess: Call), List(i: Identifier, f: FieldIdentifier))
          if fieldAccess.name.equals(Operators.fieldAccess) =>
        val iTypes = if (symbolTable.contains(i)) symbolTable.get(i) else symbolTable.get(CallAlias(i.name))
        val cTypes = symbolTable.get(fieldAccess)
        persistType(i, iTypes)
        persistType(fieldAccess, cTypes)
        Traversal.from(fieldAccess.astParent).isCall.headOption match {
          case Some(callFromFieldName) if symbolTable.contains(callFromFieldName) =>
            persistType(callFromFieldName, symbolTable.get(callFromFieldName))
          case Some(callFromFieldName) if iTypes.nonEmpty =>
            persistType(callFromFieldName, iTypes.map(it => s"$it$pathSep${f.canonicalName}"))
          case _ =>
        }
        // This field may be a function pointer
        handlePotentialFunctionPointer(fieldAccess, iTypes, f.canonicalName, f.argumentIndex, Option(i.name))
      case _ => persistType(x, symbolTable.get(x))
    }

  /** In the case this field access is a function pointer, we would want to make sure this has a method ref.
    */
  private def handlePotentialFunctionPointer(
    funcPtr: Expression,
    baseTypes: Set[String],
    funcName: String,
    argIdx: Int,
    baseName: Option[String] = None
  ): Unit = {
    // Sometimes the function identifier is an argument to the call itself as a "base". In this case we don't need
    // a method ref. This happens in jssrc2cpg
    if (funcPtr.astParent.collectAll[Call].exists(_.name == funcName)) return

    baseTypes
      .map(t => if (t.endsWith(funcName)) t else s"$t$pathSep$funcName")
      .flatMap(p => cpg.method.fullNameExact(p))
      .map { m =>
        (
          m,
          NewMethodRef()
            .code(s"${baseName.map(_.appended(pathSep)).getOrElse("")}$funcName")
            .methodFullName(m.fullName)
            .argumentIndex(argIdx + 1)
            .lineNumber(funcPtr.lineNumber)
            .columnNumber(funcPtr.columnNumber)
        )
      }
      .filterNot { case (_, mRef) =>
        addedNodes.contains((funcPtr.id(), s"${mRef.label()}$pathSep${mRef.methodFullName}"))
      }
      .foreach { case (m, mRef) =>
        funcPtr.astParent
          .filterNot(_.astChildren.isMethodRef.methodFullNameExact(mRef.methodFullName).nonEmpty)
          .foreach { inCall =>
            state.changesWereMade.compareAndSet(false, true)
            builder.addNode(mRef)
            builder.addEdge(mRef, m, EdgeTypes.REF)
            builder.addEdge(inCall, mRef, EdgeTypes.AST)
            builder.addEdge(inCall, mRef, EdgeTypes.ARGUMENT)
            mRef.argumentIndex(inCall.astChildren.size)
          }
        addedNodes.add((funcPtr.id(), s"${mRef.label()}$pathSep${mRef.methodFullName}"))
      }
  }

  private def persistType(x: StoredNode, types: Set[String]): Unit = {
    val filteredTypes = if (state.config.enabledDummyTypes) types else types.filterNot(XTypeRecovery.isDummyType)
    if (filteredTypes.nonEmpty) {
      storeNodeTypeInfo(x, filteredTypes.toSeq)
      x match {
        case i: Identifier if symbolTable.contains(i) =>
          if (isField(i)) persistMemberType(i, filteredTypes)
          handlePotentialFunctionPointer(i, filteredTypes, i.name, i.argumentIndex)
        case _ =>
      }
    }
  }

  private def persistMemberType(i: Identifier, types: Set[String]): Unit = {
    getLocalMember(i) match {
      case Some(m) => storeNodeTypeInfo(m, types.toSeq)
      case None    =>
    }
  }

  /** Type decls where member access are required need to point to the correct type decl that holds said members. This
    * allows implementations to use the type names to find the correct type holding members.
    * @param typeFullName
    *   the type full name.
    * @return
    *   the type full name that has member children.
    */
  protected def typeDeclTraversal(typeFullName: String): Traversal[TypeDecl] = cpg.typeDecl.fullNameExact(typeFullName)

  /** Given a type full name and member name, will persist the given types to the member.
    * @param typeFullName
    *   the type full name.
    * @param memberName
    *   the member name.
    * @param types
    *   the types to associate.
    */
  protected def persistMemberWithTypeDecl(typeFullName: String, memberName: String, types: Set[String]): Unit =
    typeDeclTraversal(typeFullName).member.nameExact(memberName).headOption.foreach { m =>
      storeNodeTypeInfo(m, types.toSeq)
    }

  /** Given an identifier that has been determined to be a field, an attempt is made to get the corresponding member.
    * This implementation follows more the way dynamic languages define method/type relations.
    * @param i
    *   the identifier.
    * @return
    *   the corresponding member, if found
    */
  protected def getLocalMember(i: Identifier): Option[Member] =
    typeDeclTraversal(i.method.typeDecl.fullName.headOption.getOrElse(i.method.fullName)).member
      .nameExact(i.name)
      .headOption

  private def storeNodeTypeInfo(storedNode: StoredNode, types: Seq[String]): Unit = {
    lazy val existingTypes = nodeExistingTypes(storedNode)

    if (types.nonEmpty && types != existingTypes) {
      storedNode match {
        case m: Member =>
          // To avoid overwriting member updates, we store them elsewhere until the end
          newTypesForMembers.updateWith(m) {
            case Some(ts) => Some(ts ++ types)
            case None     => Some(types.toSet)
          }
        case n =>
          state.changesWereMade.compareAndSet(false, true)
          if (types.size == 1) builder.setNodeProperty(n, PropertyNames.TYPE_FULL_NAME, types.head)
          else builder.setNodeProperty(n, PropertyNames.DYNAMIC_TYPE_HINT_FULL_NAME, types)
      }
    }
  }

  private def nodeExistingTypes(storedNode: StoredNode): Seq[String] = (storedNode.property(
    PropertyNames.TYPE_FULL_NAME,
    "ANY"
  ) +: storedNode.property(PropertyNames.DYNAMIC_TYPE_HINT_FULL_NAME, Seq.empty)).filterNot(_ == "ANY")

}
