package io.joern.x2cpg.passes.frontend

import io.joern.x2cpg.Defines
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.codepropertygraph.generated.{Operators, PropertyNames}
import io.shiftleft.passes.CpgPass
import io.shiftleft.semanticcpg.language._
import io.shiftleft.semanticcpg.language.operatorextension.OpNodes
import io.shiftleft.semanticcpg.language.operatorextension.OpNodes.{Assignment, FieldAccess}
import org.slf4j.{Logger, LoggerFactory}
import overflowdb.BatchedUpdate.DiffGraphBuilder
import overflowdb.traversal.Traversal

import java.util.Objects
import java.util.concurrent.RecursiveTask
import scala.collection.mutable

/** Based on a flow-insensitive symbol-table-style approach. This pass aims to be fast and deterministic and does not
  * try to converge to some fixed point but rather iterates a fixed number of times. This will help recover:
  * <ol><li>Imported call signatures from external dependencies</li><li>Dynamic type hints for mutable variables in a
  * computational unit.</ol>
  *
  * The algorithm flows roughly as follows: <ol> <li> Scan for method signatures of methods for each compilation unit,
  * either by internally defined methods or by reading import signatures. This includes looking for aliases, e.g. import
  * foo as bar.</li><li>(Optionally) Prune these method signatures by checking their validity against the
  * CPG.</li><li>Visit assignments to populate where variables are assigned a value to extrapolate its type. Store these
  * values in a local symbol table. If a field is assigned a value, store this in the global table</li><li>Find
  * instances of where these fields and variables are used and update their type information.</li><li>If this variable
  * is the receiver of a call, make sure to set the type of the call accordingly.</li></ol>
  *
  * In order to propagate types across computational units, but avoid the poor scalability of a fixed-point algorithm,
  * the number of iterations can be configured using the [[iterations]] parameter. Note that [[iterations]] < 2 will not
  * provide any interprocedural type recovery capabilities.
  *
  * The symbol tables use the [[SymbolTable]] class to track possible type information. <br> <strong>Note: Local symbols
  * are cleared once a compilation unit is complete. This is to keep memory usage down while maximizing
  * concurrency.</strong>
  *
  * @param cpg
  *   the CPG to recovery types for.
  * @param iterations
  *   the total number of iterations through which types are to be propagated. At least 2 are recommended in order to
  *   propagate interprocedural types. Think of this as similar to the dataflowengineoss' 'maxCallDepth'.
  * @tparam ComputationalUnit
  *   the [[AstNode]] type used to represent a computational unit of the language.
  */
abstract class XTypeRecovery[ComputationalUnit <: AstNode](cpg: Cpg, iterations: Int = 2) extends CpgPass(cpg) {

  /** Stores type information for global structures that persist across computational units, e.g. field identifiers.
    */
  protected val globalTable = new SymbolTable[GlobalKey](SBKey.fromNodeToGlobalKey)

  override def run(builder: DiffGraphBuilder): Unit = try {
    for (_ <- 0 until iterations)
      computationalUnit
        .map(unit => generateRecoveryForCompilationUnitTask(unit, builder).fork())
        .foreach(_.get())
  } finally {
    globalTable.clear()
  }

  /** @return
    *   the computational units as per how the language is compiled. e.g. file.
    */
  def computationalUnit: Traversal[ComputationalUnit]

  /** A factory method to generate a [[RecoverForXCompilationUnit]] task with the given parameters.
    * @param unit
    *   the compilation unit.
    * @param builder
    *   the graph builder.
    * @return
    *   a forkable [[RecoverForXCompilationUnit]] task.
    */
  def generateRecoveryForCompilationUnitTask(
    unit: ComputationalUnit,
    builder: DiffGraphBuilder
  ): RecoverForXCompilationUnit[ComputationalUnit]

}

object XTypeRecovery {
  val DUMMY_RETURN_TYPE                                     = "<returnValue>"
  val DUMMY_MEMBER_LOAD                                     = "<member>"
  val DUMMY_INDEX_ACCESS                                    = "<indexAccess>"
  def DUMMY_MEMBER_TYPE(prefix: String, memberName: String) = s"$prefix.$DUMMY_MEMBER_LOAD($memberName)"
}

/** Defines how a procedure is available to be called in the current scope either by it being defined in this module or
  * being imported.
  *
  * @param callingName
  *   how this procedure is to be called, i.e., alias name, name with path, etc.
  * @param fullName
  *   the full name to where this method is defined.
  */
abstract class ScopedXProcedure(val callingName: String, val fullName: String, val isConstructor: Boolean = false) {

  /** @return
    *   there are multiple ways that this procedure could be resolved in some languages. This will be pruned later by
    *   comparing this to actual methods in the CPG using postVisitImport.
    */
  def possibleCalleeNames: Set[String] = Set()

  override def toString: String = s"ProcedureCalledAs(${possibleCalleeNames.mkString(", ")})"

  override def equals(obj: Any): Boolean = {
    obj match {
      case o: ScopedXProcedure =>
        callingName.equals(o.callingName) && fullName.equals(o.fullName) && isConstructor == o.isConstructor
      case _ => false
    }
  }

  override def hashCode(): Int = Objects.hash(callingName, fullName, isConstructor)

}

/** Tasks responsible for populating the symbol table with import data.
  *
  * @param node
  *   a node that references import information.
  */
abstract class SetXProcedureDefTask(node: AstNode) extends RecursiveTask[Unit] {

  protected val logger: Logger = LoggerFactory.getLogger(classOf[SetXProcedureDefTask])

  override def compute(): Unit =
    node match {
      case x: Call   => visitImport(x)
      case _         =>
    }

  /** Refers to the declared import information.
    *
    * @param importCall
    *   the call that imports entities into this scope.
    */
  def visitImport(importCall: Call): Unit

}

/** Performs type recovery from the root of a compilation unit level
  *
  * @param cpg
  *   the graph.
  * @param cu
  *   a compilation unit, e.g. file, procedure, type, etc.
  * @param builder
  *   the graph builder
  * @param globalTable
  *   the global symbol table.
  * @tparam ComputationalUnit
  *   the [[AstNode]] type used to represent a computational unit of the language.
  */
abstract class RecoverForXCompilationUnit[ComputationalUnit <: AstNode](
  cpg: Cpg,
  cu: ComputationalUnit,
  builder: DiffGraphBuilder,
  globalTable: SymbolTable[GlobalKey]
) extends RecursiveTask[Unit] {

  protected val logger: Logger = LoggerFactory.getLogger(getClass)

  /** Stores type information for local structures that live within this compilation unit, e.g. local variables.
    */
  protected val symbolTable = new SymbolTable[LocalKey](SBKey.fromNodeToLocalKey)

  /** Provides an entrypoint to add known symbols and their possible types.
    */
  def prepopulateSymbolTable(): Unit = {}

  protected def assignments: Traversal[Assignment] =
    cu.ast.isCall.name(Operators.assignment).map(new OpNodes.Assignment(_))

  protected def members: Traversal[Member] =
    cu.ast.isMember

  override def compute(): Unit = try {
    prepopulateSymbolTable()
    // Set known aliases that point to imports for local and external methods/modules
    setImportsFromDeclaredProcedures(importNodes(cu))
    // Prune import names if the methods exist in the CPG
    postVisitImports()
    // Populate fields
    members.foreach(visitMembers)
    // Populate local symbol table with assignments
    assignments.foreach(visitAssignments)
    // Persist findings
    setTypeInformation()
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
  protected def setImportsFromDeclaredProcedures(procedureDeclarations: Traversal[AstNode]): Unit =
    procedureDeclarations.map(f => generateSetProcedureDefTask(f, symbolTable).fork()).foreach(_.get())

  /** Generates a task to create an import task.
    *
    * @param node
    *   the import node or method definition node.
    * @param symbolTable
    *   the local table.
    * @return
    *   a forkable [[SetXProcedureDefTask]] task.
    */
  protected def generateSetProcedureDefTask(node: AstNode, symbolTable: SymbolTable[LocalKey]): SetXProcedureDefTask

  /** @return
    *   the import nodes of this computational unit.
    */
  protected def importNodes(cu: AstNode): Traversal[AstNode] = cu.ast.isImport

  /** @param cu
    *   the current computational unit.
    * @return
    *   the methods defined within this computational unit.
    */
  protected def internalMethodNodes(cu: AstNode): Traversal[Method] = cu.ast.isMethod.isExternal(false)

  /** The initial import setting is over-approximated, so this step checks the CPG for any matches and prunes against
    * these findings. If there are no findings, it will leave the table as is. The latter is significant for external
    * types or methods.
    */
  protected def postVisitImports(): Unit = {}

  /** Using member information, will propagate member information to the global and local symbol table. By default,
    * fields in the local table will be prepended with "this".
    */
  protected def visitMembers(member: Member): Unit = {
    symbolTable.put(LocalVar(member.name), Set.empty[String])
    globalTable.put(FieldVar(member.typeDecl.fullName, member.name), Set.empty[String])
  }

  /** Using assignment and import information (in the global symbol table), will propagate these types in the symbol
    * table.
    *
    * @param a
    *   assignment call pointer.
    */
  protected def visitAssignments(a: Assignment): Set[String] = {
    a.argumentOut.l match {
      case List(i: Identifier, b: Block)     => visitIdentifierAssignedToBlock(i, b)
      case List(i: Identifier, c: Call)      => visitIdentifierAssignedToCall(i, c)
      case List(i: Identifier, l: Literal)   => visitIdentifierAssignedToLiteral(i, l)
      case List(i: Identifier, m: MethodRef) => visitIdentifierAssignedToMethodRef(i, m)
      case List(i: Identifier, t: TypeRef)   => visitIdentifierAssignedToTypeRef(i, t)
      case List(c: Call, i: Identifier)      => visitCallAssignedToIdentifier(c, i)
      case List(x: Call, y: Call)            => visitCallAssignedToCall(x, y)
      case List(c: Call, l: Literal)         => visitCallAssignedToLiteral(c, l)
      case List(c: Call, m: MethodRef)       => visitCallAssignedToMethodRef(c, m)
      case List(c: Call, b: Block)           => visitCallAssignedToBlock(c, b)
      case xs =>
        logger.warn(s"Unhandled assignment ${xs.map(x => (x.label, x.code)).mkString(",")} @ ${debugLocation(a)}");
        Set.empty
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
        case x: Call if x.name.equals(Operators.assignment)                    => visitAssignments(new Assignment(x))
        case x: Identifier if x.astChildren.isEmpty && symbolTable.contains(x) => symbolTable.get(x)
        case x: Call if symbolTable.contains(x)                                => symbolTable.get(x)
        case x: Call if x.argument.headOption.exists(symbolTable.contains)     => setCallMethodFullNameFromBase(x)
        case x => logger.warn(s"Unhandled block element ${x.label}:${x.code} @ ${debugLocation(x)}"); Set.empty[String]
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

  /** Will build a call full path using the call base node. This method assumes the base node is in the symbol table.
    */
  protected def setCallMethodFullNameFromBase(c: Call): Set[String] = {
    val recTypes  = c.argument.headOption.map(symbolTable.get).getOrElse(Set.empty[String])
    val callTypes = recTypes.map(_.concat(s".${c.name}"))
    symbolTable.append(c, callTypes)
  }

  /** A heuristic method to determine if a call is a constructor or not.
    */
  protected def isConstructor(c: Call): Boolean

  /** A heuristic method to determine if an identifier may be a field or not. The result means that it would be stored
    * in the global symbol table. By default this checks if the identifier name matches a member name.
    */
  protected def isField(i: Identifier): Boolean = i.method.typeDecl.member.exists(_.name.equals(i.name))

  /** Associates the types with the identifier. This may sometimes be an identifier that should be considered a field
    * which this method uses [[isField(i)]] to determine.
    */
  protected def associateTypes(i: Identifier, types: Set[String]): Set[String] = {
    if (isField(i)) globalTable.put(i, types)
    symbolTable.append(i, types)
  }

  /** Returns the appropriate field parent scope.
    */
  protected def getFieldParents(fa: FieldAccess): Set[String] = {
    val fieldName = getFieldName(fa).split("\\.").last
    cpg.typeDecl.filter(_.member.exists(_.name.equals(fieldName))).fullName.filterNot(_.contains("ANY")).toSet
  }

  /** Associates the types with the identifier. This may sometimes be an identifier that should be considered a field
    * which this method uses [[isField(i)]] to determine.
    */
  protected def associateTypes(symbol: LocalVar, fa: FieldAccess, types: Set[String]): Set[String] = {
    val fieldParents = getFieldParents(fa)
    fa.astChildren.l match {
      case ::(i: Identifier, _) if isField(i) && fieldParents.nonEmpty =>
        fieldParents.foreach(fp => globalTable.put(FieldVar(fp, symbol.identifier), types))
      case _ =>
    }
    symbolTable.append(symbol, types)
  }

  /** Similar to [[associateTypes()]] but used in the case where there is some kind of field load.
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
        associateTypes(i, baseTypes.map(t => XTypeRecovery.DUMMY_MEMBER_TYPE(t, fieldName)))
      }
    } else {
      logger.warn(s"Unable to associate interprocedural type for $i = $fieldFullName @ ${debugLocation(i)}")
      Set.empty
    }
  }

  /** Visits an identifier being assigned to an operator call.
    */
  protected def visitIdentifierAssignedToOperator(i: Identifier, c: Call, operation: String): Set[String] = {
    operation match {
      case Operators.alloc       => visitIdentifierAssignedToConstructor(i, c)
      case Operators.fieldAccess => visitIdentifierAssignedToFieldLoad(i, new FieldAccess(c))
      case x                     => logger.warn(s"Unhandled operation $x (${c.code}) @ ${debugLocation(c)}"); Set.empty
    }
  }

  /** Visits an identifier being assigned to a constructor and attempts to speculate the constructor path.
    */
  protected def visitIdentifierAssignedToConstructor(i: Identifier, c: Call): Set[String] = {
    val constructorPaths = symbolTable.get(c).map(t => t.concat(s".${Defines.ConstructorMethodName}"))
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
      }).map(_.concat(s".${c.name}")).toSeq
      val callReturns = methodReturnValues(callFullNames)
      associateTypes(i, callReturns)
    } else {
      logger.warn(s"Unable to assign identifier '${i.name} to return value for ${c.code} @ ${debugLocation(c)}")
      Set.empty
    }
  }

  /** Will attempt to find the return values of a method if in the CPG, otherwise will give a dummy value.
    */
  protected def methodReturnValues(methodFullNames: Seq[String]): Set[String] = {
    val rs = cpg.method.fullNameExact(methodFullNames: _*).methodReturn.typeFullName.filterNot(_.equals("ANY")).toSet
    if (rs.isEmpty) methodFullNames.map(_.concat(s".${XTypeRecovery.DUMMY_RETURN_TYPE}")).toSet
    else rs
  }

  /** Will handle literal value assignments. Override if special handling is required.
    */
  protected def visitIdentifierAssignedToLiteral(i: Identifier, l: Literal): Set[String] =
    associateTypes(i, getLiteralType(l))

  /** Not all frontends populate <code>typeFullName</code> for literals so we allow this to be overriden.
    */
  protected def getLiteralType(l: Literal): Set[String] = Set(l.typeFullName)

  /** Will handle an identifier holding a function pointer.
    */
  protected def visitIdentifierAssignedToMethodRef(i: Identifier, m: MethodRef): Set[String] =
    symbolTable.append(CallAlias(i.name), Set(m.methodFullName))

  /** Will handle an identifier holding a type pointer.
    */
  protected def visitIdentifierAssignedToTypeRef(i: Identifier, t: TypeRef): Set[String] =
    symbolTable.append(CallAlias(i.name), Set(t.typeFullName))

  /** Visits a call assigned to an identifier. This is often when there are operators involved.
    */
  protected def visitCallAssignedToIdentifier(c: Call, i: Identifier): Set[String] = {
    val rhsTypes = symbolTable.get(i)
    assignTypesToCall(c, rhsTypes)
  }

  /** Visits a call assigned to the return value of a call. This is often when there are operators involved.
    */
  protected def visitCallAssignedToCall(x: Call, y: Call): Set[String] = {
    val rhsTypes = y.name match {
      case Operators.fieldAccess        => symbolTable.get(LocalVar(getFieldName(new FieldAccess(y))))
      case _ if symbolTable.contains(y) => symbolTable.get(y)
      case Operators.indexAccess        => getIndexAccessTypes(y)
      case n =>
        logger.warn(s"Unknown RHS call type '$n' @ ${debugLocation(x)}")
        Set.empty[String]
    }
    assignTypesToCall(x, rhsTypes)
  }

  /** Given a LHS call, will retrieve its symbol to the given types.
    */
  protected def assignTypesToCall(x: Call, types: Set[String]): Set[String] = {
    if (types.isEmpty) return Set.empty
    getSymbolFromCall(x) match {
      case (lhs, globalKeys) if globalKeys.nonEmpty =>
        globalKeys.foreach(gt => globalTable.append(gt, types))
        symbolTable.append(lhs, types)
      case (lhs, _) => symbolTable.append(lhs, types)
    }
  }

  /** Will attempt to retrieve index access types otherwise will return dummy value.
    */
  protected def getIndexAccessTypes(ia: Call): Set[String] = {
    indexAccessToCollectionVar(ia) match {
      case Some(cvar) if symbolTable.contains(cvar) =>
        symbolTable.get(cvar)
      case Some(cvar) if symbolTable.contains(LocalVar(cvar.identifier)) =>
        symbolTable.get(LocalVar(cvar.identifier)).map(_.concat(s".${XTypeRecovery.DUMMY_INDEX_ACCESS}"))
      case None => Set.empty
    }
  }

  /** Tries to identify the underlying symbol from the call operation as it is used on the LHS of an assignment. The
    * second element is a list of any associated global keys if applicable.
    */
  protected def getSymbolFromCall(c: Call): (LocalKey, Set[GlobalKey]) = c.name match {
    case Operators.fieldAccess =>
      val fa        = new FieldAccess(c)
      val fieldName = getFieldName(fa)
      (LocalVar(fieldName), getFieldParents(fa).map(fp => FieldVar(fp, fieldName)))
    case Operators.indexAccess => (indexAccessToCollectionVar(c).getOrElse(LocalVar(c.name)), Set.empty)
    case x =>
      logger.warn(s"Unknown LHS call type '$x' @ ${debugLocation(c)}")
      (LocalVar(c.name), Set.empty)
  }

  /** Extracts a string representation of the name of the field within this field access.
    */
  protected def getFieldName(fa: FieldAccess, prefix: String = "", suffix: String = ""): String = {
    def wrapName(n: String) = {
      val sb = new mutable.StringBuilder()
      if (prefix.nonEmpty) sb.append(s"$prefix.")
      sb.append(n)
      if (suffix.nonEmpty) sb.append(s".$suffix")
      sb.toString()
    }

    fa.astChildren.l match {
      case List(i: Identifier, f: FieldIdentifier) if i.name.matches("(self|this)") => wrapName(f.canonicalName)
      case List(i: Identifier, f: FieldIdentifier) => wrapName(s"${i.name}.${f.canonicalName}")
      case List(c: Call, f: FieldIdentifier) if c.name.equals(Operators.fieldAccess) =>
        wrapName(getFieldName(new FieldAccess(c), suffix = f.canonicalName))
      case List(f: FieldIdentifier, c: Call) if c.name.equals(Operators.fieldAccess) =>
        wrapName(getFieldName(new FieldAccess(c), prefix = f.canonicalName))
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
        case xs =>
          logger.warn(s"Unhandled index access point assigned to literal ${xs.map(_.label)} @ ${debugLocation(c)}")
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
          .getOrElse(XTypeRecovery.DUMMY_INDEX_ACCESS)
      else x.name

    Option(c.argumentOut.l match {
      case List(i: Identifier, idx: Literal)    => CollectionVar(i.name, idx.code)
      case List(i: Identifier, idx: Identifier) => CollectionVar(i.name, idx.code)
      case List(c: Call, idx: Call)             => CollectionVar(callName(c), callName(idx))
      case List(c: Call, idx: Literal)          => CollectionVar(callName(c), idx.code)
      case List(c: Call, idx: Identifier)       => CollectionVar(callName(c), idx.code)
      case xs =>
        logger.warn(s"Unhandled index access ${xs.map(x => (x.label, x.code)).mkString(",")} @ ${debugLocation(c)}")
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
      case List(c: Call, f: FieldIdentifier) if c.name.equals(Operators.fieldAccess) =>
        val baseName = getFieldName(new FieldAccess(c))
        // Build type regardless of length
        // TODO: This is more prone to giving dummy values as it does not do global look-ups
        //  but this is okay for now
        val buf = mutable.ArrayBuffer.empty[String]
        for (segment <- baseName.split("\\.") ++ Array(f.canonicalName)) {
          val types =
            if (buf.isEmpty) symbolTable.get(LocalVar(segment))
            else buf.flatMap(t => symbolTable.get(LocalVar(s"$t.$segment"))).toSet
          if (types.nonEmpty) {
            buf.clear()
            buf.addAll(types)
          } else {
            val bufCopy = Array.from(buf)
            buf.clear()
            bufCopy.foreach(t => buf.addOne(XTypeRecovery.DUMMY_MEMBER_TYPE(t, segment)))
          }
        }
        associateTypes(i, buf.toSet)
      case _ =>
        logger.warn(s"Unable to assign identifier '${i.name}' to field load '$fieldName' @ ${debugLocation(i)}")
        Set.empty
    }
  }

  protected def getFieldBaseType(base: Identifier, fi: FieldIdentifier): Set[String] =
    getFieldBaseType(base.name, fi.canonicalName)

  protected def getFieldBaseType(baseName: String, fieldName: String): Set[String] = {
    val localTypes = symbolTable.get(LocalVar(baseName))
    val globalTypes = localTypes
      .map(t => FieldVar(t, fieldName))
      .flatMap(globalTable.get)
    globalTypes
  }

  /** Using an entry from the symbol table, will queue the CPG modification to persist the recovered type information.
    */
  protected def setTypeInformation(): Unit = {
    cu.ast
      .foreach {
        case x: Local if symbolTable.contains(x) =>
          builder.setNodeProperty(x, PropertyNames.DYNAMIC_TYPE_HINT_FULL_NAME, symbolTable.get(x).toSeq)
        case x: Identifier if symbolTable.contains(x) =>
          setTypeInformationForRecCall(x, x.inCall.headOption, x.inCall.argument.take(2).l)
        case x: Call if symbolTable.contains(x) =>
          builder.setNodeProperty(x, PropertyNames.DYNAMIC_TYPE_HINT_FULL_NAME, symbolTable.get(x).toSeq)
        case x: Identifier if symbolTable.contains(CallAlias(x.name)) && x.inCall.nonEmpty =>
          setTypeInformationForRecCall(x, x.inCall.headOption, x.inCall.argument.take(2).l)
        case _ =>
      }
  }

  private def setTypeInformationForRecCall(x: AstNode, n: Option[Call], ms: List[AstNode]): Unit =
    (n, ms) match {
      // Case 1: 'call' is an assignment from some dynamic dispatch call
      case (Some(call: Call), List(i: Identifier, c: Call)) if call.name.equals(Operators.assignment) =>
        val idTypes   = if (symbolTable.contains(i)) symbolTable.get(i) else symbolTable.get(CallAlias(i.name))
        val callTypes = symbolTable.get(c)
        persistType(call, callTypes)(builder)
        if (idTypes.nonEmpty || callTypes.nonEmpty) {
          if (idTypes.equals(callTypes))
            // Case 1.1: This is a function pointer or constructor
            persistType(i, callTypes)(builder)
          else
            // Case 1.2: This is the return value of the function
            persistType(i, idTypes)(builder)
        }
      // Case 1: 'call' is an assignment from some other data structure
      case (Some(call: Call), ::(i: Identifier, _)) if call.name.equals(Operators.assignment) =>
        val idHints = symbolTable.get(i)
        persistType(i, idHints)(builder)
        persistType(call, idHints)(builder)
      // Case 2: 'i' is the receiver of 'call'
      case (Some(call: Call), ::(i: Identifier, _)) if !call.name.equals(Operators.fieldAccess) =>
        val idHints   = symbolTable.get(i)
        val callTypes = symbolTable.get(call)
        persistType(i, idHints)(builder)
        if (callTypes.isEmpty) {
          // For now, calls are treated as function pointers and thus the type should point to the method
          persistType(call, idHints.map(t => s"$t.${call.name}"))(builder)
        } else {
          persistType(call, callTypes)(builder)
        }
      // Case 3: 'i' is the receiver for a field access on member 'f'
      case (Some(call: Call), List(i: Identifier, f: FieldIdentifier)) if call.name.equals(Operators.fieldAccess) =>
        val iTypes = if (symbolTable.contains(i)) symbolTable.get(i) else symbolTable.get(CallAlias(i.name))
        val cTypes = symbolTable.get(call)
        persistType(i, iTypes)(builder)
        persistType(call, cTypes)(builder)
        Traversal.from(call.astParent).isCall.headOption match {
          case Some(callFromFieldName) if symbolTable.contains(callFromFieldName) =>
            persistType(callFromFieldName, symbolTable.get(callFromFieldName))(builder)
          case Some(callFromFieldName) if iTypes.nonEmpty =>
            persistType(callFromFieldName, iTypes.map(it => s"$it.${f.canonicalName}"))(builder)
          case _ =>
        }
      case _ => persistType(x, symbolTable.get(x))(builder)
    }

  private def persistType(x: StoredNode, types: Set[String])(implicit builder: DiffGraphBuilder): Unit =
    if (types.nonEmpty)
      if (types.size == 1)
        builder.setNodeProperty(x, PropertyNames.TYPE_FULL_NAME, types.head)
      else
        builder.setNodeProperty(x, PropertyNames.DYNAMIC_TYPE_HINT_FULL_NAME, types.toSeq)

}
