package io.joern.x2cpg.passes.frontend

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.codepropertygraph.generated.{Operators, PropertyNames}
import io.shiftleft.passes.CpgPass
import io.shiftleft.semanticcpg.language._
import io.shiftleft.semanticcpg.language.operatorextension.OpNodes
import io.shiftleft.semanticcpg.language.operatorextension.OpNodes.Assignment
import org.slf4j.{Logger, LoggerFactory}
import overflowdb.BatchedUpdate.DiffGraphBuilder
import overflowdb.traversal.Traversal

import java.util.Objects
import java.util.concurrent.RecursiveTask

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
    for (_ <- 0 to iterations)
      computationalUnit
        .map(unit => generateRecoveryForCompilationUnitTask(unit, builder, globalTable).fork())
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
    * @param globalTable
    *   the global table.
    * @return
    *   a forkable [[RecoverForXCompilationUnit]] task.
    */
  def generateRecoveryForCompilationUnitTask(
    unit: ComputationalUnit,
    builder: DiffGraphBuilder,
    globalTable: SymbolTable[GlobalKey]
  ): RecoverForXCompilationUnit[ComputationalUnit]

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
abstract class SetXProcedureDefTask(node: CfgNode) extends RecursiveTask[Unit] {

  protected val logger: Logger = LoggerFactory.getLogger(classOf[SetXProcedureDefTask])

  override def compute(): Unit =
    node match {
      case x: Method => visitImport(x)
      case x: Call   => visitImport(x)
      case _         =>
    }

  /** Refers to the declared import information.
    *
    * @param importCall
    *   the call that imports entities into this scope.
    */
  def visitImport(importCall: Call): Unit

  /** Sets how an application method would be referred to locally.
    *
    * @param m
    *   an internal method
    */
  def visitImport(m: Method): Unit

}

/** Performs type recovery from the root of a compilation unit level
  *
  * @param cu
  *   a compilation unit, e.g. file, procedure, type, etc.
  * @param builder
  *   the graph builder
  * @tparam ComputationalUnit
  *   the [[AstNode]] type used to represent a computational unit of the language.
  */
abstract class RecoverForXCompilationUnit[ComputationalUnit <: AstNode](
  cu: ComputationalUnit,
  builder: DiffGraphBuilder
) extends RecursiveTask[Unit] {

  /** Stores type information for local structures that live within this compilation unit, e.g. local variables.
    */
  protected val symbolTable = new SymbolTable[LocalKey](SBKey.fromNodeToLocalKey)

  /** Provides an entrypoint to add known symbols and their possible types.
    */
  def prepopulateSymbolTable(): Unit = {}

  protected def assignments: Traversal[Assignment] =
    cu.ast.isCall.name(Operators.assignment).map(new OpNodes.Assignment(_))

  override def compute(): Unit = try {
    prepopulateSymbolTable()
    // Set known aliases that point to imports for local and external methods/modules
    setImportsFromDeclaredProcedures(importNodes(cu) ++ internalMethodNodes(cu))
    // Prune import names if the methods exist in the CPG
    postVisitImports()
    // Populate local symbol table with assignments
    assignments.foreach(visitAssignments)
    // Persist findings
    setTypeInformation()
  } finally {
    symbolTable.clear()
  }

  /** Using import information and internally defined procedures, will generate a mapping between how functions and
    * types are aliased and called and themselves.
    *
    * @param procedureDeclarations
    *   imports to types or functions and internally defined methods themselves.
    */
  protected def setImportsFromDeclaredProcedures(procedureDeclarations: Traversal[CfgNode]): Unit =
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
  def generateSetProcedureDefTask(node: CfgNode, symbolTable: SymbolTable[LocalKey]): SetXProcedureDefTask

  /** @return
    *   the import nodes of this computational unit.
    */
  def importNodes(cu: AstNode): Traversal[CfgNode]

  /** @param cu
    *   the current computational unit.
    * @return
    *   the methods defined within this computational unit.
    */
  def internalMethodNodes(cu: AstNode): Traversal[Method] = cu.ast.isMethod.isExternal(false)

  /** The initial import setting is over-approximated, so this step checks the CPG for any matches and prunes against
    * these findings. If there are no findings, it will leave the table as is. The latter is significant for external
    * types or methods.
    */
  def postVisitImports(): Unit = {}

  /** Using assignment and import information (in the global symbol table), will propagate these types in the symbol
    * table.
    *
    * @param assignment
    *   assignment call pointer.
    */
  def visitAssignments(assignment: Assignment): Set[String] = {
    assignment.argumentOut.l match {
      case List(i: Identifier, b: Block)     => visitIdentifierAssignedToBlock(i, b)
      case List(i: Identifier, c: Call)      => visitIdentifierAssignedToCall(i, c)
      case List(i: Identifier, l: Literal)   => visitIdentifierAssignedToLiteral(i, l)
      case List(i: Identifier, m: MethodRef) => visitIdentifierAssignedToMethodRef(i, m)
      case List(i: Identifier, t: TypeRef)   => println(s"${i.name} = ${t.code} (iden  -> typref) "); Set.empty
      case List(c: Call, i: Identifier)      => println(s"${c.name} = ${i.name} (call  -> iden) "); Set.empty
      case List(x: Call, y: Call)            => println(s"${x.name} = ${y.name} (call  -> call)"); Set.empty
      case List(c: Call, l: Literal)         => visitCallAssignedToLiteral(c, l)
      case List(c: Call, m: MethodRef)       => println(s"${c.name} = ${m.code} (call  -> methodref)"); Set.empty
      case xs                                => println("Unhandled assignment", xs.map(_.label).l); Set.empty
    }
  }

  /** Visits an identifier being assigned to the result of some operation.
    */
  def visitIdentifierAssignedToBlock(i: Identifier, b: Block): Set[String] = {
    b.astChildren
      .map {
        case x: Call if x.name.equals(Operators.assignment) =>
          symbolTable.append(i, visitAssignments(new Assignment(x)))
        case x: Identifier if x.astChildren.isEmpty && symbolTable.contains(x) =>
          symbolTable.append(i, symbolTable.get(x))
        case x => println(s"Unhandled block element ${x.label}"); Set.empty[String]
      }
      .lastOption
      .getOrElse(Set.empty)
  }

  /** Visits an identifier being assigned to a call.
    */
  def visitIdentifierAssignedToCall(i: Identifier, c: Call): Set[String] = {
    if (c.name.startsWith("<operator>")) {
      visitIdentifierAssignedToOperator(i, c, c.name)
    } else {
      println(s"${i.name} = ${c.name} (Iden -> Call)")
      Set.empty
    }
  }

  /** Visits an identifier being assigned to an operator call.
    */
  def visitIdentifierAssignedToOperator(i: Identifier, c: Call, operation: String): Set[String]

  /** Will handle literal value assignments. Override if special handling is required.
    */
  def visitIdentifierAssignedToLiteral(i: Identifier, l: Literal): Set[String] =
    symbolTable.append(i, Set(l.typeFullName))

  /** Will handle an identifier holding a function pointer.
    */
  def visitIdentifierAssignedToMethodRef(i: Identifier, m: MethodRef): Set[String] =
    symbolTable.put(CallAlias(i.name), Set(m.methodFullName))

  def visitCallAssignedToLiteral(c: Call, l: Literal): Set[String] = {
    if (c.name.equals(Operators.indexAccess)) {
      // For now, we will just handle this on a very basic level
      c.argumentOut.l match {
        case List(i: Identifier, idx: Literal) => symbolTable.put(CollectionVar(i.name, idx.code), l.typeFullName)
        case List(i: Identifier, idx: Identifier) if symbolTable.contains(idx) =>
          symbolTable.put(CollectionVar(i.name, idx.code), symbolTable.get(idx))
        case _ => println("Unhandled index access point"); Set.empty
      }
    } else {
      println("Unhandled index access point"); Set.empty
    }
  }

  def visitFieldAssignment(): Unit = {}

  def visitFieldAssignedToFunctionCall(): Unit = {}

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
