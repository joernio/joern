package io.joern.x2cpg.passes.frontend

import io.joern.x2cpg.{Defines, X2CpgConfig}
import io.shiftleft.codepropertygraph.generated.{Cpg, DispatchTypes, EdgeTypes, NodeTypes, Operators, PropertyNames}
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.passes.{CpgPass, CpgPassBase, ForkJoinParallelCpgPass}
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.importresolver.*
import io.shiftleft.semanticcpg.language.operatorextension.OpNodes
import io.shiftleft.semanticcpg.language.operatorextension.OpNodes.{Assignment, FieldAccess}
import org.slf4j.{Logger, LoggerFactory}
import io.shiftleft.codepropertygraph.generated.DiffGraphBuilder
import scopt.OParser

import java.util.regex.Pattern
import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.{Failure, Success, Try}
import scala.util.matching.Regex

/** @param iterations
  *   the number of iterations to run.
  * @param enabledDummyTypes
  *   whether to enable placeholder dummy values for partially resolved types during the final iteration.
  */
case class XTypeRecoveryConfig(iterations: Int = 2, enabledDummyTypes: Boolean = true)

object XTypeRecoveryConfig {
  private val logger = LoggerFactory.getLogger(getClass)

  def parse(cmdLineArgs: Seq[String]): XTypeRecoveryConfig = {
    OParser
      .parse(parserOptions, cmdLineArgs, XTypeRecoveryConfig())
      .getOrElse(
        throw new RuntimeException(
          s"unable to parse XTypeRecoveryConfig from commandline arguments ${cmdLineArgs.mkString(" ")}"
        )
      )
  }

  def parserOptions: OParser[Unit, XTypeRecoveryConfig] = {
    _parserOptions[XTypeRecoveryConfig](
      configureNoDummyTypes = _.copy(enabledDummyTypes = false),
      configureIterations = (iterations, config) => config.copy(iterations = iterations)
    )
  }

  /** Parser options for languages implementing this pass. */
  def parserOptionsForParserConfig[R <: X2CpgConfig[R] & TypeRecoveryParserConfig[R]]: OParser[?, R] = {
    _parserOptions[R](
      configureNoDummyTypes = _.withDisableDummyTypes(true),
      configureIterations = (iterations, config) => config.withTypePropagationIterations(iterations)
    )
  }

  private def _parserOptions[C](configureNoDummyTypes: C => C, configureIterations: (Int, C) => C): OParser[Unit, C] = {
    val builder = OParser.builder[C]
    import builder.*
    OParser.sequence(
      opt[Unit]("no-dummyTypes")
        .hidden()
        .action((_, c) => configureNoDummyTypes(c))
        .text("disable generation of dummy types during type propagation"),
      opt[Int]("type-prop-iterations")
        .hidden()
        .action((x, c) => configureIterations(x, c))
        .text("maximum iterations of type propagation")
        .validate { x =>
          if (x <= 0) {
            logger.info("Disabling type propagation as the given iteration count is <= 0")
          } else if (x == 1) {
            logger.info("Intra-procedural type propagation enabled")
          } else if (x > 5) {
            logger.warn(s"Large iteration count of $x will take a while to terminate")
          }
          success
        }
    )
  }

}

/** @param config
  *   the user defined config.
  * @param currentIteration
  *   the current iteration.
  */
class XTypeRecoveryState(val config: XTypeRecoveryConfig = XTypeRecoveryConfig(), var currentIteration: Int = 0) {
  def isFinalIteration: Boolean = currentIteration == config.iterations - 1

  def enableDummyTypesForThisIteration: Boolean = isFinalIteration && config.enabledDummyTypes

  def isFirstIteration: Boolean = currentIteration == 0
}

object XTypeRecoveryPassGenerator {
  private def linkMembersToTheirRefs(cpg: Cpg, builder: DiffGraphBuilder): Unit = {
    import io.joern.x2cpg.passes.frontend.XTypeRecovery.AllNodeTypesFromIteratorExt
    import io.joern.x2cpg.passes.frontend.XTypeRecovery.AllNodeTypesFromNodeExt

    def getFieldBaseTypes(fieldAccess: FieldAccess): Iterator[TypeDecl] = {
      fieldAccess
        .argumentOption(1)
        .map {
          case x: Call if x.name == Operators.fieldAccess =>
            cpg.typeDecl.fullNameExact(x.asInstanceOf[FieldAccess].referencedMember.getKnownTypes.toSeq*)
          case x: Call if !x.name.startsWith("<operator>") =>
            if (!x.typeFullName.matches(XTypeRecovery.unknownTypePattern.pattern.pattern()))
              cpg.typeDecl.fullNameExact(x.typeFullName)
            else
              Iterator.empty
          case x: Expression =>
            cpg.typeDecl.fullNameExact(x.getKnownTypes.toSeq*)
        }
        .getOrElse(Iterator.empty)
    }

    // Set all now-typed fieldAccess calls to their referencing members (if they exist)
    cpg.fieldAccess
      .whereNot(_.referencedMember)
      .foreach { fieldAccess =>
        getFieldBaseTypes(fieldAccess).member
          .nameExact(fieldAccess.fieldIdentifier.canonicalName.toSeq*)
          .foreach(builder.addEdge(fieldAccess, _, EdgeTypes.REF))
      }
  }

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
abstract class XTypeRecoveryPassGenerator[CompilationUnitType <: AstNode](
  cpg: Cpg,
  config: XTypeRecoveryConfig = XTypeRecoveryConfig()
) {

  def generate(): List[CpgPassBase] = {
    val state = new XTypeRecoveryState(config)
    val res   = mutable.ArrayBuffer[CpgPassBase]()
    for (i <- Range(0, config.iterations)) {
      res.append(generateRecoveryPass(state, i))
    }
    if (postTypeRecoveryAndPropagation)
      res.append(
        new CpgPass(cpg):
          override def run(builder: DiffGraphBuilder): Unit = {
            XTypeRecoveryPassGenerator.linkMembersToTheirRefs(cpg, builder)
          }
      )
    res.toList
  }

  /** A hook for the end of the type recovery and propagation.
    */
  protected def postTypeRecoveryAndPropagation: Boolean = false
  protected def generateRecoveryPass(state: XTypeRecoveryState, iteration: Int): XTypeRecovery[CompilationUnitType]

}

trait TypeRecoveryParserConfig[R <: X2CpgConfig[R]] { this: R =>

  var disableDummyTypes: Boolean     = false
  var typePropagationIterations: Int = 2

  def withDisableDummyTypes(value: Boolean): R = {
    this.disableDummyTypes = value
    this
  }

  def withTypePropagationIterations(value: Int): R = {
    typePropagationIterations = value
    this
  }

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
abstract class XTypeRecovery[CompilationUnitType <: AstNode](cpg: Cpg, state: XTypeRecoveryState, iteration: Int)
    extends ForkJoinParallelCpgPass[CompilationUnitType](cpg) {

  // fixme: neable or disable?
  override def isParallel: Boolean = true
  override def init(): Unit = {
    super.init()
    state.currentIteration = iteration
  }

  override def generateParts(): Array[AnyRef] = compilationUnits.toArray[AnyRef]

  override def runOnPart(builder: DiffGraphBuilder, part: CompilationUnitType): Unit =
    generateRecoveryForCompilationUnitTask(part, builder).run()

  /** @return
    *   the compilation units as per how the language is compiled. e.g. file.
    */
  def compilationUnits: Iterator[CompilationUnitType]

  /** A factory method to generate a [[RecoverForXCompilationUnit]] task with the given parameters.
    *
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
  private val logger = LoggerFactory.getLogger(getClass)

  val DummyReturnType                       = "<returnValue>"
  val DummyMemberLoad                       = "<member>"
  val DummyIndexAccess                      = "<indexAccess>"
  private lazy val DummyTokens: Set[String] = Set(DummyReturnType, DummyMemberLoad, DummyIndexAccess)

  val unknownTypePattern: Regex = s"(i?)(UNKNOWN|ANY|${Defines.UnresolvedNamespace}).*".r

  def dummyMemberType(prefix: String, memberName: String, sep: String = "."): String =
    s"$prefix$sep$DummyMemberLoad($memberName)"

  /** Scans the type for placeholder/dummy types.
    */
  def isDummyType(typ: String): Boolean = DummyTokens.exists(typ.contains)

  @deprecated("please use XTypeRecoveryConfig.parserOptionsForParserConfig", since = "2.0.415")
  def parserOptions[R <: X2CpgConfig[R] & TypeRecoveryParserConfig[R]]: OParser[?, R] =
    XTypeRecoveryConfig.parserOptionsForParserConfig

  // The below are convenience calls for accessing type properties, one day when this pass uses `Tag` nodes instead of
  // the symbol table then perhaps this would work out better
  implicit class AllNodeTypesFromNodeExt(x: StoredNode) {
    def allTypes: Iterator[String] =
      (x.property(PropertyNames.TYPE_FULL_NAME, "ANY") +:
        (x.property(PropertyNames.DYNAMIC_TYPE_HINT_FULL_NAME, Seq.empty)
          ++ x.property(PropertyNames.POSSIBLE_TYPES, Seq.empty))).iterator

    def getKnownTypes: Set[String] = {
      x.allTypes.toSet.filterNot(XTypeRecovery.unknownTypePattern.matches)
    }
  }

  implicit class AllNodeTypesFromIteratorExt(x: Iterator[StoredNode]) {
    def allTypes: Iterator[String] = x.flatMap(_.allTypes)

    def getKnownTypes: Set[String] =
      x.allTypes.toSet.filterNot(XTypeRecovery.unknownTypePattern.matches)
  }

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
) extends Runnable {

  import io.joern.x2cpg.passes.frontend.XTypeRecovery.AllNodeTypesFromNodeExt
  import io.joern.x2cpg.passes.frontend.XTypeRecovery.AllNodeTypesFromIteratorExt

  protected val logger: Logger = LoggerFactory.getLogger(getClass)

  /** Stores type information for local structures that live within this compilation unit, e.g. local variables.
    */
  protected val symbolTable = new SymbolTable[LocalKey](fromNodeToLocalKey)

  protected val isFieldCache = mutable.HashMap[Identifier, Boolean]()

  protected def fromNodeToLocalKey(node: AstNode): Option[LocalKey] = SBKey.fromNodeToLocalKey(node)

  /** The root of the target codebase.
    */
  protected val codeRoot: String = s"${cpg.metaData.root.headOption.getOrElse("")}${java.io.File.separator}"

  /** The delimiter used to separate methods/functions in qualified names.
    */
  protected val pathSep = "."

  /** New node tracking set.
    */
  protected val addedNodes = mutable.HashSet.empty[String]

  /** For tracking members and the type operations that need to be performed. Since these are mostly out of scope
    * locally it helps to track these separately.
    *
    * // TODO: Potentially a new use for a global table or modification to the symbol table?
    */
  protected val newTypesForMembers = mutable.HashMap.empty[Member, Set[String]]

  /** Provides an entrypoint to add known symbols and their possible types.
    */
  protected def prepopulateSymbolTable(): Unit = {
    (cu.ast.isIdentifier ++ cu.ast.isCall ++ cu.ast.isLocal ++ cu.ast.isParameter)
      .filter(hasTypes)
      .foreach(prepopulateSymbolTableEntry)
  }

  protected def prepopulateSymbolTableEntry(x: AstNode): Unit = x match {
    case x @ (_: Identifier | _: Local | _: MethodParameterIn) => symbolTable.append(x, x.getKnownTypes)
    case call: Call =>
      symbolTable.append(call, (call.methodFullName +: (call.dynamicTypeHintFullName ++ call.possibleTypes)).toSet)
    case _ =>
  }

  protected def hasTypes(node: AstNode): Boolean = node match {
    case x: Call if !x.methodFullName.startsWith("<operator>") =>
      !x.methodFullName.toLowerCase().matches("(<unknownfullname>|any)")
    case x => x.getKnownTypes.nonEmpty
  }

  protected def assignments: Iterator[Assignment] = cu match {
    case x: File =>
      x.method.flatMap(_._callViaContainsOut).nameExact(Operators.assignment).cast[Assignment]
    case x: Method => x.flatMap(_._callViaContainsOut).nameExact(Operators.assignment).cast[Assignment]
    case x         => x.ast.isCall.nameExact(Operators.assignment).cast[Assignment]
  }

  protected def returns: Iterator[Return] = cu match {
    case x: File   => x.method.flatMap(_._returnViaContainsOut)
    case x: Method => x._returnViaContainsOut
    case x         => x.ast.isReturn
  }

  protected def importNodes: Iterator[Import] = cu match {
    case x: File   => x.method.flatMap(_._callViaContainsOut).referencedImports
    case x: Method => x.file.method.flatMap(_._callViaContainsOut).referencedImports
    case x         => x.ast.isCall.referencedImports
  }

  override def run(): Unit = {
    // Set known aliases that point to imports for local and external methods/modules
    importNodes.foreach(visitImport)
    // Look at symbols with existing type info
    prepopulateSymbolTable()
    // Prune import names if the methods exist in the CPG
    postVisitImports()
    // Populate local symbol table with assignments
    assignments.foreach(visitAssignments)
    // See if any new information are in the parameters of methods
    returns.foreach(visitReturns)
    // Persist findings
    setTypeInformation()
    // Entrypoint for any final changes
    postSetTypeInformation()
  }

  private def debugLocation(n: AstNode): String = {
    val fileName = n.file.name.headOption.getOrElse("<unknown>").stripPrefix(codeRoot)
    val lineNo   = n.lineNumber.getOrElse("<unknown>")
    s"$fileName#L$lineNo"
  }

  /** Visits an import and stores references in the symbol table as both an identifier and call.
    */
  protected def visitImport(i: Import): Unit = for {
    resolvedImport <- i.call.tag
    alias          <- i.importedAs
  } {
    EvaluatedImport.tagToEvaluatedImport(resolvedImport).foreach {
      case ResolvedMethod(fullName, alias, receiver, _) =>
        symbolTable.append(CallAlias(alias, receiver), fullName)
      case ResolvedTypeDecl(fullName, _) =>
        symbolTable.append(LocalVar(alias), fullName)
      case ResolvedMember(basePath, memberName, _) =>
        val matchingIdentifiers = cpg.method.fullNameExact(basePath).local
        val matchingMembers     = cpg.typeDecl.fullNameExact(basePath).member
        val memberTypes = (matchingMembers ++ matchingIdentifiers)
          .nameExact(memberName)
          .getKnownTypes
        symbolTable.append(LocalVar(alias), memberTypes)
      case UnknownMethod(fullName, alias, receiver, _) =>
        symbolTable.append(CallAlias(alias, receiver), fullName)
      case UnknownTypeDecl(fullName, _) =>
        symbolTable.append(LocalVar(alias), fullName)
      case UnknownImport(path, _) =>
        symbolTable.append(CallAlias(alias), path)
        symbolTable.append(LocalVar(alias), path)
    }
  }

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
  protected def visitAssignments(a: Assignment): Set[String] = visitAssignmentArguments(a.argumentOut.l)

  protected def visitAssignmentArguments(args: List[AstNode]): Set[String] = args match {
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

  /** Visits an identifier being assigned to the result of some operation.
    */
  protected def visitIdentifierAssignedToBlock(i: Identifier, b: Block): Set[String] = {
    val blockTypes = visitStatementsInBlock(b, Some(i))
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
  protected def visitStatementsInBlock(b: Block, assignmentTarget: Option[Identifier] = None): Set[String] =
    b.astChildren
      .map {
        case x: Call if x.name.startsWith(Operators.assignment) => visitAssignments(x.asInstanceOf[Assignment])
        case x: Call if x.name.startsWith("<operator>") && assignmentTarget.isDefined =>
          visitIdentifierAssignedToOperator(assignmentTarget.get, x, x.name)
        case x: Identifier if symbolTable.contains(x)                      => symbolTable.get(x)
        case x: Call if symbolTable.contains(x)                            => symbolTable.get(x)
        case x: Call if x.argument.headOption.exists(symbolTable.contains) => setCallMethodFullNameFromBase(x)
        case x: Block                                                      => visitStatementsInBlock(x)
        case x: Local                                                      => symbolTable.get(x)
        case _: ControlStructure                                           => Set.empty[String]
        case x =>
          logger.debug(s"Unhandled block element ${x.label}:${x.code} @ ${debugLocation(x)}")
          Set.empty[String]
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
    } else if (c.argument.argumentIndex(0).headOption.exists(symbolTable.contains)) {
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
    val recTypes = c.argument.headOption
      .map {
        case x: Call if x.typeFullName != "ANY" =>
          Set(x.typeFullName)
        case x: Call =>
          val returns                 = cpg.method.fullNameExact(c.methodFullName).methodReturn.typeFullNameNot("ANY")
          val returnWithPossibleTypes = cpg.method.fullNameExact(c.methodFullName).methodReturn.where(_.possibleTypes)
          val fullNames               = returns.typeFullName ++ returnWithPossibleTypes.possibleTypes
          fullNames.toSet match {
            case xs if xs.nonEmpty => xs
            case _ => symbolTable.get(x).map(t => Seq(t, XTypeRecovery.DummyReturnType).mkString(pathSep))
          }
        case x =>
          symbolTable.get(x)
      }
      .getOrElse(Set.empty[String])
    val callTypes = recTypes.map(_.concat(s"$pathSep${c.name}"))
    symbolTable.append(c, callTypes)
  }

  /** A heuristic method to determine if a call is a constructor or not.
    */
  protected def isConstructor(c: Call): Boolean

  /** A heuristic method to determine if a call name is a constructor or not.
    */
  protected def isConstructor(name: String): Boolean

  /** A heuristic method to determine if an identifier may be a field or not. The result means that it would be stored
    * in the global symbol table. By default this checks if the identifier name matches a member name.
    *
    * This has found to be an expensive operation accessed often so we have memoized this step.
    */
  protected final def isField(i: Identifier): Boolean =
    isFieldCache.getOrElseUpdate(i, isFieldUncached(i))

  protected def isFieldUncached(i: Identifier): Boolean =
    i.method.typeDecl.member.nameExact(i.name).nonEmpty

  /** Associates the types with the identifier. This may sometimes be an identifier that should be considered a field
    * which this method uses [[isField]] to determine.
    */
  protected def associateTypes(i: Identifier, types: Set[String]): Set[String] =
    symbolTable.append(i, types)

  /** Returns the appropriate field parent scope.
    */
  protected def getFieldParents(fa: FieldAccess): Set[String] = {
    val fieldName = getFieldName(fa).split(Pattern.quote(pathSep)).last
    Try(cpg.member.nameExact(fieldName).typeDecl.fullName.filterNot(_.contains("ANY")).toSet) match
      case Failure(exception) =>
        logger.warn("Unable to obtain name of member's parent type declaration", exception)
        Set.empty
      case Success(typeDeclNames) => typeDeclNames
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
      lazy val existingMembers = cpg.typeDecl.fullNameExact(baseTypes.toSeq*).member.nameExact(fieldName)
      if (baseTypes.equals(symbolTable.get(LocalVar(fieldFullName)))) {
        associateTypes(i, baseTypes)
      } else if (existingMembers.isEmpty) {
        // If not available, use a dummy variable that can be useful for call matching
        associateTypes(i, baseTypes.map(t => XTypeRecovery.dummyMemberType(t, fieldName, pathSep)))
      } else {
        Set.empty
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
      case Operators.fieldAccess => visitIdentifierAssignedToFieldLoad(i, c.asInstanceOf[FieldAccess])
      case Operators.indexAccess => visitIdentifierAssignedToIndexAccess(i, c)
      case Operators.cast        => visitIdentifierAssignedToCast(i, c)
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
    val rs = cpg.method
      .fullNameExact(methodFullNames*)
      .methodReturn
      .flatMap(mr => mr.typeFullName +: (mr.dynamicTypeHintFullName ++ mr.possibleTypes))
      .filterNot(_.equals("ANY"))
      .toSet
    if (rs.isEmpty) methodFullNames.map(_.concat(s"$pathSep${XTypeRecovery.DummyReturnType}")).toSet
    else rs
  }

  /** Will handle literal value assignments. Override if special handling is required.
    */
  protected def visitIdentifierAssignedToLiteral(i: Identifier, l: Literal): Set[String] =
    associateTypes(i, getLiteralType(l))

  /** Not all frontends populate <code>typeFullName</code> for literals so we allow this to be overridden.
    */
  protected def getLiteralType(l: Literal): Set[String] = Set(l.typeFullName) ++ l.possibleTypes

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
    case Operators.fieldAccess        => symbolTable.get(LocalVar(getFieldName(c.asInstanceOf[FieldAccess])))
    case _ if symbolTable.contains(c) => methodReturnValues(symbolTable.get(c).toSeq)
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
    case Some(cVar) if ia.argument(1).isCall && symbolTable.contains(CallAlias(cVar.identifier)) =>
      symbolTable
        .get(CallAlias(cVar.identifier))
        .map(x => s"$x$pathSep${XTypeRecovery.DummyReturnType}$pathSep${XTypeRecovery.DummyIndexAccess}")
    case Some(cVar) if symbolTable.contains(LocalVar(cVar.identifier)) =>
      symbolTable.get(LocalVar(cVar.identifier)).map(x => s"$x$pathSep${XTypeRecovery.DummyIndexAccess}")
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
      val fa        = c.asInstanceOf[FieldAccess]
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

    lazy val typesFromBaseCall = fa.argumentOut.headOption match
      case Some(call: Call) => getTypesFromCall(call)
      case _                => Set.empty[String]

    fa.argumentOut.l match {
      case ::(i: Identifier, ::(f: FieldIdentifier, _)) if i.name.matches("(self|this)") => wrapName(f.canonicalName)
      case ::(i: Identifier, ::(f: FieldIdentifier, _)) => wrapName(s"${i.name}$pathSep${f.canonicalName}")
      case ::(c: Call, ::(f: FieldIdentifier, _)) if c.name.equals(Operators.fieldAccess) =>
        wrapName(getFieldName(c.asInstanceOf[FieldAccess], suffix = f.canonicalName))
      case ::(_: Call, ::(f: FieldIdentifier, _)) if typesFromBaseCall.nonEmpty =>
        // TODO: Handle this case better
        wrapName(s"${typesFromBaseCall.head}$pathSep${f.canonicalName}")
      case ::(f: FieldIdentifier, ::(c: Call, _)) if c.name.equals(Operators.fieldAccess) =>
        wrapName(getFieldName(c.asInstanceOf[FieldAccess], prefix = f.canonicalName))
      case ::(c: Call, ::(f: FieldIdentifier, _)) =>
        // TODO: Handle this case better
        val callCode = if (c.code.contains("(")) c.code.substring(c.code.indexOf("(")) else c.code
        XTypeRecovery.dummyMemberType(callCode, f.canonicalName, pathSep)
      case ::(_: TypeRef, ::(f: FieldIdentifier, _)) =>
        f.canonicalName
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
      val fa        = c.asInstanceOf[FieldAccess]
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
        getFieldName(x.asInstanceOf[FieldAccess])
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
    fa.argumentOut.l match {
      case ::(base: Identifier, ::(fi: FieldIdentifier, _)) if symbolTable.contains(LocalVar(base.name)) =>
        // Get field from global table if referenced as a variable
        val localTypes = symbolTable.get(LocalVar(base.name))
        associateInterproceduralTypes(i, base, fi, fieldName, localTypes)
      case ::(base: Identifier, ::(fi: FieldIdentifier, _)) if symbolTable.contains(LocalVar(fieldName)) =>
        val localTypes = symbolTable.get(LocalVar(fieldName))
        associateInterproceduralTypes(i, base, fi, fieldName, localTypes)
      case ::(base: Identifier, ::(fi: FieldIdentifier, _)) =>
        val dummyTypes = Set(s"$fieldName$pathSep${XTypeRecovery.DummyReturnType}")
        associateInterproceduralTypes(i, base, fi, fieldName, dummyTypes)
      case ::(c: Call, ::(fi: FieldIdentifier, _)) if c.name.equals(Operators.fieldAccess) =>
        val baseName = getFieldName(c.asInstanceOf[FieldAccess])
        // Build type regardless of length
        // TODO: This is more prone to giving dummy values as it does not do global look-ups
        //  but this is okay for now
        val buf = mutable.ArrayBuffer.empty[String]
        for (segment <- baseName.split(Pattern.quote(pathSep)) ++ Array(fi.canonicalName)) {
          val types =
            if (buf.isEmpty) symbolTable.get(LocalVar(segment))
            else buf.flatMap(t => symbolTable.get(LocalVar(s"$t$pathSep$segment"))).toSet
          if (types.nonEmpty) {
            buf.clear()
            buf.addAll(types)
          } else {
            val bufCopy = Array.from(buf)
            buf.clear()
            bufCopy.foreach {
              case t if isConstructor(segment) => buf.addOne(s"$t$pathSep$segment")
              case t                           => buf.addOne(XTypeRecovery.dummyMemberType(t, segment, pathSep))
            }
          }
        }
        associateTypes(i, buf.toSet)
      case ::(call: Call, ::(fi: FieldIdentifier, _)) =>
        assignTypesToCall(
          call,
          Set(fieldName.stripSuffix(s"${XTypeRecovery.DummyMemberLoad}$pathSep${fi.canonicalName}"))
        )
      case _ =>
        logger.warn(s"Unable to assign identifier '${i.name}' to field load '$fieldName' @ ${debugLocation(i)}")
        Set.empty
    }
  }

  /** Visits an identifier being assigned to the result of an index access operation.
    */
  protected def visitIdentifierAssignedToIndexAccess(i: Identifier, c: Call): Set[String] =
    associateTypes(i, getTypesFromCall(c))

  /** Visits an identifier that is the target of a cast operation.
    */
  protected def visitIdentifierAssignedToCast(i: Identifier, c: Call): Set[String] =
    associateTypes(i, (c.typeFullName +: (c.dynamicTypeHintFullName ++ c.possibleTypes)).filterNot(_ == "ANY").toSet)

  protected def getFieldBaseType(base: Identifier, fi: FieldIdentifier): Set[String] =
    getFieldBaseType(base.name, fi.canonicalName)

  protected def getFieldBaseType(baseName: String, fieldName: String): Set[String] =
    symbolTable
      .get(LocalVar(baseName))
      .flatMap(t => typeDeclIterator(t).member.nameExact(fieldName))
      .flatMap(m => m.typeFullName +: (m.dynamicTypeHintFullName ++ m.possibleTypes))
      .filterNot(_ == "ANY")

  protected def visitReturns(ret: Return): Unit = {
    val m = ret.method
    val existingTypes = mutable.HashSet.from(
      (m.methodReturn.typeFullName +: (m.methodReturn.dynamicTypeHintFullName ++ m.methodReturn.possibleTypes))
        .filterNot(_ == "ANY")
    )
    @tailrec
    def extractTypes(xs: List[CfgNode]): Set[String] = xs match {
      case ::(head: Literal, Nil) => getLiteralType(head)
      case ::(head: Call, Nil) if head.name == Operators.fieldAccess =>
        val fieldAccess = head.asInstanceOf[FieldAccess]
        val (sym, ts)   = getSymbolFromCall(fieldAccess)
        val cpgTypes = cpg.typeDecl
          .fullNameExact(ts.map(_.compUnitFullName).toSeq*)
          .member
          .nameExact(sym.identifier)
          .flatMap(m => m.typeFullName +: (m.dynamicTypeHintFullName ++ m.possibleTypes))
          .filterNot { x => x == "ANY" || x == "this" }
          .toSet
        if (cpgTypes.nonEmpty) cpgTypes
        else symbolTable.get(sym)
      case ::(head: Call, Nil) if symbolTable.contains(head) =>
        val callPaths    = symbolTable.get(head)
        val returnValues = methodReturnValues(callPaths.toSeq)
        if (returnValues.isEmpty)
          callPaths.map(c => s"$c$pathSep${XTypeRecovery.DummyReturnType}")
        else
          returnValues
      case ::(head: Call, Nil) if head.argumentOut.headOption.exists(symbolTable.contains) =>
        symbolTable
          .get(head.argumentOut.head)
          .map(t => Seq(t, head.name, XTypeRecovery.DummyReturnType).mkString(pathSep))
      case ::(identifier: Identifier, Nil) if symbolTable.contains(identifier) =>
        symbolTable.get(identifier)
      case ::(head: Call, Nil) =>
        extractTypes(head.argument.l)
      case _ => Set.empty
    }
    val returnTypes = extractTypes(ret.argumentOut.l)
    existingTypes.addAll(returnTypes)
    builder.setNodeProperty(ret.method.methodReturn, PropertyNames.DYNAMIC_TYPE_HINT_FULL_NAME, existingTypes)
  }

  /** Using an entry from the symbol table, will queue the CPG modification to persist the recovered type information.
    */
  protected def setTypeInformation(): Unit = {
    cu.ast
      .collect {
        case n: Local                                       => n
        case n: Call                                        => n
        case n: Expression                                  => n
        case n: MethodParameterIn if state.isFinalIteration => n
        case n: MethodReturn if state.isFinalIteration      => n
      }
      .foreach {
        case x: Local if symbolTable.contains(x) => storeNodeTypeInfo(x, symbolTable.get(x).toSeq)
        case x: MethodParameterIn                => setTypeFromTypeHints(x)
        case x: MethodReturn =>
          setTypeFromTypeHints(x)
        case x: Identifier if symbolTable.contains(x) =>
          setTypeInformationForRecCall(x, x.inCall.headOption, x.inCall.argument.l)
        case x: Call if symbolTable.contains(x) =>
          val typs =
            if (state.enableDummyTypesForThisIteration) symbolTable.get(x).toSeq
            else symbolTable.get(x).filterNot(XTypeRecovery.isDummyType).toSeq
          storeCallTypeInfo(x, typs)
        case x: Identifier if symbolTable.contains(CallAlias(x.name)) && x.inCall.nonEmpty =>
          setTypeInformationForRecCall(x, x.inCall.headOption, x.inCall.argument.l)
        case x: Call if x.argument.headOption.exists(symbolTable.contains) =>
          setTypeInformationForRecCall(x, Option(x), x.argument.l)
        case _ =>
      }
    // Set types in an atomic way
    newTypesForMembers.foreach { case (m, ts) => storeDefaultTypeInfo(m, ts.toSeq) }
  }

  protected def createCallFromIdentifierTypeFullName(typeFullName: String, callName: String): String =
    s"$typeFullName$pathSep$callName"

  /** Sets type information for a receiver/call pattern.
    */
  private def setTypeInformationForRecCall(x: AstNode, n: Option[Call], ms: List[AstNode]): Unit = {
    (n, ms) match {
      // Case 1: 'call' is an assignment from some dynamic dispatch call
      case (Some(call: Call), ::(i: Identifier, ::(c: Call, _))) if call.name == Operators.assignment =>
        setTypeForIdentifierAssignedToCall(call, i, c)
      // Case 1: 'call' is an assignment from some other data structure
      case (Some(call: Call), ::(i: Identifier, _)) if call.name == Operators.assignment =>
        setTypeForIdentifierAssignedToDefault(call, i)
      // Case 2: 'i' is the receiver of 'call'
      case (Some(call: Call), ::(i: Identifier, _)) if call.name != Operators.fieldAccess =>
        (i.argumentIndex, call.dispatchType) match {
          case (0, DispatchTypes.DYNAMIC_DISPATCH) => setTypeForDynamicDispatchCall(call, i)
          case (1, DispatchTypes.STATIC_DISPATCH)  => setTypeForStaticDispatchCall(call, i)
          case _                                   =>
        }
      // Case 3: 'i' is the receiver for a field access on member 'f'
      case (Some(fieldAccess: Call), ::(i: Identifier, ::(f: FieldIdentifier, _)))
          if fieldAccess.name == Operators.fieldAccess =>
        setTypeForFieldAccess(fieldAccess.asInstanceOf[FieldAccess], i, f)
      case _ =>
    }
    // Handle the node itself
    x match {
      case c: Call if c.name.startsWith("<operator") =>
      case _                                         => persistType(x, symbolTable.get(x))
    }
  }

  protected def setTypeForFieldAccess(fieldAccess: Call, i: Identifier, f: FieldIdentifier): Unit = {
    val idHints   = if (symbolTable.contains(i)) symbolTable.get(i) else symbolTable.get(CallAlias(i.name))
    val callTypes = symbolTable.get(fieldAccess)
    persistType(i, idHints)
    persistType(fieldAccess, callTypes)
    fieldAccess.astParent.iterator.isCall.headOption match {
      case Some(callFromFieldName) if symbolTable.contains(callFromFieldName) =>
        persistType(callFromFieldName, symbolTable.get(callFromFieldName))
      case _ =>
    }
    // This field may be a function pointer
    handlePotentialFunctionPointer(fieldAccess, idHints, f.canonicalName, Option(i.name))
  }

  protected def setTypeForDynamicDispatchCall(call: Call, i: Identifier): Unit = {
    val idHints   = symbolTable.get(i)
    val callTypes = symbolTable.get(call)
    persistType(i, idHints)
    if (callTypes.isEmpty && !call.name.startsWith("<operator>"))
      // For now, calls are treated as function pointers and thus the type should point to the method
      persistType(call, idHints.map(t => createCallFromIdentifierTypeFullName(t, call.name)))
    else {
      persistType(call, callTypes)
    }
  }

  protected def setTypeForStaticDispatchCall(call: Call, i: Identifier): Unit = {
    // TODO: Should it have special handling?
    setTypeForDynamicDispatchCall(call, i)
  }

  protected def setTypeForIdentifierAssignedToDefault(call: Call, i: Identifier): Unit = {
    val idHints = symbolTable.get(i)
    persistType(i, idHints)
    persistType(call, idHints)
  }

  protected def setTypeForIdentifierAssignedToCall(call: Call, i: Identifier, c: Call): Unit = {
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
  }

  protected def setTypeFromTypeHints(n: StoredNode): Unit = {
    val types = n.getKnownTypes.filterNot(XTypeRecovery.isDummyType)
    if (types.nonEmpty)
      setTypes(n, types.toSeq)
  }

  /** In the case this field access is a function pointer, we would want to make sure this has a method ref.
    */
  protected def handlePotentialFunctionPointer(
    funcPtr: Expression,
    baseTypes: Set[String],
    funcName: String,
    baseName: Option[String] = None
  ): Unit = {
    // Sometimes the function identifier is an argument to the call itself as a "base". In this case we don't need
    // a method ref. This happens in jssrc2cpg
    if (!funcPtr.astParent.iterator.collectAll[Call].exists(_.name == funcName)) {
      baseTypes
        .map(t => if (t.endsWith(funcName)) t else s"$t$pathSep$funcName")
        .flatMap(cpg.method.fullNameExact)
        .filterNot(m => addedNodes.contains(s"${funcPtr.id()}${NodeTypes.METHOD_REF}$pathSep${m.fullName}"))
        .map(m => m -> createMethodRef(baseName, funcName, m.fullName, funcPtr.lineNumber, funcPtr.columnNumber))
        .foreach { case (m, mRef) =>
          funcPtr
            .map { ptr =>
              ptr.astParent match
                case parent: Call if parent.name.startsWith("<operator>") => ptr
                case parent                                               => parent
            }
            .filterNot(_.astChildren.isMethodRef.exists(_.methodFullName == mRef.methodFullName))
            .foreach { inCall =>
              integrateMethodRef(funcPtr, m, mRef, inCall)
            }
        }
    }
  }

  private def createMethodRef(
    baseName: Option[String],
    funcName: String,
    methodFullName: String,
    lineNo: Option[Int],
    columnNo: Option[Int]
  ): NewMethodRef =
    NewMethodRef()
      .code(s"${baseName.map(_ + pathSep).getOrElse("")}$funcName")
      .methodFullName(methodFullName)
      .lineNumber(lineNo)
      .columnNumber(columnNo)

  /** Integrate this method ref node into the CPG according to schema rules. Since we're adding this after the base
    * passes, we need to add the necessary linking manually.
    */
  private def integrateMethodRef(funcPtr: Expression, m: Method, mRef: NewMethodRef, inCall: AstNode) = {
    builder.addNode(mRef)
    builder.addEdge(mRef, m, EdgeTypes.REF)
    builder.addEdge(inCall, mRef, EdgeTypes.AST)
    builder.addEdge(funcPtr.method, mRef, EdgeTypes.CONTAINS)
    inCall match {
      case x: Call =>
        builder.addEdge(x, mRef, EdgeTypes.ARGUMENT)
        mRef.argumentIndex(x.argumentOut.size + 1)
      case x =>
        mRef.argumentIndex(x.astChildren.size + 1)
    }
    addedNodes.add(s"${funcPtr.id()}${NodeTypes.METHOD_REF}$pathSep${mRef.methodFullName}")
  }

  protected def persistType(x: StoredNode, types: Set[String]): Unit = {
    val filteredTypes =
      if (state.enableDummyTypesForThisIteration) types else types.filterNot(XTypeRecovery.isDummyType)
    if (filteredTypes.nonEmpty) {
      storeNodeTypeInfo(x, filteredTypes.toSeq)
      x match {
        case i: Identifier if symbolTable.contains(i) =>
          if (isField(i)) {
            persistMemberType(i, filteredTypes)
            handlePotentialFunctionPointer(i, filteredTypes, i.name)
          }
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
  protected def typeDeclIterator(typeFullName: String): Iterator[TypeDecl] = cpg.typeDecl.fullNameExact(typeFullName)

  /** Given a type full name and member name, will persist the given types to the member.
    * @param typeFullName
    *   the type full name.
    * @param memberName
    *   the member name.
    * @param types
    *   the types to associate.
    */
  protected def persistMemberWithTypeDecl(typeFullName: String, memberName: String, types: Set[String]): Unit =
    typeDeclIterator(typeFullName).member.nameExact(memberName).headOption.foreach { m =>
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
    typeDeclIterator(i.method.typeDecl.fullName.headOption.getOrElse(i.method.fullName)).member
      .nameExact(i.name)
      .headOption

  private def storeNodeTypeInfo(storedNode: StoredNode, types: Seq[String]): Unit = {
    lazy val existingTypes = storedNode.getKnownTypes

    val hasUnknownTypeFullName = storedNode
      .property(PropertyNames.TYPE_FULL_NAME, Defines.Any)
      .matches(XTypeRecovery.unknownTypePattern.pattern.pattern())

    if (types.nonEmpty && (hasUnknownTypeFullName || types.toSet != existingTypes)) {
      storedNode match {
        case m: Member =>
          // To avoid overwriting member updates, we store them elsewhere until the end
          newTypesForMembers.updateWith(m) {
            case Some(ts) => Option(ts ++ types)
            case None     => Option(types.toSet)
          }
        case i: Identifier                               => storeIdentifierTypeInfo(i, types)
        case l: Local                                    => storeLocalTypeInfo(l, types)
        case c: Call if !c.name.startsWith("<operator>") => storeCallTypeInfo(c, types)
        case _: Call                                     =>
        case n =>
          setTypes(n, types)
      }
    }
  }

  protected def storeCallTypeInfo(c: Call, types: Seq[String]): Unit =
    if (types.nonEmpty) {
      builder.setNodeProperty(
        c,
        PropertyNames.DYNAMIC_TYPE_HINT_FULL_NAME,
        (c.dynamicTypeHintFullName ++ types).distinct
      )
    }

  /** Allows one to modify the types assigned to identifiers.
    */
  protected def storeIdentifierTypeInfo(i: Identifier, types: Seq[String]): Unit =
    storeDefaultTypeInfo(i, types)

  /** Allows one to modify the types assigned to nodes otherwise.
    */
  protected def storeDefaultTypeInfo(n: StoredNode, types: Seq[String]): Unit =
    val hasUnknownType =
      n.property(PropertyNames.TYPE_FULL_NAME, Defines.Any).matches(XTypeRecovery.unknownTypePattern.pattern.pattern())

    if (types.toSet != n.getKnownTypes || (hasUnknownType && types.nonEmpty)) {
      setTypes(n, (n.property(PropertyNames.DYNAMIC_TYPE_HINT_FULL_NAME, Seq.empty) ++ types).distinct)
    }

  /** If there is only 1 type hint then this is set to the `typeFullName` property and `dynamicTypeHintFullName` is
    * cleared. If not then `dynamicTypeHintFullName` is set to the types.
    */
  protected def setTypes(n: StoredNode, types: Seq[String]): Unit =
    if (types.sizeIs == 1) builder.setNodeProperty(n, PropertyNames.TYPE_FULL_NAME, types.head)
    else builder.setNodeProperty(n, PropertyNames.DYNAMIC_TYPE_HINT_FULL_NAME, types)

  /** Allows one to modify the types assigned to locals.
    */
  protected def storeLocalTypeInfo(l: Local, types: Seq[String]): Unit = {
    storeDefaultTypeInfo(
      l,
      if (state.enableDummyTypesForThisIteration) types else types.filterNot(XTypeRecovery.isDummyType)
    )
  }

  /** Allows an implementation to perform an operation once type persistence is complete.
    */
  protected def postSetTypeInformation(): Unit = {}

}
