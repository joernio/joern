package io.joern.x2cpg.passes.frontend

import io.joern.x2cpg.Defines
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.codepropertygraph.generated.{Operators, PropertyNames}
import io.shiftleft.passes.CpgPass
import io.shiftleft.semanticcpg.language._
import io.shiftleft.semanticcpg.language.operatorextension.OpNodes
import io.shiftleft.semanticcpg.language.operatorextension.OpNodes.Assignment
import org.apache.logging.log4j.LogManager
import overflowdb.traversal.Traversal

import java.io.{File => JFile}
import java.util.concurrent.RecursiveTask
import java.util.regex.Matcher
import scala.collection.MapView
import scala.collection.concurrent.TrieMap
import scala.util.Try

/** Based on a flow-insensitive symbol-table-style approach. This does not accurately determine the difference between
  * shadowed variables in the same file but this is due to REF edges not connecting children methods to parent scope
  * (yet).
  */
class PythonTypeRecovery(cpg: Cpg) extends CpgPass(cpg) {

  private val globalTable = new SymbolTable()
  private val logger      = LogManager.getLogger(classOf[PythonTypeRecovery])

  override def run(builder: DiffGraphBuilder): Unit =
    try {
      // Set known aliases that point to imports for local and external methods/modules
      setImportsFromDeclaredProcedures(importNodes ++ internalMethodNodes)
      // Prune import names if the methods exist in the CPG
      postVisitImports()
      // Find identifiers that have been declared with some literal or function known in the symbol table
      cpg.file.map(unit => new RecoverForCompilationUnit(unit, builder).fork()).foreach(_.get())
    } finally {
      globalTable.clear()
    }

  /** Using import information and internally defined procedures, will generate a mapping between how functions and
    * types are aliased and called and themselves.
    *
    * @param procedureDeclarations
    *   imports to types or functions and internally defined methods themselves.
    */
  private def setImportsFromDeclaredProcedures(procedureDeclarations: Traversal[CfgNode]): Unit =
    procedureDeclarations.map(f => new SetImportTask(f).fork()).foreach(_.get())

  private def importNodes: Traversal[CfgNode] = cpg.call.nameExact("import")

  private def internalMethodNodes: Traversal[Method] = cpg.method.isExternal(false)

  /** The initial import setting is over-approximated, so this step checks the CPG for any matches and prunes against
    * these findings. If there are no findings, it will leave the table as is. The latter is significant for external
    * types or methods.
    */
  private def postVisitImports(): Unit = {
    globalTable.view.foreach { case (k, v) =>
      val ms = cpg.method.fullNameExact(v.toSeq: _*).l
      val ts = cpg.typeDecl.fullNameExact(v.toSeq: _*).l
      if (ts.nonEmpty)
        globalTable.put(k, ts.fullName.toSet)
      else if (ms.nonEmpty)
        globalTable.put(k, ms.fullName.toSet)
      else {
        // This is likely external and we will ignore the init variant to be consistent
        globalTable.put(k, globalTable(k).filterNot(_.contains("__init__.py")))
      }
    }
  }

  /** Tasks responsible for populating the symbol table with import data.
    * @param node
    *   a node that references import information.
    */
  class SetImportTask(node: CfgNode) extends RecursiveTask[Unit] {

    override def compute(): Unit = {
      node match {
        case x: Method => visitImport(x)
        case x: Call   => visitImport(x)
      }
    }

    /** Refers to the declared import information.
      *
      * @param importCall
      *   the call that imports entities into this scope.
      */
    private def visitImport(importCall: Call): Unit = {
      importCall.argumentOut.l match {
        case List(path: Literal, funcOrModule: Literal) =>
          val calleeNames = extractMethodDetailsFromImport(path.code, funcOrModule.code).possibleCalleeNames
          globalTable.put(funcOrModule, calleeNames)
        case List(path: Literal, funcOrModule: Literal, alias: Literal) =>
          val calleeNames =
            extractMethodDetailsFromImport(path.code, funcOrModule.code, Option(alias.code)).possibleCalleeNames
          globalTable.put(alias, calleeNames)
        case x => logger.warn(s"Unknown import pattern: ${x.map(_.label).mkString(", ")}")
      }
    }

    /** Parses all imports and identifies their full names and how they are to be called in this scope.
      *
      * @param path
      *   the module path.
      * @param funcOrModule
      *   the name of the imported entity.
      * @param maybeAlias
      *   an optional alias given to the imported entity.
      * @return
      *   the procedure information in this scope.
      */
    private def extractMethodDetailsFromImport(
      path: String,
      funcOrModule: String,
      maybeAlias: Option[String] = None
    ): ProcedureInScope = {
      val isConstructor = funcOrModule.split("\\.").last.charAt(0).isUpper
      if (path.isEmpty) {
        if (funcOrModule.contains(".")) {
          // Case 1: We have imported a function using a qualified path, e.g., import foo.bar => (bar.py or bar/__init.py)
          val splitFunc = funcOrModule.split("\\.")
          val name      = splitFunc.tail.mkString(".")
          ProcedureInScope(name, s"${splitFunc(0)}.py:<module>.$name", isConstructor)
        } else {
          // Case 2: We have imported a module, e.g., import foo => (foo.py or foo/__init.py)
          ProcedureInScope(funcOrModule, s"$funcOrModule.py:<module>", isConstructor)
        }
      } else {
        val sep = Matcher.quoteReplacement(JFile.separator)
        maybeAlias match {
          // TODO: This assumes importing from modules and never importing nested method
          // Case 3:  We have imported a function from a module using an alias, e.g. import bar from foo as faz
          case Some(alias) =>
            ProcedureInScope(alias, s"${path.replaceAll("\\.", sep)}.py:<module>.$funcOrModule", isConstructor)
          // Case 4: We have imported a function from a module, e.g. import bar from foo
          case None =>
            ProcedureInScope(funcOrModule, s"${path.replaceAll("\\.", sep)}.py:<module>.$funcOrModule", isConstructor)
        }
      }
    }

    /** Sets how an application method would be referred to locally.
      *
      * @param m
      *   an internal method
      */
    private def visitImport(m: Method): Unit = {
      val calleeNames = ProcedureInScope(m.name, m.fullName).possibleCalleeNames
      globalTable.put(m, calleeNames)
    }
  }

  /** Performs type recovery from the root of a compilation unit level
    * @param cu
    *   a compilation unit, e.g. file, procedure, type, etc.
    * @param builder
    *   the graph builder
    */
  class RecoverForCompilationUnit(cu: AstNode, builder: DiffGraphBuilder) extends RecursiveTask[Unit] {

    private val symbolTable = new SymbolTable()

    private def assignments: Traversal[Assignment] =
      cu.ast.isCall.name(Operators.assignment).map(new OpNodes.Assignment(_))

    override def compute(): Unit = try {
      // Conservatively populate local table with interprocedural knowledge
      // TODO: Only populate global knowledge that is in scope, i.e. imported
      symbolTable.from(globalTable)
      // Populate local symbol table with assignments
      assignments.foreach(visitAssignments)
      symbolTable.view.foreach { case (k, v) => println(k.idName, v) }
      // Persist findings
      setTypeInformation()
    } finally {
      symbolTable.clear()
    }

    /** Using assignment and import information (in the global symbol table), will propagate these types in the symbol
      * table.
      *
      * @param assignment
      *   assignment call pointer.
      */
    private def visitAssignments(assignment: Assignment): Unit = {
      // TODO: Handle fields being imported and loaded with a new value
      assignment.argumentOut.take(2).l match {
        case List(i: Identifier, c: Call) if globalTable.contains(c) =>
          val importedTypes = globalTable.get(c)
          if (!c.code.endsWith(")")) {
            // Case 1: The identifier is at the assignment to a function pointer. Lack of parenthesis should indicate this.
            symbolTable.append(i, importedTypes)
          } else if (c.name.charAt(0).isUpper && c.code.endsWith(")")) {
            // Case 2: The identifier is receiving a constructor invocation, thus is now an instance of the type
            symbolTable.append(i, importedTypes.map(_.stripSuffix(s".${Defines.ConstructorMethodName}")))
          } else {
            // TODO: This identifier should contain the type of the return value of 'c'
          }
        case List(i: Identifier, l: Literal) if Try(java.lang.Integer.parseInt(l.code)).isSuccess =>
          symbolTable.append(i, Set("int"))
        case List(i: Identifier, l: Literal) if Try(java.lang.Double.parseDouble(l.code)).isSuccess =>
          symbolTable.append(i, Set("float"))
        case List(i: Identifier, l: Literal) if "True".equals(l.code) || "False".equals(l.code) =>
          symbolTable.append(i, Set("bool"))
        case List(i: Identifier, l: Literal) if l.code.matches("^(\"|').*(\"|')$") =>
          symbolTable.append(i, Set("str"))
        case List(i: Identifier, c: Call) if c.name.equals("<operator>.listLiteral") =>
          symbolTable.append(i, Set("list"))
        case List(i: Identifier, c: Call) if c.name.equals("<operator>.tupleLiteral") =>
          symbolTable.append(i, Set("tuple"))
        case List(i: Identifier, b: Block)
            if b.astChildren.isCall.headOption.exists(
              _.argument.isCall.exists(_.name.equals("<operator>.dictLiteral"))
            ) =>
          symbolTable.append(i, Set("dict"))
        case _ =>
      }
    }

    /** Using an entry from the symbol table, will queue the CPG modification to persist the recovered type information.
      */
    private def setTypeInformation(): Unit = {
      cu.ast
        .foreach {
          case x: Local if symbolTable.contains(x) =>
            builder.setNodeProperty(x, PropertyNames.DYNAMIC_TYPE_HINT_FULL_NAME, symbolTable.get(x).toSeq)
          case x: Identifier if symbolTable.contains(x) =>
            (x.inCall.headOption, x.inCall.argument.take(2).l) match {
              // Case 1: 'call' is an assignment
              case (Some(call: Call), List(i: Identifier, c: Call)) if call.name.equals(Operators.assignment) =>
                val idTypes   = symbolTable.get(i)
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
              // Case 2: 'i' is the receiver of 'call'
              case (Some(call: Call), List(i: Identifier, _)) if !call.name.equals(Operators.fieldAccess) =>
                val idHints = symbolTable.get(i)
                persistType(i, idHints)(builder)
                persistType(call, idHints)(builder)
              // Case 3: 'i' is the receiver for a field access on member 'f'
              case (Some(call: Call), List(i: Identifier, f: FieldIdentifier))
                  if call.name.equals(Operators.fieldAccess) =>
                persistType(i, symbolTable.get(x))(builder)
              // TODO: Handle fields
              // Case 4: We are elsewhere
              case _ => persistType(x, symbolTable.get(x))(builder)
            }
          case x: Call if symbolTable.contains(x) =>
            builder.setNodeProperty(x, PropertyNames.DYNAMIC_TYPE_HINT_FULL_NAME, symbolTable.get(x).toSeq)
          case _ =>
        }
    }

    private def persistType(x: StoredNode, types: Set[String])(implicit builder: DiffGraphBuilder): Unit =
      if (types.nonEmpty)
        if (types.size == 1)
          builder.setNodeProperty(x, PropertyNames.TYPE_FULL_NAME, types.head)
        else
          builder.setNodeProperty(x, PropertyNames.DYNAMIC_TYPE_HINT_FULL_NAME, types.toSeq)

  }

  /** Defines how a procedure is available to be called in the current scope either by it being defined in this module
    * or being imported.
    *
    * @param callingName
    *   how this procedure is to be called, i.e., alias name, name with path, etc.
    * @param fullNameAsPyFile
    *   the full name to where this method is defined where it's assumed to be defined under a named Python file.
    */
  case class ProcedureInScope(callingName: String, fullNameAsPyFile: String, isConstructor: Boolean = false) {

    /** @return
      *   the full name of the procedure where it's assumed that it is defined within an <code>__init.py__</code> of the
      *   module.
      */
    private def fullNameAsInit: String = fullNameAsPyFile.replace(".py", s"${JFile.separator}__init__.py")

    /** @return
      *   the two ways that this procedure could be resolved to in Python. This will be pruned later by comparing this
      *   to actual methods in the CPG.
      */
    def possibleCalleeNames: Set[String] =
      if (isConstructor)
        Set(fullNameAsPyFile.concat(s".${Defines.ConstructorMethodName}"))
      else
        Set(fullNameAsPyFile, fullNameAsInit)

    override def toString: String = s"Either($fullNameAsPyFile or $fullNameAsInit)"
  }

  /** Represents an identifier at a specific scope.
    *
    * @param filename
    *   the file name in which this identifier belongs.
    * @param idName
    *   the name of the identifier.
    */
  case class SBKey(filename: String, idName: String) {

    override def equals(obj: Any): Boolean = {
      obj match {
        case node: CfgNode => this.equals(SBKey.fromNode(node))
        case o: SBKey      => filename.equals(o.filename) && idName.equals(o.idName)
        case _             => false
      }
    }

  }

  object SBKey {
    def fromNode(node: AstNode): SBKey = {
      val name = node match {
        case x: Identifier => x.name
        case x: Method     => x.name
        case x: Call       => x.name
        case x: Local      => x.name
        case x             => x.code
      }
      val filename = node.file.name.headOption match {
        case Some(fileName) => fileName
        case None =>
          logger.warn(
            s"Unable to successfully use file name for symbol table, type recovery may become more imprecise. Node: ${node.propertiesMap()}"
          ); ""
      }
      SBKey(filename, name)
    }

  }

  /** A thread-safe symbol table that can represent multiple types per symbol.
    */
  private class SymbolTable {

    import SBKey.fromNode

    private val table = TrieMap.empty[SBKey, Set[String]]

    def apply(sbKey: SBKey): Set[String] = table(sbKey)

    def apply(node: AstNode): Set[String] = table(fromNode(node))

    def from(sb: SymbolTable): SymbolTable = { table.addAll(sb.table); this }

    def put(sbKey: SBKey, typeFullNames: Set[String]): Option[Set[String]] =
      table.put(sbKey, typeFullNames)

    def put(node: AstNode, typeFullNames: Set[String]): Option[Set[String]] =
      put(fromNode(node), typeFullNames)

    def append(node: AstNode, typeFullName: String): Option[Set[String]] =
      append(node, Set(typeFullName))

    def append(node: AstNode, typeFullNames: Set[String]): Option[Set[String]] =
      append(fromNode(node), typeFullNames)

    private def append(sbKey: SBKey, typeFullNames: Set[String]): Option[Set[String]] = {
      table.get(sbKey) match {
        case Some(ts) => table.put(sbKey, ts ++ typeFullNames)
        case None     => table.put(sbKey, typeFullNames)
      }
    }

    private def contains(sbKey: SBKey): Boolean = table.contains(sbKey)

    def contains(node: AstNode): Boolean = contains(fromNode(node))

    private def get(sbKey: SBKey): Set[String] = table.getOrElse(sbKey, Set.empty)

    def get(node: AstNode): Set[String] = get(fromNode(node))

    def view: MapView[SBKey, Set[String]] = table.view

    def clear(): Unit = table.clear()

  }

}
