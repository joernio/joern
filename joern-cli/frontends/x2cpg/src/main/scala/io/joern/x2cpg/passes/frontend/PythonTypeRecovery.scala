package io.joern.x2cpg.passes.frontend

import io.joern.x2cpg.Defines
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.codepropertygraph.generated.{Operators, PropertyNames}
import io.shiftleft.passes.CpgPass
import io.shiftleft.semanticcpg.language._
import io.shiftleft.semanticcpg.language.operatorextension.OpNodes.Assignment
import org.apache.logging.log4j.LogManager

import java.io.{File => JFile}
import java.util.concurrent.RecursiveTask
import java.util.regex.Matcher
import scala.collection.MapView
import scala.collection.concurrent.TrieMap

/** Based on a flow-insensitive symbol-table-style approach. This does not accurately determine the difference between
  * shadowed variables in the same file but this is due to REF edges not connecting children methods to parent scope
  * (yet).
  *
  * TODO: Currently this just improves the type recovery of imported function and types.
  */
class PythonTypeRecovery(cpg: Cpg) extends CpgPass(cpg) {

  private val symbolTable = new SymbolTable()
  private val logger      = LogManager.getLogger(classOf[PythonTypeRecovery])

  override def run(builder: DiffGraphBuilder): Unit =
    try {
      // Set aliases that point to imports for local and external methods/modules
      def importCalls = cpg.call.where(_.nameExact("import"))

      def internalMethods = cpg.method.isExternal(false)

      (importCalls ++ internalMethods).map(f => new SetImportTask(f).fork()).foreach(_.get())
      // Prune import names if the methods exist in the CPG
      pruneImports()
      // Find identifiers that have been declared using imports
      cpg.assignment.map(a => new SetDeclaredIdentifiersTask(a).fork()).foreach(_.get())
      // Persist findings
      symbolTable.view
        .map { case (varDecl, typeHints) => new PersistRecoveredTypeTask(builder, varDecl, typeHints).fork() }
        .foreach(_.get())
    } finally {
      symbolTable.clear
    }

  /** The initial import setting is over-approximated, so this step checks the CPG for any matches and prunes against
    * these findings. If there are no findings, it will leave the table as is. The latter is significant for external
    * types or methods.
    */
  private def pruneImports(): Unit = {
    symbolTable.view.foreach { case (k, v) =>
      val ms = cpg.method.fullNameExact(v.toSeq: _*).l
      val ts = cpg.typeDecl.fullNameExact(v.toSeq: _*).l
      if (ts.nonEmpty)
        symbolTable.put(k, ts.fullName.toSet)
      else if (ms.nonEmpty)
        symbolTable.put(k, ms.fullName.toSet)
      else {
        // This is likely external and we will ignore the init variant to be consistent
        symbolTable.put(k, symbolTable(k).filterNot(_.contains("__init__.py")))
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
        case x: Method => setImports(x)
        case x: Call   => setImports(x)
      }
    }

    /** Refers to the declared import information.
      *
      * @param importCall
      *   the call that imports entities into this scope.
      */
    private def setImports(importCall: Call): Unit = {
      val filename = importCall.file.name.head
      importCall.argumentOut.l match {
        case List(path: Literal, funcOrModule: Literal) =>
          val calleeNames = extractMethodDetailsFromImport(path.code, funcOrModule.code).possibleCalleeNames
          symbolTable.put(filename, funcOrModule.code, calleeNames)
        case List(path: Literal, funcOrModule: Literal, alias: Literal) =>
          val calleeNames =
            extractMethodDetailsFromImport(path.code, funcOrModule.code, Option(alias.code)).possibleCalleeNames
          symbolTable.put(filename, alias.code, calleeNames)
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
    private def setImports(m: Method): Unit = {
      val calleeNames = ProcedureInScope(m.name, m.fullName).possibleCalleeNames
      symbolTable.put(m.filename, m.name, calleeNames)
    }
  }

  /** Using assignment and import information (in the symbol table), will propagate these types in the symbol table.
    * @param assignment
    *   assignment call pointer.
    */
  class SetDeclaredIdentifiersTask(assignment: Assignment) extends RecursiveTask[Unit] {
    override def compute(): Unit = {
      // TODO: Handle fields being imported and loaded with a new value
      assignment.argumentOut.take(2).l match {
        case List(i: Identifier, c: Call) if symbolTable.contains(c.file.name.head, c.name) =>
          val importedTypes = symbolTable.get(c.file.name.head, c.name)
          if (!c.code.endsWith(")")) {
            // Case 1: The identifier is at the assignment to a function pointer. Lack of parenthesis should indicate this.
            symbolTable.put(c.file.name.head, i.name, importedTypes)
          } else if (c.name.charAt(0).isUpper && c.code.endsWith(")")) {
            // Case 2: The identifier is receiving a constructor invocation, thus is now an instance of the type
            symbolTable.put(
              c.file.name.head,
              i.name,
              importedTypes.map(_.stripSuffix(s".${Defines.ConstructorMethodName}"))
            )
          } else {
            // TODO: This identifier should contain the type of the return value of 'c'
          }
        case _ =>
      }
    }

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

  /** Using an entry from the symbol table, will queue the CPG modification to persist the recovered type information.
    *
    * @param builder
    *   the graph builder.
    * @param varDecl
    *   the variable declaration identifier.
    * @param typeHints
    *   the recovered type hints.
    */
  class PersistRecoveredTypeTask(builder: DiffGraphBuilder, varDecl: VarDecl, typeHints: Set[String])
      extends RecursiveTask[Unit] {
    override def compute(): Unit = {
      val VarDecl(filename, alias) = varDecl
      cpg.file
        .nameExact(filename)
        .method
        .ast
        .foreach {
          case x: Local if x.name.equals(alias) =>
            builder.setNodeProperty(x, PropertyNames.DYNAMIC_TYPE_HINT_FULL_NAME, typeHints.toSeq)
          case x: Identifier if x.name.equals(alias) =>
            (x.inCall.headOption, x.inCall.argument.take(2).l) match {
              // Case 1: 'call' is an assignment
              case (Some(call: Call), List(i: Identifier, c: Call)) if call.name.equals(Operators.assignment) =>
                val idTypes   = symbolTable.get(i.file.name.head, i.name)
                val callTypes = symbolTable.get(c.file.name.head, c.name)
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
                persistType(i, typeHints)(builder)
                persistType(call, typeHints)(builder)
              // Case 3: 'i' is the receiver for a field access on member 'f'
              case (Some(call: Call), List(i: Identifier, f: FieldIdentifier))
                  if call.name.equals(Operators.fieldAccess) =>
                persistType(i, typeHints)(builder)
              // TODO: Handle fields
              // Case 4: We are elsewhere
              case _ => persistType(x, typeHints)(builder)
            }
          case x: Call if x.name.equals(alias) =>
            builder.setNodeProperty(x, PropertyNames.DYNAMIC_TYPE_HINT_FULL_NAME, typeHints.toSeq)
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

  /** Represents the scope of an identifier.
    *
    * @param filename
    *   the file name in which this identifier belongs.
    * @param idName
    *   the name of the identifier.
    */
  case class VarDecl(filename: String, idName: String)

  /** A thread-safe symbol table that can represent multiple types per symbol.
    */
  private class SymbolTable {

    private val table = TrieMap.empty[VarDecl, Set[String]]

    def apply(varDecl: VarDecl): Set[String] = table(varDecl)

    def apply(filename: String, idName: String): Set[String] = table(key(filename, idName))

    private def key(filename: String, idName: String): VarDecl = VarDecl(filename, idName)

    def append(varDecl: VarDecl, typeFullNames: Set[String]): Option[Set[String]] =
      table.get(varDecl) match {
        case Some(types) => table.put(varDecl, types ++ typeFullNames)
        case None        => table.put(varDecl, typeFullNames)
      }

    def put(varDecl: VarDecl, typeFullNames: Set[String]): Option[Set[String]] =
      table.put(varDecl, typeFullNames)

    def put(filename: String, idName: String, typeFullNames: Set[String]): Option[Set[String]] =
      put(key(filename, idName), typeFullNames)

    private def contains(varDecl: VarDecl): Boolean = table.contains(varDecl)

    def contains(filename: String, idName: String): Boolean = contains(key(filename, idName))

    private def get(varDecl: VarDecl): Set[String] = table.getOrElse(varDecl, Set.empty)

    def get(filename: String, idName: String): Set[String] = get(key(filename, idName))

    def view: MapView[VarDecl, Set[String]] = table.view

    def clear: Unit = table.clear()

  }

}
