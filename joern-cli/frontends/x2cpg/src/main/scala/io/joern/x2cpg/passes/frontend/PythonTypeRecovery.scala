package io.joern.x2cpg.passes.frontend

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.PropertyNames
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.passes.CpgPass
import io.shiftleft.semanticcpg.language._
import io.shiftleft.semanticcpg.language.operatorextension.OpNodes.Assignment
import org.apache.logging.log4j.LogManager

import java.io.{File => JFile}
import java.util.regex.Matcher
import scala.collection.MapView
import scala.collection.concurrent.TrieMap

/** Based on a flow-insensitive symbol-table-style approach. This does not accurately determine the difference between
  * shadowed variables in the same file but this is due to REF edges not connecting children methods to parent scope
  * (yet).
  *
  * TODO: Currently this just improves the type recovery of imported functions and fields.
  */
class PythonTypeRecovery(cpg: Cpg) extends CpgPass(cpg) {

  private val symbolTable = new SymbolTable()
  private val logger      = LogManager.getLogger(classOf[PythonTypeRecovery])

  override def run(builder: DiffGraphBuilder): Unit = {
    // Set aliases that point to imports for local and external methods/modules
    // TODO: Parallelize
    cpg.call.where(_.nameExact("import")).foreach(setImports)
    cpg.method.isExternal(false).foreach(setImports)
    // Prune import names if the methods exist in the CPG
    pruneImports()
    // TODO: Join
    // TODO: Parallelize
    // Find identifiers that have been declared using imports
    cpg.assignment.foreach(setDeclaredIdentifierTypes)

    symbolTable.view.foreach { case (VarDecl(f, n), v) => println((f, n, v)) }

    // TODO: Join
    // Persist findings
    // TODO: Parallelize + Join
    persistVarTypes(builder)
  }

  private def setDeclaredIdentifierTypes(assignment: Assignment): Unit = {
    // TODO: Handle fields being imported and loaded with a new value
    assignment.argumentOut.take(2).l match {
      case List(x: Identifier, y: Call) if symbolTable.contains(y.file.name.head, y.name) =>
        val importedTypes = symbolTable.get(y.file.name.head, y.name)
        symbolTable.put(y.file.name.head, x.name, importedTypes)
      case _ =>
    }
  }

  /** With the findings on the symbol table, will set the identifier type names appropriately.
    * @param builder
    *   the builder.
    */
  private def persistVarTypes(builder: DiffGraphBuilder): Unit =
    symbolTable.view.foreach { case (VarDecl(filename, alias), typeHints) =>
      cpg
        .file(filename)
        .method
        .ast
        .collect {
          case x: Local if x.name.equals(alias)      => x
          case x: Identifier if x.name.equals(alias) => x
        }
        .foreach { i =>
          i.inAssignment.argument.l match {
            // Case 1: The identifier is at the assignment, and the RHS is only 1 type
            case List(i: Identifier, c: Call) if symbolTable.get(c.file.name.head, c.name).size == 1 =>
              builder.setNodeProperty(i, PropertyNames.TYPE_FULL_NAME, typeHints.head)
            // Case 2: The identifier is elsewhere, but we are not tracking flow so it could be any of the types
            case _ => builder.setNodeProperty(i, PropertyNames.DYNAMIC_TYPE_HINT_FULL_NAME, typeHints.toSeq)
          }
        }
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

  /** Sets how an application method would be referred to locally.
    * @param m
    *   an internal method
    */
  private def setImports(m: Method): Unit = {
    val calleeNames = ProcInScope(m.name, m.fullName).possibleCalleeNames
    symbolTable.put(m.filename, m.name, calleeNames)
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
  ): ProcInScope = {
    if (path.isEmpty) {
      if (funcOrModule.contains(".")) {
        // Case 1: We have imported a function using a qualified path, e.g., import foo.bar => (bar.py or bar/__init.py)
        val splitFunc = funcOrModule.split("\\.")
        val name      = splitFunc.tail.mkString(".")
        ProcInScope(name, s"${splitFunc(0)}.py:<module>.$name")
      } else {
        // Case 2: We have imported a module, e.g., import foo => (foo.py or foo/__init.py)
        ProcInScope(funcOrModule, s"$funcOrModule.py:<module>")
      }
    } else {
      val sep = Matcher.quoteReplacement(JFile.separator)
      maybeAlias match {
        // TODO: This assumes importing from modules and never importing nested method
        // Case 3:  We have imported a function from a module using an alias, e.g. import bar from foo as faz
        case Some(alias) => ProcInScope(alias, s"${path.replaceAll("\\.", sep)}.py:<module>.$funcOrModule")
        // Case 4: We have imported a function from a module, e.g. import bar from foo
        case None => ProcInScope(funcOrModule, s"${path.replaceAll("\\.", sep)}.py:<module>.$funcOrModule")
      }
    }
  }

  case class VarDecl(filename: String, idName: String)

  /** Defines how a procedure is available to be called in the current scope.
    *
    * @param callingName
    *   how this procedure is to be called, i.e., alias name, name with path, etc.
    * @param fullNameAsPyFile
    *   the full name to where this method is defined where it's assumed to be defined under a named Python file.
    */
  case class ProcInScope(callingName: String, fullNameAsPyFile: String) {

    /** @return
      *   the full name of the procedure where it's assumed that it is defined within an <code>__init.py__</code> of the
      *   module.
      */
    def fullNameAsInit: String = fullNameAsPyFile.replace(".py", s"${JFile.separator}__init__.py")

    /** @return
      *   the two ways that this procedure could be resolved to in Python. This will be pruned later by comparing this
      *   to actual methods in the CPG.
      */
    def possibleCalleeNames: Set[String] = Set(fullNameAsPyFile, fullNameAsInit)
    override def toString: String        = s"Either($fullNameAsPyFile or $fullNameAsInit)"
  }

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

    def append(filename: String, idName: String, typeFullName: String): Option[Set[String]] =
      append(key(filename, idName), Set(typeFullName))

    def append(filename: String, idName: String, typeFullNames: Set[String]): Option[Set[String]] =
      append(key(filename, idName), typeFullNames)

    def put(varDecl: VarDecl, typeFullNames: Set[String]): Option[Set[String]] =
      table.put(varDecl, typeFullNames)

    def put(filename: String, idName: String, typeFullName: String): Option[Set[String]] =
      put(key(filename, idName), Set(typeFullName))

    def put(filename: String, idName: String, typeFullNames: Set[String]): Option[Set[String]] =
      put(key(filename, idName), typeFullNames)

    def contains(varDecl: VarDecl): Boolean = table.contains(varDecl)

    def contains(filename: String, idName: String): Boolean = contains(key(filename, idName))

    def get(varDecl: VarDecl): Set[String] = table.getOrElse(varDecl, Set.empty)

    def get(filename: String, idName: String): Set[String] = get(key(filename, idName))

    def clear: Unit = table.clear

    def view: MapView[VarDecl, Set[String]] = table.view

    def keys: Iterable[VarDecl] = table.keys

  }

}
