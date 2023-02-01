package io.joern.pysrc2cpg

import io.joern.x2cpg.Defines
import io.joern.x2cpg.passes.frontend._
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._
import io.shiftleft.semanticcpg.language.operatorextension.OpNodes
import io.shiftleft.semanticcpg.language.operatorextension.OpNodes.{Assignment, FieldAccess}
import overflowdb.BatchedUpdate.DiffGraphBuilder
import overflowdb.traversal.Traversal

import java.io.{File => JFile}
import java.util.regex.Matcher
import scala.util.Try

class PythonTypeRecovery(cpg: Cpg) extends XTypeRecovery[File](cpg) {

  override def computationalUnit: Traversal[File] = cpg.file
  override def generateRecoveryForCompilationUnitTask(
    unit: File,
    builder: DiffGraphBuilder,
    globalTable: SymbolTable[GlobalKey]
  ): RecoverForXCompilationUnit[File] = new RecoverForPythonFile(cpg, unit, builder, globalTable)

}

/** Defines how a procedure is available to be called in the current scope either by it being defined in this module or
  * being imported.
  *
  * @param callingName
  *   how this procedure is to be called, i.e., alias name, name with path, etc.
  * @param fullName
  *   the full name to where this method is defined where it's assumed to be defined under a named Python file.
  */
class ScopedPythonProcedure(callingName: String, fullName: String, isConstructor: Boolean = false)
    extends ScopedXProcedure(callingName, fullName, isConstructor) {

  /** @return
    *   the full name of the procedure where it's assumed that it is defined within an <code>__init.py__</code> of the
    *   module.
    */
  private def fullNameAsInit: String = fullName.replace(".py", s"${JFile.separator}__init__.py")

  /** @return
    *   the two ways that this procedure could be resolved to in Python. This will be pruned later by comparing this to
    *   actual methods in the CPG.
    */
  override def possibleCalleeNames: Set[String] =
    if (isConstructor)
      Set(fullName.concat(s".${Defines.ConstructorMethodName}"))
    else
      Set(fullName, fullNameAsInit)

}

/** Tasks responsible for populating the symbol table with import data and method definition data.
  *
  * @param node
  *   a node that references import information.
  */
class SetPythonProcedureDefTask(node: CfgNode, symbolTable: SymbolTable[LocalKey]) extends SetXProcedureDefTask(node) {

  /** Refers to the declared import information.
    *
    * @param importCall
    *   the call that imports entities into this scope.
    */
  override def visitImport(importCall: Call): Unit = {
    importCall.argumentOut.l match {
      case List(path: Literal, funcOrModule: Literal) =>
        val calleeNames = extractMethodDetailsFromImport(path.code, funcOrModule.code).possibleCalleeNames
        symbolTable.put(CallAlias(funcOrModule.code), calleeNames)
      case List(path: Literal, funcOrModule: Literal, alias: Literal) =>
        val calleeNames =
          extractMethodDetailsFromImport(path.code, funcOrModule.code, Option(alias.code)).possibleCalleeNames
        symbolTable.put(CallAlias(alias.code), calleeNames)
      case x => logger.warn(s"Unknown import pattern: ${x.map(_.label).mkString(", ")}")
    }
  }

  override def visitImport(m: Method): Unit = {
    val calleeNames = new ScopedPythonProcedure(m.name, m.fullName).possibleCalleeNames
    symbolTable.put(m, calleeNames)
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
  ): ScopedXProcedure = {
    val isConstructor = funcOrModule.split("\\.").last.charAt(0).isUpper
    if (path.isEmpty) {
      if (funcOrModule.contains(".")) {
        // Case 1: We have imported a function using a qualified path, e.g., import foo.bar => (bar.py or bar/__init.py)
        val splitFunc = funcOrModule.split("\\.")
        val name      = splitFunc.tail.mkString(".")
        new ScopedPythonProcedure(name, s"${splitFunc(0)}.py:<module>.$name", isConstructor)
      } else {
        // Case 2: We have imported a module, e.g., import foo => (foo.py or foo/__init.py)
        new ScopedPythonProcedure(funcOrModule, s"$funcOrModule.py:<module>", isConstructor)
      }
    } else {
      val sep = Matcher.quoteReplacement(JFile.separator)
      maybeAlias match {
        // TODO: This assumes importing from modules and never importing nested method
        // Case 3:  We have imported a function from a module using an alias, e.g. import bar from foo as faz
        case Some(alias) =>
          new ScopedPythonProcedure(alias, s"${path.replaceAll("\\.", sep)}.py:<module>.$funcOrModule", isConstructor)
        // Case 4: We have imported a function from a module, e.g. import bar from foo
        case None =>
          new ScopedPythonProcedure(
            funcOrModule,
            s"${path.replaceAll("\\.", sep)}.py:<module>.$funcOrModule",
            isConstructor
          )
      }
    }
  }

}

/** Performs type recovery from the root of a compilation unit level
  *
  * @param cu
  *   a compilation unit, e.g. file.
  * @param builder
  *   the graph builder
  */
class RecoverForPythonFile(cpg: Cpg, cu: File, builder: DiffGraphBuilder, globalTable: SymbolTable[GlobalKey])
    extends RecoverForXCompilationUnit[File](cpg, cu, builder, globalTable) {

  /** Adds built-in functions to expect.
    */
  override def prepopulateSymbolTable(): Unit =
    PythonTypeRecovery.BUILTINS
      .map(t => (CallAlias(t), s"${PythonTypeRecovery.BUILTIN_PREFIX}.$t"))
      .foreach { case (alias, typ) =>
        symbolTable.put(alias, typ)
      }

  override def importNodes(cu: AstNode): Traversal[CfgNode] = cu.ast.isCall.nameExact("import")

  override def postVisitImports(): Unit = {
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

      // Imports are by default used as calls, a second pass will tell us if this is not the case and we should
      // check against global table
      // TODO: This is a bit of a bandaid compared to potentially having alias sensitivity. Values could be an
      //  Either[SBKey, Set[String] where Left[SBKey] could point to the aliased symbol

      def fieldVar(path: String) = FieldVar(path.stripSuffix(s".${k.identifier}"), k.identifier)

      symbolTable.get(k).headOption match {
        case Some(path) if globalTable.contains(fieldVar(path)) =>
          symbolTable.replaceWith(k, LocalVar(k.identifier), globalTable.get(fieldVar(path)))
        case _ =>
      }
    }
  }

  override def generateSetProcedureDefTask(node: CfgNode, symbolTable: SymbolTable[LocalKey]): SetXProcedureDefTask =
    new SetPythonProcedureDefTask(node, symbolTable)

  /** Using assignment and import information (in the global symbol table), will propagate these types in the symbol
    * table.
    *
    * @param assignment
    *   assignment call pointer.
    */
  override def visitAssignments(assignment: Assignment): Unit = {
    // TODO: Handle fields being imported and loaded with a new value
    assignment.argumentOut.take(2).l match {
      case List(i: Identifier, c: Call) if symbolTable.contains(c) =>
        val importedTypes = symbolTable.get(c)
        if (!c.code.endsWith(")")) {
          // Case 1: The identifier is at the assignment to a function pointer. Lack of parenthesis should indicate this.
          setIdentifier(i, importedTypes)
        } else if (c.name.charAt(0).isUpper && c.code.endsWith(")")) {
          // Case 2: The identifier is receiving a constructor invocation, thus is now an instance of the type
          setIdentifier(i, importedTypes.map(_.stripSuffix(s".${Defines.ConstructorMethodName}")))
        } else {
          // TODO: This identifier should contain the type of the return value of 'c'
        }
      case List(i: Identifier, c: CfgNode)
          if visitLiteralAssignment(i, c, symbolTable) => // if unsuccessful, then check next
      case List(i: Identifier, c: Call) if c.receiver.isCall.name.exists(_.equals(Operators.fieldAccess)) =>
        val field = c.receiver.isCall.name(Operators.fieldAccess).map(new OpNodes.FieldAccess(_)).head
        visitCallFromFieldMember(i, c, field, symbolTable)
      // Use global table knowledge (in i >= 2 iterations) or CPG to extract field types
      case List(_: Identifier, c: Call) if c.name.equals(Operators.fieldAccess) =>
        c.inCall.argument
          .flatMap {
            case n: Call if n.name.equals(Operators.fieldAccess) => new OpNodes.FieldAccess(n).argumentOut
            case n                                               => n
          }
          .take(3)
          .l match {
          case List(assigned: Identifier, i: Identifier, f: FieldIdentifier)
              if symbolTable.contains(CallAlias(i.name)) =>
            // Get field from global table if referenced as function call
            val fieldTypes = symbolTable
              .get(CallAlias(i.name))
              .flatMap(recModule => globalTable.get(FieldVar(recModule, f.canonicalName)))
            symbolTable.append(assigned, fieldTypes)
          case List(assigned: Identifier, i: Identifier, f: FieldIdentifier)
              if symbolTable
                .contains(LocalVar(i.name)) =>
            // Get field from global table if referenced as a variable
            val localTypes = symbolTable.get(LocalVar(i.name))
            val memberTypes = localTypes
              .flatMap { t =>
                cpg.typeDecl.fullNameExact(t).member.nameExact(f.canonicalName).l ++
                  cpg.typeDecl.fullNameExact(t).method.fullNameExact(t).l
              }
              .flatMap {
                case m: Member => Some(m.typeFullName)
                case m: Method => Some(m.fullName)
                case _         => None
              }
            if (memberTypes.nonEmpty)
              // First use the member type info from the CPG, if present
              symbolTable.append(assigned, memberTypes)
            else if (localTypes.nonEmpty) {
              // If not available, use a dummy variable that can be useful for call matching
              symbolTable.append(assigned, localTypes.map { t => s"$t.<member>(${f.canonicalName})" })
            }
          case _ =>
        }
      case _ =>
    }
  }

  private def setIdentifier(i: Identifier, types: Set[String]): Option[Set[String]] = {
    if (i.method.name.equals("<module>")) globalTable.put(i, types)
    symbolTable.append(i, types)
  }

  private def visitCallFromFieldMember(
    i: Identifier,
    c: Call,
    field: FieldAccess,
    symbolTable: SymbolTable[LocalKey]
  ): Unit = {
    field.astChildren.l match {
      case List(rec: Identifier, f: FieldIdentifier) if symbolTable.contains(rec) =>
        val identifierFullName = symbolTable.get(rec).map(_.concat(s".${f.canonicalName}"))
        val callMethodFullName =
          if (f.canonicalName.charAt(0).isUpper)
            identifierFullName.map(_.concat(s".${Defines.ConstructorMethodName}"))
          else
            identifierFullName
        symbolTable.put(i, identifierFullName)
        symbolTable.put(c, callMethodFullName)
      case _ =>
    }
  }

  /** Will handle literal value assignments.
    * @param lhs
    *   the identifier.
    * @param rhs
    *   the literal.
    * @param symbolTable
    *   the symbol table.
    * @return
    *   true if a literal assigment was successfully determined and added to the symbol table, false if otherwise.
    */
  private def visitLiteralAssignment(lhs: Identifier, rhs: CfgNode, symbolTable: SymbolTable[LocalKey]): Boolean = {
    ((lhs, rhs) match {
      case (i: Identifier, l: Literal) if Try(java.lang.Integer.parseInt(l.code)).isSuccess =>
        setIdentifier(i, Set("int"))
      case (i: Identifier, l: Literal) if Try(java.lang.Double.parseDouble(l.code)).isSuccess =>
        setIdentifier(i, Set("float"))
      case (i: Identifier, l: Literal) if "True".equals(l.code) || "False".equals(l.code) =>
        setIdentifier(i, Set("bool"))
      case (i: Identifier, l: Literal) if l.code.matches("^(\"|').*(\"|')$") =>
        setIdentifier(i, Set("str"))
      case (i: Identifier, c: Call) if c.name.equals("<operator>.listLiteral") =>
        setIdentifier(i, Set("list"))
      case (i: Identifier, c: Call) if c.name.equals("<operator>.tupleLiteral") =>
        setIdentifier(i, Set("tuple"))
      case (i: Identifier, b: Block)
          if b.astChildren.isCall.headOption.exists(
            _.argument.isCall.exists(_.name.equals("<operator>.dictLiteral"))
          ) =>
        setIdentifier(i, Set("dict"))
      case _ => None
    }).hasNext
  }

}

object PythonTypeRecovery {

  /** @see
    *   <a href="https://docs.python.org/3/library/functions.html#func-dict">Python Built-in Functions</a>
    */
  lazy val BUILTINS: Set[String] = Set(
    "abs",
    "aiter",
    "all",
    "anext",
    "ascii",
    "bin",
    "bool",
    "breakpoint",
    "bytearray",
    "bytes",
    "callable",
    "chr",
    "classmethod",
    "compile",
    "complex",
    "delattr",
    "dict",
    "dir",
    "divmod",
    "enumerate",
    "eval",
    "exec",
    "filter",
    "float",
    "format",
    "frozenset",
    "getattr",
    "globals",
    "hasattr",
    "hash",
    "help",
    "hex",
    "id",
    "input",
    "int",
    "isinstance",
    "issubclass",
    "iter",
    "len",
    "list",
    "locals",
    "map",
    "max",
    "memoryview",
    "min",
    "next",
    "object",
    "oct",
    "open",
    "ord",
    "pow",
    "print",
    "property",
    "range",
    "repr",
    "reversed",
    "round",
    "set",
    "setattr",
    "slice",
    "sorted",
    "staticmethod",
    "str",
    "sum",
    "super",
    "tuple",
    "type",
    "vars",
    "zip",
    "__import__"
  )
  def BUILTIN_PREFIX = "builtins.py:<module>"
}
