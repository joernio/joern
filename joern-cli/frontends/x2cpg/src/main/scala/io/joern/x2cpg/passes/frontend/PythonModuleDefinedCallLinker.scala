package io.joern.x2cpg.passes.frontend

import io.joern.x2cpg.Defines
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, Operators, PropertyNames}
import io.shiftleft.passes.CpgPass
import io.shiftleft.semanticcpg.language._

import java.io.File
import java.util.regex.Matcher
import scala.collection.mutable

/** Attempts to set the <code>methodFullName</code> and <code>dispatchType</code> properties of calls where the callees
  * are defined under a module.
  *
  * @param cpg
  *   the target code property graph.
  */
class PythonModuleDefinedCallLinker(cpg: Cpg) extends CpgPass(cpg) {

  override def run(builder: DiffGraphBuilder): Unit =
    cpg.method.where(_.nameExact("<module>")).foreach(module => runOnModule(module, builder))

  private def runOnModule(module: Method, builder: DiffGraphBuilder): Unit = {
    val imports = module.call.where(_.nameExact("import")).l
    val methodsInScope: Map[String, ProcInScope] =
      imports.map(extractMethodFromImport).map(f => f.callingName -> f).toMap ++
        extractLocallyDefinedMethods(module)
    val symbolTable = propagateImportedTypeToIdentifiers(module, methodsInScope, builder)
    speculativelyLinkCallsOnIdentifiers(module, symbolTable, builder)
    linkDirectCallsOnImportedFunctions(module, methodsInScope, builder)
  }

  /** Links all calls directly from imports in this scope.
    * @param module
    *   the procedure scope.
    * @param methodsInScope
    *   methods defined/imported into this module.
    * @param builder
    *   graph builder.
    */
  private def linkDirectCallsOnImportedFunctions(
    module: Method,
    methodsInScope: Map[String, ProcInScope],
    builder: DiffGraphBuilder
  ): Unit = {
    module.ast
      .collect { case call: Call if !call.name.equals("import") => call }
      .foreach(c =>
        methodsInScope.get(c.name) match {
          case Some(procInScope: ProcInScope) => linkCalleeIfPresent(c, procInScope, builder)
          case None                           => // "out of scope" call
        }
      )
  }

  /** Uses assignments to some imported function to set type information on the referenced identifiers.
    * @param module
    *   the procedure scope.
    * @param methodsInScope
    *   methods defined/imported into this module.
    * @param builder
    *   graph builder.
    * @return
    *   a symbol table/abstract interpreter of declaration and their potential assigned imported types/functions.
    */
  private def propagateImportedTypeToIdentifiers(
    module: Method,
    methodsInScope: Map[String, ProcInScope],
    builder: DiffGraphBuilder
  ): Map[Declaration, Set[String]] = {
    val symbolTable = mutable.HashMap.empty[Declaration, Set[String]]
    module.ast
      .collect {
        case call: Call if call.name.equals(Operators.assignment) =>
          (call.argument.argumentIndex(1).headOption, call.argument.argumentIndex(2).headOption)
      }
      .foreach {
        case (Some(id: Identifier), Some(call: Call)) if methodsInScope.contains(call.name) =>
          val procInScope = methodsInScope(call.name)
          builder.setNodeProperty(id, PropertyNames.TYPE_FULL_NAME, procInScope.fullNameAsPyFile)
          // Flow-insensitively mark the rest of the identifiers in this scope with the import info
          id.refsTo.flatMap(_._refIn).collectAll[Identifier].filterNot(_.equals(id)).foreach { i =>
            val newTypeHints = i.dynamicTypeHintFullName ++ List(procInScope.fullNameAsPyFile)
            id.refsTo.headOption match {
              case Some(decl) => symbolTable.put(decl, symbolTable.getOrElse(decl, Set()) ++ newTypeHints.toSet)
              case None       =>
            }
            builder.setNodeProperty(i, PropertyNames.DYNAMIC_TYPE_HINT_FULL_NAME, newTypeHints)
          }

        case (_, _) =>
      }
    symbolTable.toMap
  }

  /** With flow-insensitive information about the declaration types, will link calls from this identifier by combining
    * the type hint and call name.
    *
    * @param module
    *   the procedure scope.
    * @param symbolTable
    *   the symbol table about declarations within the procedure scope.
    * @param builder
    *   graph builder.
    */
  private def speculativelyLinkCallsOnIdentifiers(
    module: Method,
    symbolTable: Map[Declaration, Set[String]],
    builder: DiffGraphBuilder
  ): Unit = {
    module.ast
      .collect {
        case call: Call if !call.name.startsWith("<operator>") => (call, call.argument.argumentIndex(0).headOption)
      }
      .foreach {
        case (call, Some(id: Identifier)) if call.methodFullName.equals(Defines.DynamicCallUnknownFallName) =>
          val potentialCallees = id.refsTo.headOption match {
            case Some(decl) =>
              symbolTable
                .getOrElse(decl, Set())
                .filterNot(_.equals("ANY"))
                .map(_.concat(s".${call.name}"))
            case None => Set.empty[String]
          }
          potentialCallees
            .flatMap(c => cpg.method.fullNameExact(c))
            .foreach(m => builder.addEdge(call, m, EdgeTypes.CALL))
          // If there is only one potential callee, set this as method full name
          if (potentialCallees.size == 1)
            builder.setNodeProperty(call, PropertyNames.METHOD_FULL_NAME, potentialCallees.head)
        case (_, _) =>
      }
  }

  /** If we can find the method that this name belongs to, then link the call edge.
    *
    * @param call
    *   the source call node.
    * @param procInScope
    *   scope information about the callee.
    * @param builder
    *   the diff graph builder.
    */
  private def linkCalleeIfPresent(call: Call, procInScope: ProcInScope, builder: DiffGraphBuilder): Unit = {
    (cpg.method.fullNameExact(procInScope.fullNameAsPyFile).toSeq ++
      cpg.method.fullNameExact(procInScope.fullNameAsInit).toSeq).headOption match {
      // We give preference to the "as Py" version as it is a more precisely qualified name and what happens to be
      // chosen over the other in local experiments
      case Some(callee) =>
        builder.setNodeProperty(call, PropertyNames.METHOD_FULL_NAME, callee.fullName)
        builder.addEdge(call, callee, EdgeTypes.CALL)
      // This is not an application file, and thus may be external. We will label these all as py file imports
      case None if cpg.file.nameExact(procInScope.fileName).isEmpty =>
        builder.setNodeProperty(call, PropertyNames.METHOD_FULL_NAME, procInScope.fullNameAsPyFile)
      case None => // may be internal, but no existing method found
    }
  }

  /** Parses all imports and identifies their full names and how they are to be called in this scope.
    * @param i
    *   the import call.
    * @return
    *   the procedure information in this scope.
    */
  private def extractMethodFromImport(i: Call): ProcInScope = {
    val astChildren = i.astChildren.l
    val module      = astChildren(1).code
    val func        = astChildren(2).code
    val maybeAlias  = if (astChildren.size >= 4) Some(astChildren(3).code) else None
    if (module.isEmpty) {
      if (func.contains(".")) {
        // Case 1: We have imported a function using a qualified path, e.g., import foo.bar => (bar.py or bar/__init.py)
        val splitFunc = func.split("\\.")
        val name      = splitFunc.tail.mkString(".")
        ProcInScope(name, s"${splitFunc(0)}.py:<module>.$name")
      } else {
        // Case 2: We have imported a module, e.g., import foo => (foo.py or foo/__init.py)
        ProcInScope(func, s"$func.py:<module>")
      }
    } else {
      val sep = Matcher.quoteReplacement(File.separator)
      maybeAlias match {
        // TODO: This assumes importing from modules and never importing nested method
        // Case 3:  We have imported a function from a module using an alias, e.g. import bar from foo as faz
        case Some(alias) => ProcInScope(alias, s"${module.replaceAll("\\.", sep)}.py:<module>.$func")
        // Case 4: We have imported a function from a module, e.g. import bar from foo
        case None => ProcInScope(func, s"${module.replaceAll("\\.", sep)}.py:<module>.$func")
      }
    }
  }

  /** Returns all method declarations within this module that are not defined under a class definition.
    *
    * TODO: Some classes may annotate their methods as static
    * @param module
    *   the module to search within.
    * @return
    *   a mapping of the method names and their procedures.
    */
  private def extractLocallyDefinedMethods(module: Method): Map[String, ProcInScope] =
    // Collect procedure definitions not defined under a class (and thus may only be dynamic methods)
    module.ast
      .collect { case m: Method if !m.astParent.isInstanceOf[TypeDecl] => m }
      .map(m => m.name -> ProcInScope(m.name, m.fullName))
      .toMap

  /** Defines how a procedure is available to be called in the current scope.
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
    def fullNameAsInit: String = fullNameAsPyFile.replace(".py", s"${File.separator}__init__.py")

    def fileName: String = fullNameAsPyFile.split(":").head

    override def toString: String = s"Either($fullNameAsPyFile or $fullNameAsInit)"
  }

}
