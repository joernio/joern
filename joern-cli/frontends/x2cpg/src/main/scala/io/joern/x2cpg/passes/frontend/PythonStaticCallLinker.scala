package io.joern.x2cpg.passes.frontend

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.PropertyNames
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Method}
import io.shiftleft.passes.SimpleCpgPass
import io.shiftleft.proto.cpg.Cpg.DispatchTypes
import io.shiftleft.semanticcpg.language._

/** Attempts to set the <code>methodFullName</code> and <code>dispatchType</code> properties of "static" calls.
  * @param cpg
  *   the target code property graph.
  */
class PythonStaticCallLinker(cpg: Cpg) extends SimpleCpgPass(cpg) {

  override def run(builder: DiffGraphBuilder): Unit =
    cpg.method.where(_.nameExact("<module>")).foreach(module => runOnModule(module, builder))

  def runOnModule(module: Method, builder: DiffGraphBuilder): Unit = {
    val imports        = module.call.where(_.nameExact("import")).l
    val methodsInScope = imports.map(extractMethodFromImport).map(f => f.callingName -> f).toMap
    module.call
      .whereNot(_.nameExact("import"))
      .foreach(c =>
        methodsInScope.get(c.name) match {
          case Some(procInScope: ProcInScope) =>
            builder.setNodeProperty(c, PropertyNames.METHOD_FULL_NAME, procInScope.fullName)
            builder.setNodeProperty(c, PropertyNames.DISPATCH_TYPE, DispatchTypes.STATIC_DISPATCH.name())
          case None => // "out of scope" call
        }
      )
  }

  /** Parses all imports and identifies their full names and how they are to be called in this scope.
    * @param i
    *   the import call.
    * @return
    *   the procedure information in this scope.
    */
  def extractMethodFromImport(i: Call): ProcInScope = {
    val astChildren = i.astChildren.l
    val module      = astChildren(1).code
    val func        = astChildren(2).code
    val maybeAlias  = if (astChildren.size >= 4) Some(astChildren(3).code) else None
    if (module.isEmpty) {
      if (func.contains(".")) {
        // Case 1: We have imported a function using a qualified path, e.g., import foo.bar
        val splitFunc = func.split("\\.")
        val name      = splitFunc.tail.mkString(".")
        ProcInScope(name, s"${splitFunc(0)}.py:<module>.$name")
      } else {
        // Case 2: We have imported a module, e.g., import foo
        ProcInScope(func, s"$func.py:<module>")
      }
    } else {
      maybeAlias match {
        // TODO: This assumes importing from modules and never importing nested method
        // Case 3:  We have imported a function from a module using an alias, e.g. import bar from foo as faz
        case Some(alias) => ProcInScope(alias, s"${module.replaceAll("\\.", "/")}.py:<module>.$func")
        // Case 4: We have imported a function from a module, e.g. import bar from foo
        case None => ProcInScope(func, s"${module.replaceAll("\\.", "/")}.py:<module>.$func")
      }
    }
  }

  /** Defines how a procedure is available to be called in the current scope.
    * @param callingName
    *   how this procedure is to be called, i.e., alias name, name with path, etc.
    * @param fullName
    *   the full name to where this method is defined, i.e., the defining Method node's full name.
    */
  case class ProcInScope(callingName: String, fullName: String)

}
