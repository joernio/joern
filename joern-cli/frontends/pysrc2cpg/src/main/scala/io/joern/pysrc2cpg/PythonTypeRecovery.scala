package io.joern.pysrc2cpg

import io.joern.pysrc2cpg.PythonTypeRecovery.BUILTIN_PREFIX
import io.joern.x2cpg.Defines
import io.joern.x2cpg.passes.frontend._
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._
import io.shiftleft.semanticcpg.language.operatorextension.OpNodes.FieldAccess
import overflowdb.BatchedUpdate.DiffGraphBuilder
import overflowdb.traversal.Traversal

import java.io.{File => JFile}
import java.util.regex.Matcher
import scala.util.Try

class PythonTypeRecovery(cpg: Cpg) extends XTypeRecovery[File](cpg) {

  override def compilationUnit: Traversal[File] = cpg.file

  override def generateRecoveryForCompilationUnitTask(
    unit: File,
    builder: DiffGraphBuilder
  ): RecoverForXCompilationUnit[File] = new RecoverForPythonFile(cpg, unit, builder, globalTable)

}

/** Performs type recovery from the root of a compilation unit level
  */
class RecoverForPythonFile(cpg: Cpg, cu: File, builder: DiffGraphBuilder, globalTable: SymbolTable[GlobalKey])
    extends RecoverForXCompilationUnit[File](cpg, cu, builder, globalTable) {

  /** Overridden to include legacy import calls until imports are supported.
    */
  override def importNodes(cu: AstNode): Traversal[AstNode] =
    cu.ast.isCall.nameExact("import") ++ super.importNodes(cu)

  override def visitImport(i: Call): Unit = {
    i.argumentOut.l match {
      case List(path: Literal, funcOrModule: Literal) =>
        val calleeNames = extractPossibleCalleeNames(path.code, funcOrModule.code)
        symbolTable.put(CallAlias(funcOrModule.code), calleeNames)
        symbolTable.put(LocalVar(funcOrModule.code), calleeNames)
      case List(path: Literal, funcOrModule: Literal, alias: Literal) =>
        val calleeNames =
          extractPossibleCalleeNames(path.code, funcOrModule.code, Option(alias.code))
        symbolTable.put(CallAlias(alias.code), calleeNames)
        symbolTable.put(LocalVar(alias.code), calleeNames)
      case x => logger.warn(s"Unknown import pattern: ${x.map(_.label).mkString(", ")}")
    }
  }

  /** For an import - given by its module path, the name of the imported function or module, and an optional alias -
    * determine the possible callee names.
    *
    * @param path
    *   the module path.
    * @param funcOrModule
    *   the name of the imported entity.
    * @param maybeAlias
    *   an optional alias given to the imported entity.
    * @return
    *   the possible callee names
    */
  private def extractPossibleCalleeNames(
    path: String,
    funcOrModule: String,
    maybeAlias: Option[String] = None
  ): Set[String] = {
    val sep = Matcher.quoteReplacement(JFile.separator)
    val procedureName = path match {
      case "" if funcOrModule.contains(".") =>
        // Case 1: Qualified path: import foo.bar => (bar.py or bar/__init.py)
        val splitFunc = funcOrModule.split("\\.")
        val name      = splitFunc.tail.mkString(".")
        s"${splitFunc(0)}.py:<module>.$name"
      case "" =>
        // Case 2: import of a module: import foo => (foo.py or foo/__init.py)
        s"$funcOrModule.py:<module>"
      case _ =>
        // TODO: This assumes importing from modules and never importing nested method
        // Case 3:  Import from module using alias, e.g. import bar from foo as faz
        s"${path.replaceAll("\\.", sep)}.py:<module>.$funcOrModule"
    }

    /** The two ways that this procedure could be resolved to in Python. */
    def possibleCalleeNames(procedureName: String, isConstructor: Boolean): Set[String] =
      if (isConstructor)
        Set(procedureName.concat(s".${Defines.ConstructorMethodName}"))
      else
        Set(procedureName, fullNameAsInit)

    /** the full name of the procedure where it's assumed that it is defined within an <code>__init.py__</code> of the
      * module.
      */
    def fullNameAsInit: String = procedureName.replace(".py", s"${JFile.separator}__init__.py")

    possibleCalleeNames(procedureName, isConstructor(funcOrModule))
  }

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
      // TODO: This is a bit of a bandaid compared to potentially having alias sensitivity.

      def fieldVar(path: String) = FieldVar(path.stripSuffix(s".${k.identifier}"), k.identifier)

      symbolTable.get(k).headOption match {
        case Some(path) if globalTable.contains(fieldVar(path)) =>
          symbolTable.replaceWith(k, LocalVar(k.identifier), globalTable.get(fieldVar(path)))
        case _ =>
      }
    }
  }

  /** Determines if a function call is a constructor by following the heuristic that Python classes are typically
    * camel-case and start with an upper-case character.
    */
  override def isConstructor(c: Call): Boolean =
    c.name.nonEmpty && c.name.charAt(0).isUpper && c.code.endsWith(")")

  def isConstructor(funcOrModule: String): Boolean =
    funcOrModule.split("\\.").lastOption.exists(_.charAt(0).isUpper)

  /** If the parent method is module then it can be used as a field.
    */
  override def isField(i: Identifier): Boolean =
    i.method.name.matches("(<module>|__init__)") || super.isField(i)

  override def visitIdentifierAssignedToOperator(i: Identifier, c: Call, operation: String): Set[String] = {
    operation match {
      case "<operator>.listLiteral"  => associateTypes(i, Set(s"$BUILTIN_PREFIX.list"))
      case "<operator>.tupleLiteral" => associateTypes(i, Set(s"$BUILTIN_PREFIX.tuple"))
      case "<operator>.dictLiteral"  => associateTypes(i, Set(s"$BUILTIN_PREFIX.dict"))
      case "<operator>.setLiteral"   => associateTypes(i, Set(s"$BUILTIN_PREFIX.set"))
      case Operators.conditional     => associateTypes(i, Set(s"$BUILTIN_PREFIX.bool"))
      case _                         => super.visitIdentifierAssignedToOperator(i, c, operation)
    }
  }

  override def visitIdentifierAssignedToConstructor(i: Identifier, c: Call): Set[String] = {
    val constructorPaths = symbolTable
      .get(c)
      .map(_.stripSuffix(s".${Defines.ConstructorMethodName}"))
      .map(x => (x.split("\\.").last, x))
      .map {
        case (x, y) => s"$y.$x<body>"
        case (_, z) => z
      }
    associateTypes(i, constructorPaths)
  }

  override def visitIdentifierAssignedToCall(i: Identifier, c: Call): Set[String] = {
    // Ignore legacy import representation
    if (c.name.equals("import")) Set.empty
    // Stop custom annotation representation from hitting superclass
    else if (c.name.isBlank) Set.empty
    else super.visitIdentifierAssignedToCall(i, c)
  }

  override def visitIdentifierAssignedToFieldLoad(i: Identifier, fa: FieldAccess): Set[String] = {
    val fieldParents = getFieldParents(fa)
    fa.astChildren.l match {
      case List(base: Identifier, fi: FieldIdentifier) if base.name.equals("self") && fieldParents.nonEmpty =>
        val globalTypes = fieldParents.flatMap(fp => globalTable.get(FieldVar(fp, fi.canonicalName)))
        associateTypes(i, globalTypes)
      case _ => super.visitIdentifierAssignedToFieldLoad(i, fa)
    }
  }

  override def getTypesFromCall(c: Call): Set[String] = c.name match {
    case "<operator>.listLiteral"  => Set(s"$BUILTIN_PREFIX.list")
    case "<operator>.tupleLiteral" => Set(s"$BUILTIN_PREFIX.tuple")
    case "<operator>.dictLiteral"  => Set(s"$BUILTIN_PREFIX.dict")
    case "<operator>.setLiteral"   => Set(s"$BUILTIN_PREFIX.set")
    case _                         => super.getTypesFromCall(c)
  }

  override def getFieldParents(fa: FieldAccess): Set[String] = {
    if (fa.method.name.equals("<module>")) {
      Set(fa.method.fullName)
    } else if (fa.method.typeDecl.nonEmpty) {
      val parentTypes =
        fa.method.typeDecl.fullName.map(_.stripSuffix("<meta>")).map { t => s"$t.${t.split("\\.").last}" }.toSeq
      val baseTypes = cpg.typeDecl.fullNameExact(parentTypes: _*).inheritsFromTypeFullName.toSeq
      // TODO: inheritsFromTypeFullName does not give full name in pysrc2cpg
      val baseTypeFullNames = cpg.typ.nameExact(baseTypes: _*).fullName.toSeq
      (parentTypes ++ baseTypeFullNames)
        .map(_.concat(".<body>"))
        .filterNot(t => t.toLowerCase.matches("(any|object)"))
        .toSet
    } else {
      super.getFieldParents(fa)
    }
  }

  override def getLiteralType(l: Literal): Set[String] = {
    l match {
      case _ if Try(java.lang.Integer.parseInt(l.code)).isSuccess   => Set(s"$BUILTIN_PREFIX.int")
      case _ if Try(java.lang.Double.parseDouble(l.code)).isSuccess => Set(s"$BUILTIN_PREFIX.float")
      case _ if "True".equals(l.code) || "False".equals(l.code)     => Set(s"$BUILTIN_PREFIX.bool")
      case _ if l.code.matches("^(\"|').*(\"|')$")                  => Set(s"$BUILTIN_PREFIX.str")
      case _ if l.code.equals("None")                               => Set(s"$BUILTIN_PREFIX.None")
      case _                                                        => Set()
    }
  }

}

object PythonTypeRecovery {
  def BUILTIN_PREFIX = "__builtin"
}
