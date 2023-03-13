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
import java.nio.file.Paths
import java.util.regex.Matcher
import scala.collection.mutable

class PythonTypeRecovery(cpg: Cpg) extends XTypeRecovery[File](cpg) {

  override def compilationUnit: Traversal[File] = cpg.file

  override def generateRecoveryForCompilationUnitTask(
    unit: File,
    builder: DiffGraphBuilder
  ): RecoverForXCompilationUnit[File] = new RecoverForPythonFile(cpg, unit, builder, globalTable, addedNodes)

}

/** Performs type recovery from the root of a compilation unit level
  */
class RecoverForPythonFile(
  cpg: Cpg,
  cu: File,
  builder: DiffGraphBuilder,
  globalTable: SymbolTable[GlobalKey],
  addedNodes: mutable.Set[(Long, String)]
) extends RecoverForXCompilationUnit[File](cpg, cu, builder, globalTable, addedNodes) {

  /** Replaces the `this` prefix with the Pythonic `self` prefix for instance methods of functions local to this
    * compilation unit.
    */
  private def fromNodeToLocalPythonKey(node: AstNode): Option[LocalKey] =
    node match {
      case n: Method => Option(CallAlias(n.name, Option("self")))
      case _         => SBKey.fromNodeToLocalKey(node)
    }

  override val symbolTable: SymbolTable[LocalKey] = new SymbolTable[LocalKey](fromNodeToLocalPythonKey)

  /** Overridden to include legacy import calls until imports are supported.
    */
  override def importNodes(cu: AstNode): Traversal[AstNode] =
    cu.ast.isCall.nameExact("import") ++ super.importNodes(cu)

  override def visitImport(importCall: Call): Unit = {
    importCall.argument.l match {
      case (path: Literal) :: (funcOrModule: Literal) :: alias =>
        val calleeNames = extractPossibleCalleeNames(path.code, funcOrModule.code)
        alias match {
          case (alias: Literal) :: Nil =>
            symbolTable.put(CallAlias(alias.code), calleeNames)
            symbolTable.put(LocalVar(alias.code), calleeNames)
          case Nil =>
            symbolTable.put(CallAlias(funcOrModule.code), calleeNames)
            symbolTable.put(LocalVar(funcOrModule.code), calleeNames)
          case x =>
            logger.warn(s"Unknown import pattern: ${x.map(_.label).mkString(", ")}")
        }
      case _ =>
    }
  }

  /** For an import - given by its module path and the name of the imported function or module - determine the possible
    * callee names.
    *
    * @param path
    *   the module path.
    * @param expEntity
    *   the name of the imported entity. This could be a function, module, or variable/field.
    * @return
    *   the possible callee names
    */
  private def extractPossibleCalleeNames(path: String, expEntity: String): Set[String] = {
    val sep = Matcher.quoteReplacement(JFile.separator)

    lazy val methodsWithExportEntityAsIdentifier: List[String] = cpg.typeDecl
      .fullName(s".*$path.*")
      .where(_.member.nameExact(expEntity))
      .fullName
      .toList

    val procedureName = path match {
      case "" if expEntity.contains(".") =>
        // Case 1: Qualified path: import foo.bar => (bar.py or bar/__init__.py)
        val splitFunc = expEntity.split("\\.")
        val name      = splitFunc.tail.mkString(".")
        s"${splitFunc(0)}.py:<module>.$name"
      case "" =>
        // Case 2: import of a module: import foo => (foo.py or foo/__init__.py)
        s"$expEntity.py:<module>"
      case _ if methodsWithExportEntityAsIdentifier.nonEmpty =>
        // Case 3: import of a variable: from api import db => (api.py or foo.__init__.py) @ identifier(db)
        methodsWithExportEntityAsIdentifier.map(f => s"$f<var>$expEntity").head
      case _ =>
        // Case 4:  Import from module using alias, e.g. import bar from foo as faz
        val rootDirectory = cpg.metaData.root.headOption
        val absPath       = rootDirectory.map(r => Paths.get(r, path))
        val fileOrDir     = absPath.map(a => better.files.File(a))
        val pyFile        = absPath.map(a => Paths.get(a.toString + ".py"))
        fileOrDir match {
          case Some(f) if f.isDirectory && !pyFile.exists { p => better.files.File(p).exists } =>
            s"${path.replaceAll("\\.", sep)}${java.io.File.separator}$expEntity.py:<module>"
          case Some(f) if f.isDirectory && (f / s"$expEntity.py").exists =>
            s"${(f / s"$expEntity.py").pathAsString}:<module>"
          case _ =>
            s"${path.replaceAll("\\.", sep)}.py:<module>.$expEntity"
        }
    }

    /** The two ways that this procedure could be resolved to in Python. */
    def possibleCalleeNames(procedureName: String, isConstructor: Boolean, isFieldOrVar: Boolean): Set[String] =
      if (isConstructor)
        Set(procedureName.concat(s".${Defines.ConstructorMethodName}"))
      else if (isFieldOrVar) {
        val Array(m, v) = procedureName.split("<var>")
        // TODO: When the workaround below is replaced, the following type->member check will kick in
        cpg.typeDecl.fullNameExact(m).member.nameExact(v).headOption match {
          case Some(i) => (i.typeFullName +: i.dynamicTypeHintFullName).filterNot(_ == "ANY").toSet
          case None    => Set.empty
        }
        // TODO: Workaround until we write-to-CPG each iteration, use this for now as long as we have global table
        cpg.method
          .fullNameExact(m)
          .ast
          .isIdentifier
          .nameExact(v)
          .dedupBy(_.name)
          .filter(globalTable.contains)
          .flatMap(globalTable.get)
          .toSet
      } else
        Set(procedureName, fullNameAsInit)

    /** the full name of the procedure where it's assumed that it is defined within an <code>__init.py__</code> of the
      * module.
      */
    def fullNameAsInit: String =
      if (procedureName.contains("__init__.py")) procedureName
      else procedureName.replace(".py", s"${JFile.separator}__init__.py")

    possibleCalleeNames(procedureName, isConstructor(expEntity), procedureName.contains("<var>"))
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
        case (x, y) if x.nonEmpty => s"$y.$x<body>"
        case (_, z)               => z
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

  private def isPyString(s: String): Boolean =
    (s.startsWith("\"") || s.startsWith("'")) && (s.endsWith("\"") || s.endsWith("'"))

  override def getLiteralType(l: Literal): Set[String] = {
    (l.code match {
      case code if code.toIntOption.isDefined                  => Some(s"$BUILTIN_PREFIX.int")
      case code if code.toDoubleOption.isDefined               => Some(s"$BUILTIN_PREFIX.float")
      case code if "True".equals(code) || "False".equals(code) => Some(s"$BUILTIN_PREFIX.bool")
      case code if code.equals("None")                         => Some(s"$BUILTIN_PREFIX.None")
      case code if isPyString(code)                            => Some(s"$BUILTIN_PREFIX.str")
      case _                                                   => None
    }).toSet
  }

}

object PythonTypeRecovery {
  def BUILTIN_PREFIX = "__builtin"
}
