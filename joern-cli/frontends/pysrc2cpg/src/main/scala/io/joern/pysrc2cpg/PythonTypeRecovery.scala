package io.joern.pysrc2cpg

import io.joern.x2cpg.Defines
import io.joern.x2cpg.passes.frontend._
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._
import io.shiftleft.semanticcpg.language.operatorextension.OpNodes
import io.shiftleft.semanticcpg.language.operatorextension.OpNodes.FieldAccess
import overflowdb.BatchedUpdate.DiffGraphBuilder
import overflowdb.traversal.Traversal

import java.io.{File => JFile}
import java.nio.file.Paths
import java.util.regex.{Matcher, Pattern}

class PythonTypeRecoveryPass(cpg: Cpg, config: XTypeRecoveryConfig = XTypeRecoveryConfig())
    extends XTypeRecoveryPass[File](cpg, config) {
  override protected def generateRecoveryPass(state: XTypeRecoveryState): XTypeRecovery[File] =
    new PythonTypeRecovery(cpg, state)
}

private class PythonTypeRecovery(cpg: Cpg, state: XTypeRecoveryState) extends XTypeRecovery[File](cpg, state) {

  override def compilationUnit: Traversal[File] = cpg.file

  override def generateRecoveryForCompilationUnitTask(
    unit: File,
    builder: DiffGraphBuilder
  ): RecoverForXCompilationUnit[File] =
    new RecoverForPythonFile(
      cpg,
      unit,
      builder,
      state.copy(config =
        state.config.copy(enabledDummyTypes = state.isFinalIteration && state.config.enabledDummyTypes)
      )
    )

}

/** Performs type recovery from the root of a compilation unit level
  */
private class RecoverForPythonFile(cpg: Cpg, cu: File, builder: DiffGraphBuilder, state: XTypeRecoveryState)
    extends RecoverForXCompilationUnit[File](cpg, cu, builder, state) {

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
    cu.ast.isCall.nameExact("import") // TODO: Remove and use IMPORT nodes

  override def visitImport(importCall: Call): Unit = {
    importCall.argument.l match {
      case (path: Literal) :: (funcOrModule: Literal) :: alias =>
        val namespace = if (path.code.startsWith(".")) {
          // TODO: pysrc2cpg does not link files to the correct namespace nodes
          val root     = cpg.metaData.root.headOption.getOrElse("")
          val fileName = path.file.name.headOption.getOrElse("").stripPrefix(root)
          val sep      = Matcher.quoteReplacement(JFile.separator)
          // The below gives us the full path of the relative "."
          val relativeNamespace =
            if (fileName.contains(JFile.separator))
              fileName.substring(0, fileName.lastIndexOf(JFile.separator)).replaceAll(sep, ".")
            else ""
          if (path.code.length > 1) relativeNamespace + path.code.replaceAll(sep, ".")
          else relativeNamespace
        } else path.code
        val calleeNames = extractPossibleCalleeNames(namespace, funcOrModule.code)
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

  override def visitAssignments(a: OpNodes.Assignment): Set[String] = {
    a.argumentOut.l match {
      case List(i: Identifier, c: Call) if c.name.isBlank && c.signature.isBlank =>
        // This is usually some decorator wrapper
        c.argument.isMethodRef.headOption match {
          case Some(mRef) => visitIdentifierAssignedToMethodRef(i, mRef)
          case None       => super.visitAssignments(a)
        }
      case _ => super.visitAssignments(a)
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
      .fullName(s".*${Pattern.quote(path)}.*")
      .where(_.member.nameExact(expEntity))
      .fullName
      .toList

    val procedureName = path match {
      case "" if expEntity.contains(".") =>
        // Case 1: Qualified path: import foo.bar => (bar.py or bar/__init__.py)
        val splitFunc = expEntity.split("\\.")
        val name      = splitFunc.tail.mkString(".")
        s"${splitFunc(0)}.py:<module>$pathSep$name"
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
            s"${path.replaceAll("\\.", sep)}.py:<module>$pathSep$expEntity"
        }
    }

    /** The two ways that this procedure could be resolved to in Python. */
    def possibleCalleeNames(procedureName: String, isMaybeConstructor: Boolean, isFieldOrVar: Boolean): Set[String] =
      if (isMaybeConstructor) Set(procedureName.concat(s"$pathSep${Defines.ConstructorMethodName}"))
      else if (isFieldOrVar) {
        val Array(m, v) = procedureName.split("<var>")
        cpg.typeDecl.fullNameExact(m).member.nameExact(v).headOption match {
          case Some(i) => (i.typeFullName +: i.dynamicTypeHintFullName).filterNot(_ == Constants.ANY).toSet
          case None    => Set.empty
        }
      } else
        Set(procedureName, fullNameAsInit)

    /** the full name of the procedure where it's assumed that it is defined within an <code>__init.py__</code> of the
      * module.
      */
    def fullNameAsInit: String =
      if (procedureName.contains("__init__.py")) procedureName
      else procedureName.replace(".py", s"${JFile.separator}__init__.py")

    val isMaybeConstructor = expEntity.split("\\.").lastOption.exists(s => s.nonEmpty && s.charAt(0).isUpper)
    possibleCalleeNames(procedureName, isMaybeConstructor, procedureName.contains("<var>"))
      .map(_.replaceAll("<var>", pathSep.toString))
  }

  override def postVisitImports(): Unit = {
    symbolTable.view.foreach { case (k, v) =>
      val ms = cpg.method.fullNameExact(v.toSeq: _*).l
      val ts = cpg.typeDecl.fullNameExact(v.toSeq: _*).l
      // In case a method has been incorrectly determined to be a constructor based on the heuristic
      val tsNonConstructor =
        cpg.method
          .fullNameExact(
            v.toSeq
              .map(_.replaceAll("<var>", pathSep.toString).stripSuffix(s"$pathSep${Defines.ConstructorMethodName}")): _*
          )
          .l
      if (ts.nonEmpty)
        symbolTable.put(k, ts.fullName.toSet)
      else if (ms.nonEmpty)
        symbolTable.put(k, ms.fullName.toSet)
      else if (tsNonConstructor.nonEmpty)
        symbolTable.put(k, tsNonConstructor.fullName.toSet)
      else {
        // This is likely external and we will ignore the init variant to be consistent
        symbolTable.put(k, symbolTable(k).filterNot(_.contains("__init__.py")))
      }
    }
  }

  /** Determines if a function call is a constructor by following the heuristic that Python classes are typically
    * camel-case and start with an upper-case character.
    */
  override def isConstructor(c: Call): Boolean =
    c.name.nonEmpty && c.name.charAt(0).isUpper && c.code.endsWith(")")

  /** If the parent method is module then it can be used as a field.
    */
  override def isField(i: Identifier): Boolean =
    state.isFieldCache.getOrElseUpdate(i.id(), i.method.name.matches("(<module>|__init__)") || super.isField(i))

  override def visitIdentifierAssignedToOperator(i: Identifier, c: Call, operation: String): Set[String] = {
    operation match {
      case "<operator>.listLiteral"  => associateTypes(i, Set(s"${PythonAstVisitor.builtinPrefix}list"))
      case "<operator>.tupleLiteral" => associateTypes(i, Set(s"${PythonAstVisitor.builtinPrefix}tuple"))
      case "<operator>.dictLiteral"  => associateTypes(i, Set(s"${PythonAstVisitor.builtinPrefix}dict"))
      case "<operator>.setLiteral"   => associateTypes(i, Set(s"${PythonAstVisitor.builtinPrefix}set"))
      case Operators.conditional     => associateTypes(i, Set(s"${PythonAstVisitor.builtinPrefix}bool"))
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
        val referencedFields = cpg.typeDecl.fullNameExact(fieldParents.toSeq: _*).member.nameExact(fi.canonicalName)
        val globalTypes =
          referencedFields.flatMap(m => m.typeFullName +: m.dynamicTypeHintFullName).filterNot(_ == Constants.ANY).toSet
        associateTypes(i, globalTypes)
      case _ => super.visitIdentifierAssignedToFieldLoad(i, fa)
    }
  }

  override def getTypesFromCall(c: Call): Set[String] = c.name match {
    case "<operator>.listLiteral"  => Set(s"${PythonAstVisitor.builtinPrefix}list")
    case "<operator>.tupleLiteral" => Set(s"${PythonAstVisitor.builtinPrefix}tuple")
    case "<operator>.dictLiteral"  => Set(s"${PythonAstVisitor.builtinPrefix}dict")
    case "<operator>.setLiteral"   => Set(s"${PythonAstVisitor.builtinPrefix}set")
    case _                         => super.getTypesFromCall(c)
  }

  override def getFieldParents(fa: FieldAccess): Set[String] = {
    if (fa.method.name == "<module>") {
      Set(fa.method.fullName)
    } else if (fa.method.typeDecl.nonEmpty) {
      val parentTypes =
        fa.method.typeDecl.fullName.map(_.stripSuffix("<meta>")).toSeq
      val baseTypes = cpg.typeDecl.fullNameExact(parentTypes: _*).inheritsFromTypeFullName.toSeq
      // TODO: inheritsFromTypeFullName does not give full name in pysrc2cpg
      val baseTypeFullNames = cpg.typ.nameExact(baseTypes: _*).fullName.toSeq
      (parentTypes ++ baseTypeFullNames)
        .map(_.concat("<meta>"))
        .filterNot(_.toLowerCase.matches("(any|object)"))
        .toSet
    } else {
      super.getFieldParents(fa)
    }
  }

  private def isPyString(s: String): Boolean =
    (s.startsWith("\"") || s.startsWith("'")) && (s.endsWith("\"") || s.endsWith("'"))

  override def getLiteralType(l: Literal): Set[String] = {
    (l.code match {
      case code if code.toIntOption.isDefined                  => Some(s"${PythonAstVisitor.builtinPrefix}int")
      case code if code.toDoubleOption.isDefined               => Some(s"${PythonAstVisitor.builtinPrefix}float")
      case code if "True".equals(code) || "False".equals(code) => Some(s"${PythonAstVisitor.builtinPrefix}bool")
      case code if code.equals("None")                         => Some(s"${PythonAstVisitor.builtinPrefix}None")
      case code if isPyString(code)                            => Some(s"${PythonAstVisitor.builtinPrefix}str")
      case _                                                   => None
    }).toSet
  }

  override def persistMemberWithTypeDecl(typeFullName: String, memberName: String, types: Set[String]): Unit = {
    val pythonName = convertTypeFullNameToPythonMeta(typeFullName)
    super.persistMemberWithTypeDecl(pythonName, memberName, types)
  }

  override def typeDeclTraversal(typeFullName: String): Traversal[TypeDecl] =
    cpg.typeDecl.fullNameExact(convertTypeFullNameToPythonMeta(typeFullName))

  private def convertTypeFullNameToPythonMeta(typeFullName: String): String = {
    if (typeFullName.endsWith("<module>") || typeFullName.endsWith("<meta>")) typeFullName
    else
      (if (typeFullName.contains(pathSep))
         typeFullName.substring(0, typeFullName.lastIndexOf(pathSep))
       else typeFullName).concat("<meta>")
  }

}
