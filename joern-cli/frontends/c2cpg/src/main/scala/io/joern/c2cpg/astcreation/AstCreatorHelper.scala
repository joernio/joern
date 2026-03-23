package io.joern.c2cpg.astcreation

import io.joern.c2cpg.passes.FunctionDeclNodePass
import io.joern.x2cpg.AstNodeBuilder.dependencyNode
import io.joern.x2cpg.{Ast, AstNodeBuilder, SourceFiles}
import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.nodes.{ExpressionNew, NewCall, NewNode}
import org.eclipse.cdt.core.dom.ast.*
import org.eclipse.cdt.core.dom.ast.c.{ICASTArrayDesignator, ICASTDesignatedInitializer, ICASTFieldDesignator}
import org.eclipse.cdt.core.dom.ast.cpp.*
import org.eclipse.cdt.internal.core.dom.parser.c.CASTArrayRangeDesignator
import org.eclipse.cdt.internal.core.dom.parser.cpp.{CPPASTArrayRangeDesignator, ICPPEvaluation}

import scala.collection.mutable
import scala.util.{Success, Try}

trait AstCreatorHelper { this: AstCreator =>

  private val scopeLocalUniqueNames: mutable.Map[String, Int] = mutable.HashMap.empty
  private val file2OffsetTable: Array[Int]                    = genFileOffsetTable()

  // We use our own call ast creation function since the version in x2cpg treats
  // base as receiver if no receiver is given which does not fit the needs of this
  // frontend.
  protected def createCallAst(
    callNode: NewCall,
    arguments: Seq[Ast] = List(),
    base: Option[Ast] = None,
    receiver: Option[Ast] = None
  ): Ast = {
    setArgumentIndices(arguments)

    val baseRoot = base.flatMap(_.root).toList
    baseRoot match {
      case List(x: ExpressionNew) => x.argumentIndex = 0
      case _                      => // do nothing
    }

    val bse = base.getOrElse(Ast())
    var ast = Ast(callNode).withChild(bse)

    if (receiver.isDefined && receiver != base) {
      receiver.get.root.get.asInstanceOf[ExpressionNew].argumentIndex = -1
      ast = ast.withChild(receiver.get)
    }

    ast = ast
      .withChildren(arguments)
      .withArgEdges(callNode, baseRoot)
      .withArgEdges(callNode, arguments.flatMap(_.root))

    if (receiver.isDefined) {
      ast = ast.withReceiverEdge(callNode, receiver.get.root.get)
    }
    ast
  }

  protected def scopeLocalUniqueName(targetName: String, fullName: String = ""): String = {
    val name      = if (targetName.isEmpty) "<anonymous>" else s"<$targetName>"
    val scopePath = if (fullName.isEmpty) scope.computeScopePath else fullName.stripSuffix(".")
    val key       = s"$scopePath:$name"
    val idx       = scopeLocalUniqueNames.getOrElseUpdate(key, 0)
    scopeLocalUniqueNames.update(key, idx + 1)
    s"$name$idx"
  }

  protected def scopeLocalUniqueNamespaceFullName(fullName: String): String = {
    val newFullName = fullName match {
      case ""     => "<anonymous>"
      case s"$p." => s"$p.<anonymous>"
      case other  => other
    }
    scopeLocalUniqueNames.get(newFullName) match {
      case None =>
        scopeLocalUniqueNames.update(newFullName, 0)
        newFullName
      case Some(index) =>
        val suffix = s"${Defines.NamespaceExtension}$index"
        s"$newFullName$suffix"
    }
  }

  protected def scopeLocalUniqueName(name: String, fullName: String, targetName: String): (String, String) = {
    if (name.isEmpty && (fullName.isEmpty || fullName.endsWith("."))) {
      val newName           = scopeLocalUniqueName(targetName, fullName)
      val resultingFullName = s"$fullName$newName"
      (newName, resultingFullName)
    } else {
      (name, fullName)
    }
  }

  protected def nullSafeFileLocation(node: IASTNode): Option[IASTFileLocation] =
    Option(cdtAst.flattenLocationsToFile(node.getNodeLocations)).map(_.asFileLocation())

  protected def nullSafeFileLocationLast(node: IASTNode): Option[IASTFileLocation] =
    Option(cdtAst.flattenLocationsToFile(node.getNodeLocations.lastOption.toArray)).map(_.asFileLocation())

  protected def offsetToColumn(offset: Int): Int = {
    val index           = java.util.Arrays.binarySearch(file2OffsetTable, offset)
    val tableIndex      = if (index < 0) -(index + 1) else index + 1
    val lineStartOffset = if (tableIndex == 0) 0 else file2OffsetTable(tableIndex - 1)
    offset - lineStartOffset + 1
  }

  protected def registerType(typeName: String): String = {
    val fixedTypeName = replaceQualifiedNameSeparator(typeName)
    global.usedTypes.putIfAbsent(fixedTypeName, true)
    fixedTypeName
  }

  protected def registerMethodDeclaration(fullName: String, methodInfo: FunctionDeclNodePass.MethodInfo): Unit = {
    global.methodDeclarations.putIfAbsent(fullName, methodInfo)
  }

  protected def registerMethodDefinition(fullName: String): Unit = {
    global.methodDefinitions.putIfAbsent(fullName, true)
  }

  protected def safeGetEvaluation(expr: ICPPASTExpression): Option[ICPPEvaluation] = {
    // In case of unresolved includes etc. this may fail throwing an unrecoverable exception
    Try(expr.getEvaluation).toOption.filter(_ != null)
  }

  protected def safeGetBinding(idExpression: IASTIdExpression): Option[IBinding] = {
    // In case of unresolved includes etc. this may fail throwing an unrecoverable exception
    safeGetBinding(idExpression.getName).collect {
      case binding: IBinding if !binding.isInstanceOf[IProblemBinding] => binding
    }
  }

  protected def safeGetBinding(spec: IASTNamedTypeSpecifier): Option[IBinding] = {
    // In case of unresolved includes etc. this may fail throwing an unrecoverable exception
    safeGetBinding(spec.getName).collect {
      case binding: IBinding if !binding.isInstanceOf[IProblemBinding] => binding
    }
  }

  protected def safeGetBinding(name: IASTName): Option[IBinding] = {
    // In case of unresolved includes etc. this may fail throwing an unrecoverable exception
    Try(name.resolveBinding()).toOption.filter(_ != null)
  }

  protected def notHandledYet(node: IASTNode): Ast = {
    if (!node.isInstanceOf[IASTProblem] && !node.isInstanceOf[IASTProblemHolder]) {
      val text = notHandledText(node)
      logger.info(text)
    }
    Ast(unknownNode(node, code(node)))
  }

  protected def nullSafeCode(node: IASTNode): String = {
    Option(node).map(code).getOrElse("")
  }

  protected def nullSafeAst(node: IASTExpression, argIndex: Int): Ast = {
    val r = nullSafeAst(node)
    r.root match {
      case Some(x: ExpressionNew) =>
        x.argumentIndex = argIndex
      case _ =>
    }
    r
  }

  protected def nullSafeAst(node: IASTInitializer): Ast =
    Option(node).map(astForNode).getOrElse(Ast())

  protected def nullSafeAst(node: IASTExpression): Ast =
    Option(node).map(astForNode).getOrElse(Ast())

  protected def nullSafeAst(node: IASTDeclaration): Seq[Ast] =
    Option(node).map(astsForDeclaration).getOrElse(Seq.empty)

  protected def nullSafeAst(node: IASTStatement): Seq[Ast] = {
    Option(node).map(astsForStatement).getOrElse(Seq.empty)
  }

  protected def astsForDependenciesAndImports(iASTTranslationUnit: IASTTranslationUnit): Seq[Ast] = {
    val allIncludes = iASTTranslationUnit.getIncludeDirectives.toList.filterNot(isIncludedNode)
    allIncludes.map { include =>
      val name       = include.getName.toString
      val dependency = dependencyNode(name, name, "include")
      val importNode = newImportNode(code(include), name, name, include)
      diffGraph.addNode(dependency)
      diffGraph.addEdge(importNode, dependency, EdgeTypes.IMPORTS)
      Ast(importNode)
    }
  }

  protected def astsForComments(iASTTranslationUnit: IASTTranslationUnit): Seq[Ast] = {
    if (config.includeComments)
      iASTTranslationUnit.getComments.toList.filterNot(isIncludedNode).map(comment => astForComment(comment))
    else Seq.empty
  }

  protected def isIncludedNode(node: IASTNode): Boolean = fileName(node) != filename

  protected def fileName(node: IASTNode): String = {
    val path = Try(node.getContainingFilename) match {
      case Success(value) if value.nonEmpty => value
      case _                                => filename
    }
    SourceFiles.toRelativePath(path, config.inputPath)
  }

  protected def astForNode(node: IASTNode): Ast = {
    if (isUnsupportedCoroutineKeyword(node)) {
      return astForUnsupportedCoroutineNode(node)
    }

    node match {
      case expr: IASTExpression             => astForExpression(expr)
      case name: IASTName                   => astForIdentifier(name)
      case decl: IASTDeclSpecifier          => astForIdentifier(decl)
      case l: IASTInitializerList           => astForInitializerList(l)
      case c: ICPPASTConstructorInitializer => astForCPPASTConstructorInitializer(c)
      case d: ICASTDesignatedInitializer    => astForCASTDesignatedInitializer(d)
      case d: IASTEqualsInitializer         => astForNode(d.getInitializerClause)
      case d: ICPPASTDesignatedInitializer  => astForCPPASTDesignatedInitializer(d)
      case d: CASTArrayRangeDesignator      => astForCASTArrayRangeDesignator(d)
      case d: CPPASTArrayRangeDesignator    => astForCPPASTArrayRangeDesignator(d)
      case d: ICASTArrayDesignator          => nullSafeAst(d.getSubscriptExpression)
      case d: ICPPASTArrayDesignator        => nullSafeAst(d.getSubscriptExpression)
      case d: ICPPASTFieldDesignator        => astForNode(d.getName)
      case d: ICASTFieldDesignator          => astForNode(d.getName)
      case decl: ICPPASTDecltypeSpecifier   => astForDecltypeSpecifier(decl)
      case arrMod: IASTArrayModifier        => astForArrayModifier(arrMod)
      case _                                => notHandledYet(node)
    }
  }

  private def genFileOffsetTable(): Array[Int] = {
    cdtAst.getRawSignature.toCharArray.zipWithIndex.collect { case ('\n', idx) => idx + 1 }
  }

  private def notHandledText(node: IASTNode): String = {
    s"""Node '${node.getClass.getSimpleName}' not handled yet!
       |  Code: '${shortenCode(node.getRawSignature)}'
       |  File: '$filename'
       |  Line: ${line(node).getOrElse(-1)}
       |  """.stripMargin
  }

}
