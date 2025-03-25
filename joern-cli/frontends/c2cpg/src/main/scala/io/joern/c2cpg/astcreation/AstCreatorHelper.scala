package io.joern.c2cpg.astcreation

import io.joern.c2cpg.astcreation.C2CpgScope.PendingReference
import io.joern.x2cpg.Ast
import io.joern.x2cpg.AstNodeBuilder
import io.joern.x2cpg.SourceFiles
import io.joern.x2cpg.utils.IntervalKeyPool
import io.joern.x2cpg.utils.NodeBuilders
import io.joern.x2cpg.utils.NodeBuilders.newDependencyNode
import io.shiftleft.codepropertygraph.generated.nodes.ExpressionNew
import io.shiftleft.codepropertygraph.generated.nodes.NewCall
import io.shiftleft.codepropertygraph.generated.nodes.NewNode
import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.nodes.NewIdentifier
import io.shiftleft.codepropertygraph.generated.nodes.NewLocal
import org.apache.commons.lang3.StringUtils
import org.eclipse.cdt.core.dom.ast.*
import org.eclipse.cdt.core.dom.ast.c.ICASTArrayDesignator
import org.eclipse.cdt.core.dom.ast.c.ICASTDesignatedInitializer
import org.eclipse.cdt.core.dom.ast.c.ICASTFieldDesignator
import org.eclipse.cdt.core.dom.ast.cpp.*
import org.eclipse.cdt.internal.core.dom.parser.c.CASTArrayRangeDesignator
import org.eclipse.cdt.internal.core.dom.parser.cpp.CPPASTArrayRangeDesignator
import org.eclipse.cdt.internal.core.dom.parser.cpp.ICPPEvaluation

import scala.collection.mutable
import scala.util.Try

trait AstCreatorHelper { this: AstCreator =>

  private val fileLocalNameKeyPool = new IntervalKeyPool(first = 0, last = Long.MaxValue)

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

  protected def fileLocalUniqueName(name: String, fullName: String, targetName: String = ""): (String, String) = {
    if (name.isEmpty && (fullName.isEmpty || fullName.endsWith("."))) {
      val newName = targetName match {
        case ""    => s"<anonymous>${fileLocalNameKeyPool.next}"
        case other => s"<$other>${fileLocalNameKeyPool.next}"
      }
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

  protected def offsetToColumn(node: IASTNode, offset: Int): Int = {
    val table           = fileOffsetTable(node)
    val index           = java.util.Arrays.binarySearch(table, offset)
    val tableIndex      = if index < 0 then -(index + 1) else index + 1
    val lineStartOffset = if tableIndex == 0 then 0 else table(tableIndex - 1)
    offset - lineStartOffset + 1
  }

  private def fileOffsetTable(node: IASTNode): Array[Int] = {
    val path = SourceFiles.toAbsolutePath(fileName(node), config.inputPath)
    file2OffsetTable.computeIfAbsent(path, _ => genFileOffsetTable())
  }

  private def genFileOffsetTable(): Array[Int] = {
    cdtAst.getRawSignature.linesWithSeparators.mkString.toCharArray.zipWithIndex.collect { case ('\n', idx) => idx + 1 }
  }

  protected def fileName(node: IASTNode): String = {
    val path = Try(node.getContainingFilename).getOrElse(filename)
    SourceFiles.toRelativePath(path, config.inputPath)
  }

  protected def registerType(typeName: String): String = {
    val fixedTypeName = replaceQualifiedNameSeparator(StringUtils.normalizeSpace(typeName))
    global.usedTypes.putIfAbsent(fixedTypeName, true)
    fixedTypeName
  }

  protected def registerMethodDeclaration(fullName: String, methodInfo: CGlobal.MethodInfo): Unit = {
    global.methodDeclarations.putIfAbsent(fullName, methodInfo)
  }

  protected def registerMethodDefinition(fullName: String): Unit = {
    global.methodDefinitions.putIfAbsent(fullName, true)
  }

  protected def safeGetEvaluation(expr: ICPPASTExpression): Option[ICPPEvaluation] = {
    // In case of unresolved includes etc. this may fail throwing an unrecoverable exception
    Try(expr.getEvaluation).toOption
  }

  protected def safeGetBinding(idExpression: IASTIdExpression): Option[IBinding] = {
    // In case of unresolved includes etc. this may fail throwing an unrecoverable exception
    safeGetBinding(idExpression.getName).collect {
      case binding: IBinding if !binding.isInstanceOf[IProblemBinding] => binding
    }
  }

  protected def safeGetBinding(name: IASTName): Option[IBinding] = {
    // In case of unresolved includes etc. this may fail throwing an unrecoverable exception
    Try(name.resolveBinding()).toOption
  }

  protected def safeGetBinding(spec: IASTNamedTypeSpecifier): Option[IBinding] = {
    // In case of unresolved includes etc. this may fail throwing an unrecoverable exception
    safeGetBinding(spec.getName).collect {
      case binding: IBinding if !binding.isInstanceOf[IProblemBinding] => binding
    }
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

  protected def nullSafeAst(node: IASTStatement, argIndex: Int = -1): Seq[Ast] = {
    Option(node).map(astsForStatement(_, argIndex)).getOrElse(Seq.empty)
  }

  protected def astsForDependenciesAndImports(iASTTranslationUnit: IASTTranslationUnit): Seq[Ast] = {
    val allIncludes = iASTTranslationUnit.getIncludeDirectives.toList.filterNot(isIncludedNode)
    allIncludes.map { include =>
      val name           = include.getName.toString
      val dependencyNode = newDependencyNode(name, name, "include")
      val importNode     = newImportNode(code(include), name, name, include)
      diffGraph.addNode(dependencyNode)
      diffGraph.addEdge(importNode, dependencyNode, EdgeTypes.IMPORTS)
      Ast(importNode)
    }
  }

  protected def isIncludedNode(node: IASTNode): Boolean = fileName(node) != filename

  protected def astsForComments(iASTTranslationUnit: IASTTranslationUnit): Seq[Ast] = {
    if (config.includeComments) {
      iASTTranslationUnit.getComments.toList.filterNot(isIncludedNode).map(comment => astForComment(comment))
    } else {
      Seq.empty
    }
  }

  protected def astForNode(node: IASTNode): Ast = {
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

  protected def createVariableReferenceLinks(): Unit = {
    val resolvedReferenceIt = scope.resolve(createLocalForUnresolvedReference)
    val capturedLocals      = mutable.HashMap.empty[String, NewNode]

    resolvedReferenceIt.foreach { case C2CpgScope.ResolvedReference(variableNodeId, origin) =>
      var currentScope           = origin.stack
      var currentReference       = origin.referenceNode
      var nextReference: NewNode = null
      var done                   = false
      while (!done) {
        val localOrCapturedLocalNodeOption =
          if (currentScope.get.nameToVariableNode.contains(origin.variableName)) {
            done = true
            Option(variableNodeId)
          } else {
            currentScope.flatMap {
              case methodScope: C2CpgScope.MethodScopeElement if methodScope.needsEnclosingScope =>
                currentScope = Option(C2CpgScope.getEnclosingMethodScopeElement(currentScope))
                None
              case methodScope: C2CpgScope.MethodScopeElement =>
                val methodScopeNode          = methodScope.scopeNode
                val closureBindingIdProperty = s"$filename:${methodScope.methodName}:${origin.variableName}"
                capturedLocals.updateWith(closureBindingIdProperty) {
                  case None =>
                    val closureBindingNode = NodeBuilders.newClosureBindingNode(
                      closureBindingIdProperty,
                      origin.variableName,
                      origin.evaluationStrategy
                    )
                    methodScope.capturingRefId.foreach(diffGraph.addEdge(_, closureBindingNode, EdgeTypes.CAPTURE))
                    nextReference = closureBindingNode
                    val localNode = createLocalForUnresolvedReference(methodScopeNode, origin)
                    Option(localNode.closureBindingId(closureBindingIdProperty))
                  case someLocalNode =>
                    // When there is already a LOCAL representing the capturing, we do not
                    // need to process the surrounding scope element as this has already
                    // been processed.
                    done = true
                    someLocalNode
                }
              case _: C2CpgScope.BlockScopeElement => None
            }
          }

        localOrCapturedLocalNodeOption.foreach { localOrCapturedLocalNode =>
          (currentReference, localOrCapturedLocalNode) match {
            case (id: NewIdentifier, local: NewLocal) => transferLineAndColumnInfo(id, local)
            case _                                    => // do nothing
          }
          diffGraph.addEdge(currentReference, localOrCapturedLocalNode, EdgeTypes.REF)
          currentReference = nextReference
        }
        currentScope = currentScope.get.surroundingScope
      }
    }
  }

  private def transferLineAndColumnInfo(src: NewIdentifier, target: NewLocal): Unit = {
    src.lineNumber match {
      // If there are multiple occurrences and the local is already set, ignore later updates
      case Some(srcLineNo) if target.lineNumber.isEmpty || !target.lineNumber.exists(_ < srcLineNo) =>
        target.lineNumber(src.lineNumber)
        target.columnNumber(src.columnNumber)
      case _ => // do nothing
    }
  }

  private def createLocalForUnresolvedReference(
    methodScopeNodeId: NewNode,
    pendingReference: PendingReference
  ): NewLocal = {
    val name = pendingReference.variableName
    val tpe  = pendingReference.tpe
    val code = pendingReference.referenceNode match {
      case id: NewIdentifier => id.code
      case _                 => pendingReference.variableName
    }
    val local = AstNodeBuilder.localNodeWithExplicitPositionInfo(name, code, tpe).order(0)
    diffGraph.addEdge(methodScopeNodeId, local, EdgeTypes.AST)
    local
  }

  private def notHandledText(node: IASTNode): String = {
    s"""Node '${node.getClass.getSimpleName}' not handled yet!
       |  Code: '${shortenCode(node.getRawSignature)}'
       |  File: '$filename'
       |  Line: ${line(node).getOrElse(-1)}
       |  """.stripMargin
  }

}
