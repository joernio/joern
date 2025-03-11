package io.joern.c2cpg.astcreation

import io.joern.x2cpg.Ast
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
import org.eclipse.cdt.core.dom.ast.gnu.c.ICASTKnRFunctionDeclarator
import org.eclipse.cdt.internal.core.dom.parser.c.CASTArrayRangeDesignator
import org.eclipse.cdt.internal.core.dom.parser.cpp.CPPASTArrayRangeDesignator
import org.eclipse.cdt.internal.core.dom.parser.cpp.CPPASTFieldReference
import org.eclipse.cdt.internal.core.dom.parser.cpp.CPPASTIdExpression
import org.eclipse.cdt.internal.core.dom.parser.cpp.CPPMethod
import org.eclipse.cdt.internal.core.dom.parser.cpp.ICPPEvaluation
import org.eclipse.cdt.internal.core.dom.parser.cpp.semantics.EvalBinding
import org.eclipse.cdt.internal.core.dom.parser.cpp.semantics.EvalMemberAccess
import org.eclipse.cdt.internal.core.dom.parser.cpp.CPPASTFoldExpression
import org.eclipse.cdt.internal.core.dom.parser.cpp.semantics.EvalBinary
import org.eclipse.cdt.internal.core.dom.parser.cpp.semantics.EvalFoldExpression
import org.eclipse.cdt.internal.core.dom.parser.cpp.CPPASTEqualsInitializer
import org.eclipse.cdt.internal.core.dom.parser.cpp.CPPFunction
import org.eclipse.cdt.internal.core.dom.parser.cpp.CPPVariable
import org.eclipse.cdt.internal.core.model.ASTStringUtil

import scala.annotation.nowarn
import scala.collection.mutable
import scala.util.Try

trait AstCreatorHelper { this: AstCreator =>

  private val nameKeyPool = new IntervalKeyPool(first = 0, last = Long.MaxValue)

  // Sadly, there is no predefined List / Enum of this within Eclipse CDT:
  private val ReservedKeywordsAtTypes: List[String] =
    List(
      "const",
      "static",
      "restrict",
      "extern",
      "typedef",
      "inline",
      "constexpr",
      "auto",
      "virtual",
      "enum",
      "struct",
      "interface",
      "class"
    )

  private val KeywordsAtTypesToKeep: List[String] = List("unsigned", "volatile")

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

  protected def uniqueName(name: String, fullName: String, targetName: String = ""): (String, String) = {
    if (name.isEmpty && (fullName.isEmpty || fullName.endsWith("."))) {
      val newName = targetName match {
        case ""    => s"<anonymous>${nameKeyPool.next}"
        case other => s"<$other>${nameKeyPool.next}"
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
    val fixedTypeName = fixQualifiedName(StringUtils.normalizeSpace(typeName))
    global.usedTypes.putIfAbsent(fixedTypeName, true)
    fixedTypeName
  }

  protected def registerMethodDeclaration(fullName: String, methodInfo: CGlobal.MethodInfo): Unit = {
    global.methodDeclarations.putIfAbsent(fullName, methodInfo)
  }

  protected def registerMethodDefinition(fullName: String): Unit = {
    global.methodDefinitions.putIfAbsent(fullName, true)
  }

  protected def cleanType(rawType: String): String = {
    if (rawType == Defines.Any) return rawType
    val normalizedTpe = StringUtils.normalizeSpace(rawType.stripSuffix(" ()"))
    val tpe = ReservedKeywordsAtTypes.foldLeft(normalizedTpe) { (cur, repl) =>
      if (cur.startsWith(s"$repl ") || cur.contains(s" $repl ")) {
        cur.replace(s" $repl ", " ").stripPrefix(s"$repl ")
      } else cur
    }
    replaceWhitespaceAfterKeyword(tpe) match {
      // Empty or problematic types
      case ""                                                                      => Defines.Any
      case t if t.contains("?")                                                    => Defines.Any
      case t if t.contains("#") && !t.contains("<#0")                              => Defines.Any
      case t if t.contains("::{") || t.contains("}::")                             => Defines.Any
      case t if (t.contains("{") || t.contains("}")) && !isThisLambdaCapture(t)    => Defines.Any
      case t if t.contains("org.eclipse.cdt.internal.core.dom.parser.ProblemType") => Defines.Any
      // Special patterns with specific handling
      case t if t.startsWith("[") && t.endsWith("]")       => Defines.Array
      case t if isThisLambdaCapture(t) || t.contains("->") => Defines.Function
      case t if t.contains("<#0") && t.endsWith(">")       => fixQualifiedName(t.replaceAll("#\\d+", Defines.Any))
      case t if t.contains("( ")                           => fixQualifiedName(t.substring(0, t.indexOf("( ")))
      // Default case
      case typeStr => fixQualifiedName(typeStr)
    }
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

  protected def safeGetType(tpe: IType): String = {
    // In case of unresolved includes etc. this may fail throwing an unrecoverable exception
    Try(ASTTypeUtil.getType(tpe)).getOrElse(Defines.Any)
  }

  @nowarn
  protected def typeFor(node: IASTNode): String = {
    import org.eclipse.cdt.core.dom.ast.ASTSignatureUtil.getNodeSignature
    node match {
      case f: CPPASTFoldExpression                               => typeForCPPASTFoldExpression(f)
      case f: CPPASTFieldReference                               => typeForCPPASTFieldReference(f)
      case s: CPPASTIdExpression                                 => typeForCPPASTIdExpression(s)
      case s: ICPPASTNamedTypeSpecifier                          => typeForCPPAstNamedTypeSpecifier(s)
      case a: IASTArrayDeclarator                                => typeForIASTArrayDeclarator(a)
      case c: ICPPASTConstructorInitializer                      => typeForICPPASTConstructorInitializer(c)
      case c: CPPASTEqualsInitializer                            => typeForCPPASTEqualsInitializer(c)
      case _: IASTIdExpression | _: IASTName | _: IASTDeclarator => cleanType(safeGetNodeType(node))
      case f: IASTFieldReference          => cleanType(safeGetType(f.getFieldOwner.getExpressionType))
      case s: IASTNamedTypeSpecifier      => cleanType(ASTStringUtil.getReturnTypeString(s, null))
      case s: IASTCompositeTypeSpecifier  => cleanType(ASTStringUtil.getReturnTypeString(s, null))
      case s: IASTEnumerationSpecifier    => cleanType(ASTStringUtil.getReturnTypeString(s, null))
      case s: IASTElaboratedTypeSpecifier => cleanType(ASTStringUtil.getReturnTypeString(s, null))
      case l: IASTLiteralExpression       => cleanType(safeGetType(l.getExpressionType))
      case e: IASTExpression              => cleanType(safeGetNodeType(e))
      case d: IASTSimpleDeclaration       => cleanType(typeForDeclSpecifier(d.getDeclSpecifier))
      case _                              => cleanType(getNodeSignature(node))
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

  protected def functionTypeToSignature(typ: IFunctionType): String = {
    val returnType     = cleanType(safeGetType(typ.getReturnType))
    val parameterTypes = typ.getParameterTypes.map(t => cleanType(safeGetType(t)))
    StringUtils.normalizeSpace(s"$returnType(${parameterTypes.mkString(",")})")
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

  protected def typeForDeclSpecifier(spec: IASTNode, index: Int = 0): String = {
    val tpe = spec match {
      case s: IASTSimpleDeclSpecifier if s.getParent.isInstanceOf[IASTParameterDeclaration] =>
        val parentDecl = s.getParent.asInstanceOf[IASTParameterDeclaration].getDeclarator
        pointersAsString(s, parentDecl)
      case s: IASTSimpleDeclSpecifier if s.getParent.isInstanceOf[IASTFunctionDefinition] =>
        val parentDecl = s.getParent.asInstanceOf[IASTFunctionDefinition].getDeclarator
        ASTStringUtil.getReturnTypeString(s, parentDecl)
      case s: IASTSimpleDeclaration if s.getParent.isInstanceOf[ICASTKnRFunctionDeclarator] =>
        val decl = s.getDeclarators.toList(index)
        pointersAsString(s.getDeclSpecifier, decl)
      case s: IASTSimpleDeclSpecifier if s.getParent.isInstanceOf[IASTSimpleDeclaration] =>
        val parentDecl = s.getParent.asInstanceOf[IASTSimpleDeclaration].getDeclarators.toList(index)
        pointersAsString(s, parentDecl)
      case s: IASTSimpleDeclSpecifier =>
        ASTStringUtil.getReturnTypeString(s, null)
      case s: IASTNamedTypeSpecifier if s.getParent.isInstanceOf[IASTParameterDeclaration] =>
        val parentDecl = s.getParent.asInstanceOf[IASTParameterDeclaration].getDeclarator
        pointersAsString(s, parentDecl)
      case s: IASTNamedTypeSpecifier if s.getParent.isInstanceOf[IASTSimpleDeclaration] =>
        val parentDecl = s.getParent.asInstanceOf[IASTSimpleDeclaration].getDeclarators.toList(index)
        pointersAsString(s, parentDecl)
      case s: IASTNamedTypeSpecifier =>
        ASTStringUtil.getSimpleName(s.getName)
      case s: IASTCompositeTypeSpecifier if s.getParent.isInstanceOf[IASTSimpleDeclaration] =>
        val parentDecl = s.getParent.asInstanceOf[IASTSimpleDeclaration].getDeclarators.toList(index)
        pointersAsString(s, parentDecl)
      case s: IASTCompositeTypeSpecifier => ASTStringUtil.getSimpleName(s.getName)
      case s: IASTEnumerationSpecifier if s.getParent.isInstanceOf[IASTSimpleDeclaration] =>
        val parentDecl = s.getParent.asInstanceOf[IASTSimpleDeclaration].getDeclarators.toList(index)
        pointersAsString(s, parentDecl)
      case s: IASTEnumerationSpecifier => ASTStringUtil.getSimpleName(s.getName)
      case s: IASTElaboratedTypeSpecifier if s.getParent.isInstanceOf[IASTParameterDeclaration] =>
        val parentDecl = s.getParent.asInstanceOf[IASTParameterDeclaration].getDeclarator
        pointersAsString(s, parentDecl)
      case s: IASTElaboratedTypeSpecifier if s.getParent.isInstanceOf[IASTSimpleDeclaration] =>
        val parentDecl = s.getParent.asInstanceOf[IASTSimpleDeclaration].getDeclarators.toList(index)
        pointersAsString(s, parentDecl)
      case s: IASTElaboratedTypeSpecifier => ASTStringUtil.getSignatureString(s, null)
      // TODO: handle other types of IASTDeclSpecifier
      case _ => Defines.Any
    }
    if (tpe.isEmpty) Defines.Any else tpe
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
                    val localNode = createLocalForUnresolvedReference(methodScopeNode, origin.variableName, origin.tpe)
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
    variableName: String,
    tpe: String
  ): NewLocal = {
    val local = localNodeWithExplicitPositionInfo(variableName, variableName, tpe).order(0)
    diffGraph.addEdge(methodScopeNodeId, local, EdgeTypes.AST)
    local
  }

  private def replaceWhitespaceAfterKeyword(tpe: String): String = {
    if (KeywordsAtTypesToKeep.exists(k => tpe.startsWith(s"$k ") || tpe.contains(s" $k "))) {
      KeywordsAtTypesToKeep.foldLeft(tpe) { (cur, repl) =>
        val prefixStartsWith = s"$repl "
        val prefixContains   = s" $repl "
        if (cur.startsWith(prefixStartsWith)) {
          prefixStartsWith + replaceWhitespaceAfterKeyword(cur.substring(prefixStartsWith.length))
        } else if (cur.contains(prefixContains)) {
          val front = tpe.substring(0, tpe.indexOf(prefixContains))
          val back  = tpe.substring(tpe.indexOf(prefixContains) + prefixContains.length)
          s"${replaceWhitespaceAfterKeyword(front)}$prefixContains${replaceWhitespaceAfterKeyword(back)}"
        } else {
          cur
        }
      }
    } else {
      tpe.replace(" ", "")
    }
  }

  private def isThisLambdaCapture(tpe: String): Boolean = {
    tpe.startsWith("[*this]") || tpe.startsWith("[this]") || (tpe.startsWith("[") && tpe.contains("this]"))
  }

  private def safeGetNodeType(node: IASTNode): String = {
    // In case of unresolved includes etc. this may fail throwing an unrecoverable exception
    Try(ASTTypeUtil.getNodeType(node)).getOrElse(Defines.Any)
  }

  private def typeForCPPASTFieldReference(f: CPPASTFieldReference): String = {
    safeGetEvaluation(f.getFieldOwner) match {
      case Some(evaluation: EvalBinding) => cleanType(evaluation.getType.toString)
      case _                             => cleanType(safeGetType(f.getFieldOwner.getExpressionType))
    }
  }

  private def typeForCPPASTFoldExpression(f: CPPASTFoldExpression): String = {
    safeGetEvaluation(f) match {
      case Some(evaluation: EvalFoldExpression) =>
        Try(evaluation.getValue.getEvaluation).toOption match {
          case Some(value: EvalBinary) =>
            val s = value.toString
            cleanType(s.substring(0, s.indexOf(": ")))
          case Some(value: EvalBinding) if value.getType.isInstanceOf[ICPPParameterPackType] =>
            val s = value.getType.asInstanceOf[ICPPParameterPackType].getType.toString
            cleanType(s)
          case _ => Defines.Any
        }
      case _ => Defines.Any
    }
  }

  @nowarn
  private def typeForIASTArrayDeclarator(a: IASTArrayDeclarator): String = {
    import org.eclipse.cdt.core.dom.ast.ASTSignatureUtil.getNodeSignature
    if (safeGetNodeType(a).startsWith("? ")) {
      val tpe = getNodeSignature(a).replace("[]", "").strip()
      val arr = safeGetNodeType(a).replace("? ", "")
      s"$tpe$arr"
    } else if (safeGetNodeType(a).contains("} ") || safeGetNodeType(a).contains(" [")) {
      val tpe = getNodeSignature(a).replace("[]", "").strip()
      val arr = a.getArrayModifiers.map {
        case m if m.getConstantExpression != null => s"[${nodeSignature(m.getConstantExpression)}]"
        case _ if a.getInitializer != null =>
          a.getInitializer match {
            case l: IASTInitializerList => s"[${l.getSize}]"
            case _                      => "[]"
          }
        case _ => "[]"
      }.mkString
      s"$tpe$arr"
    } else {
      cleanType(safeGetNodeType(a))
    }
  }

  private def typeForCPPASTIdExpression(s: CPPASTIdExpression): String = {
    safeGetEvaluation(s) match {
      case Some(evaluation: EvalMemberAccess) =>
        val deref = if (evaluation.isPointerDeref) "*" else ""
        cleanType(evaluation.getOwnerType.toString + deref)
      case Some(evalBinding: EvalBinding) =>
        evalBinding.getBinding match {
          case m: CPPMethod   => cleanType(safeGetNodeType(m.getPrimaryDeclaration))
          case f: CPPFunction => cleanType(safeGetNodeType(f.getDefinition))
          case v: CPPVariable => cleanType(v.getType.toString)
          case _              => cleanType(safeGetNodeType(s))
        }
      case _ => cleanType(safeGetNodeType(s))
    }
  }

  @nowarn
  private def typeForICPPASTConstructorInitializer(c: ICPPASTConstructorInitializer): String = {
    import org.eclipse.cdt.core.dom.ast.ASTSignatureUtil.getNodeSignature
    c.getParent match {
      case initializer: ICPPASTConstructorChainInitializer =>
        val initIdFullName = fullName(initializer.getMemberInitializerId)
        cleanType(initIdFullName)
      case _ =>
        cleanType(getNodeSignature(c))
    }
  }

  private def typeForCPPASTEqualsInitializer(c: CPPASTEqualsInitializer): String = {
    import org.eclipse.cdt.core.dom.ast.ASTSignatureUtil.getNodeSignature
    c.getInitializerClause match {
      case initializer: ICPPASTFunctionCallExpression
          if initializer.getFunctionNameExpression.isInstanceOf[CPPASTIdExpression] =>
        val name = initializer.getFunctionNameExpression.asInstanceOf[CPPASTIdExpression]
        typeForCPPASTIdExpression(name)
      case _ =>
        cleanType(getNodeSignature(c))
    }
  }

  private def typeForCPPAstNamedTypeSpecifier(s: ICPPASTNamedTypeSpecifier): String = {
    val tpe = safeGetBinding(s) match {
      case Some(spec: ICPPSpecialization) => spec.toString
      case Some(n: ICPPBinding)           => n.getQualifiedName.mkString(".")
      case Some(other: IBinding)          => other.toString
      case _ if s.getName != null         => ASTStringUtil.getQualifiedName(s.getName)
      case _                              => s.getRawSignature
    }
    cleanType(tpe)
  }

  private def notHandledText(node: IASTNode): String =
    s"""Node '${node.getClass.getSimpleName}' not handled yet!
       |  Code: '${shortenCode(node.getRawSignature)}'
       |  File: '$filename'
       |  Line: ${line(node).getOrElse(-1)}
       |  """.stripMargin

  private def pointersAsString(spec: IASTDeclSpecifier, parentDecl: IASTDeclarator): String = {
    val tpe = typeFor(spec) match {
      case Defines.Auto => typeFor(parentDecl)
      case t            => t
    }
    val pointers = parentDecl.getPointerOperators
    val arr = parentDecl match {
      case p: IASTArrayDeclarator => p.getArrayModifiers.toList.map(_.getRawSignature).mkString
      case _                      => ""
    }
    if (pointers.isEmpty) { s"$tpe$arr" }
    else {
      val refs = pointers
        .map {
          case r: ICPPASTReferenceOperator if r.isRValueReference => "&&"
          case _: ICPPASTReferenceOperator                        => "&"
          case _: IASTPointer                                     => "*"
        }
        .mkString("")
      s"$tpe$arr$refs".strip()
    }
  }

}
