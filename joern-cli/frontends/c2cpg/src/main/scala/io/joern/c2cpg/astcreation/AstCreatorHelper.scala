package io.joern.c2cpg.astcreation

import io.joern.x2cpg.Ast
import io.joern.x2cpg.SourceFiles
import io.joern.x2cpg.ValidationMode
import io.joern.x2cpg.Defines as X2CpgDefines
import io.joern.x2cpg.utils.NodeBuilders.newDependencyNode
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes.ExpressionNew
import io.shiftleft.codepropertygraph.generated.nodes.NewCall
import io.shiftleft.codepropertygraph.generated.nodes.NewNode
import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.utils.IOUtils
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
import org.eclipse.cdt.internal.core.model.ASTStringUtil

import java.nio.file.Path
import java.nio.file.Paths
import scala.annotation.nowarn
import scala.collection.mutable
import scala.util.Try

object AstCreatorHelper {

  implicit class OptionSafeAst(val ast: Ast) extends AnyVal {
    def withArgEdge(src: NewNode, dst: Option[NewNode]): Ast = dst match {
      case Some(value) => ast.withArgEdge(src, value)
      case None        => ast
    }
  }
}

trait AstCreatorHelper(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  import io.joern.c2cpg.astcreation.AstCreatorHelper.*

  private var usedVariablePostfix: Int = 0

  protected def isIncludedNode(node: IASTNode): Boolean = fileName(node) != filename

  protected def uniqueName(target: String, name: String, fullName: String): (String, String) = {
    if (name.isEmpty && (fullName.isEmpty || fullName.endsWith("."))) {
      val name              = s"anonymous_${target}_$usedVariablePostfix"
      val resultingFullName = s"$fullName$name"
      usedVariablePostfix = usedVariablePostfix + 1
      (name, resultingFullName)
    } else {
      (name, fullName)
    }
  }

  private def fileOffsetTable(node: IASTNode): Array[Int] = {
    val path = SourceFiles.toAbsolutePath(fileName(node), config.inputPath)
    file2OffsetTable.computeIfAbsent(path, _ => genFileOffsetTable(Paths.get(path)))
  }

  private def genFileOffsetTable(absolutePath: Path): Array[Int] = {
    val asCharArray = IOUtils.readLinesInFile(absolutePath).mkString("\n").toCharArray
    val offsets     = mutable.ArrayBuffer.empty[Int]

    for (i <- Range(0, asCharArray.length)) {
      if (asCharArray(i) == '\n') {
        offsets.append(i + 1)
      }
    }
    offsets.toArray
  }

  protected def nullSafeFileLocation(node: IASTNode): Option[IASTFileLocation] =
    Option(cdtAst.flattenLocationsToFile(node.getNodeLocations)).map(_.asFileLocation())
  protected def nullSafeFileLocationLast(node: IASTNode): Option[IASTFileLocation] =
    Option(cdtAst.flattenLocationsToFile(node.getNodeLocations.lastOption.toArray)).map(_.asFileLocation())

  protected def fileName(node: IASTNode): String = {
    val path = Try(node.getContainingFilename).getOrElse(filename)
    SourceFiles.toRelativePath(path, config.inputPath)
  }

  protected def offsetToColumn(node: IASTNode, offset: Int): Int = {
    val table      = fileOffsetTable(node)
    val index      = java.util.Arrays.binarySearch(table, offset)
    val tableIndex = if (index < 0) -(index + 1) else index + 1
    val lineStartOffset = if (tableIndex == 0) {
      0
    } else {
      table(tableIndex - 1)
    }
    val column = offset - lineStartOffset + 1
    column
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

  // Sadly, there is no predefined List / Enum of this within Eclipse CDT:
  private val ReservedTypeKeywords: List[String] =
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

  private val KeepTypeKeywords: List[String] = List("unsigned", "volatile")

  protected def cleanType(rawType: String, stripKeywords: Boolean = true): String = {
    if (rawType == Defines.Any) return rawType
    val tpe =
      if (stripKeywords) {
        ReservedTypeKeywords.foldLeft(rawType) { (cur, repl) =>
          if (cur.contains(s"$repl ")) cur.replace(s"$repl ", "") else cur
        }
      } else {
        rawType
      }
    StringUtils.normalizeSpace(tpe) match {
      case ""                                                                      => Defines.Any
      case t if t.startsWith("[") && t.endsWith("]")                               => Defines.Array
      case t if t.contains("->")                                                   => Defines.Function
      case t if t.contains("?")                                                    => Defines.Any
      case t if t.contains("#")                                                    => Defines.Any
      case t if t.contains("::{") || t.contains("}::")                             => Defines.Any
      case t if t.contains("{") || t.contains("}")                                 => Defines.Any
      case t if t.contains("org.eclipse.cdt.internal.core.dom.parser.ProblemType") => Defines.Any
      case t if t.contains("( ") => replaceWhitespaceAfterTypeKeyword(fixQualifiedName(t.substring(0, t.indexOf("( "))))
      case t if t.contains(Defines.QualifiedNameSeparator) => replaceWhitespaceAfterTypeKeyword(fixQualifiedName(t))
      case t if KeepTypeKeywords.exists(k => t.startsWith(s"$k ")) => replaceWhitespaceAfterTypeKeyword(t)
      case t if t.contains("[") && t.contains("]")                 => replaceWhitespaceAfterTypeKeyword(t)
      case t if t.contains("*")                                    => replaceWhitespaceAfterTypeKeyword(t)
      case someType                                                => someType
    }
  }

  private def replaceWhitespaceAfterTypeKeyword(tpe: String): String = {
    if (KeepTypeKeywords.exists(k => tpe.startsWith(s"$k "))) {
      KeepTypeKeywords.foldLeft(tpe) { (cur, repl) =>
        val prefix = s"$repl "
        if (cur.startsWith(prefix)) {
          prefix + cur.substring(prefix.length).replace(" ", "")
        } else {
          cur
        }
      }
    } else {
      tpe.replace(" ", "")
    }
  }

  protected def safeGetEvaluation(expr: ICPPASTExpression): Option[ICPPEvaluation] = {
    // In case of unresolved includes etc. this may fail throwing an unrecoverable exception
    Try(expr.getEvaluation).toOption
  }

  protected def safeGetType(tpe: IType): String = {
    // In case of unresolved includes etc. this may fail throwing an unrecoverable exception
    Try(ASTTypeUtil.getType(tpe)).getOrElse(Defines.Any)
  }

  private def safeGetNodeType(node: IASTNode): String = {
    // In case of unresolved includes etc. this may fail throwing an unrecoverable exception
    Try(ASTTypeUtil.getNodeType(node)).getOrElse(Defines.Any)
  }

  private def typeForCPPASTFieldReference(f: CPPASTFieldReference, stripKeywords: Boolean = true): String = {
    safeGetEvaluation(f.getFieldOwner) match {
      case Some(evaluation: EvalBinding) => cleanType(evaluation.getType.toString, stripKeywords)
      case _                             => cleanType(safeGetType(f.getFieldOwner.getExpressionType), stripKeywords)
    }
  }

  @nowarn
  private def typeForIASTArrayDeclarator(a: IASTArrayDeclarator, stripKeywords: Boolean = true): String = {
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
      cleanType(safeGetNodeType(a), stripKeywords)
    }
  }

  private def typeForCPPASTIdExpression(s: CPPASTIdExpression, stripKeywords: Boolean = true): String = {
    safeGetEvaluation(s) match {
      case Some(evaluation: EvalMemberAccess) =>
        val deref = if (evaluation.isPointerDeref) "*" else ""
        cleanType(evaluation.getOwnerType.toString + deref, stripKeywords)
      case Some(evalBinding: EvalBinding) =>
        evalBinding.getBinding match {
          case m: CPPMethod => cleanType(fullName(m.getDefinition))
          case _            => cleanType(safeGetNodeType(s), stripKeywords)
        }
      case _ => cleanType(safeGetNodeType(s), stripKeywords)
    }
  }

  @nowarn
  private def typeForICPPASTConstructorInitializer(
    c: ICPPASTConstructorInitializer,
    stripKeywords: Boolean = true
  ): String = {
    import org.eclipse.cdt.core.dom.ast.ASTSignatureUtil.getNodeSignature
    c.getParent match {
      case initializer: ICPPASTConstructorChainInitializer =>
        val fullName_ = fullName(initializer.getMemberInitializerId)
        cleanType(fullName_, stripKeywords)
      case _ =>
        cleanType(getNodeSignature(c), stripKeywords)
    }
  }

  @nowarn
  protected def typeFor(node: IASTNode, stripKeywords: Boolean = true): String = {
    import org.eclipse.cdt.core.dom.ast.ASTSignatureUtil.getNodeSignature
    node match {
      case f: CPPASTFieldReference => typeForCPPASTFieldReference(f)
      case f: IASTFieldReference   => cleanType(safeGetType(f.getFieldOwner.getExpressionType), stripKeywords)
      case a: IASTArrayDeclarator  => typeForIASTArrayDeclarator(a)
      case s: CPPASTIdExpression   => typeForCPPASTIdExpression(s)
      case _: IASTIdExpression | _: IASTName | _: IASTDeclarator => cleanType(safeGetNodeType(node), stripKeywords)
      case s: IASTNamedTypeSpecifier        => cleanType(ASTStringUtil.getReturnTypeString(s, null), stripKeywords)
      case s: IASTCompositeTypeSpecifier    => cleanType(ASTStringUtil.getReturnTypeString(s, null), stripKeywords)
      case s: IASTEnumerationSpecifier      => cleanType(ASTStringUtil.getReturnTypeString(s, null), stripKeywords)
      case s: IASTElaboratedTypeSpecifier   => cleanType(ASTStringUtil.getReturnTypeString(s, null), stripKeywords)
      case l: IASTLiteralExpression         => cleanType(safeGetType(l.getExpressionType))
      case e: IASTExpression                => cleanType(safeGetNodeType(e), stripKeywords)
      case c: ICPPASTConstructorInitializer => typeForICPPASTConstructorInitializer(c)
      case _                                => cleanType(getNodeSignature(node), stripKeywords)
    }
  }

  private def notHandledText(node: IASTNode): String =
    s"""Node '${node.getClass.getSimpleName}' not handled yet!
       |  Code: '${node.getRawSignature}'
       |  File: '$filename'
       |  Line: ${line(node).getOrElse(-1)}
       |  """.stripMargin

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

  private def pointersAsString(spec: IASTDeclSpecifier, parentDecl: IASTDeclarator, stripKeywords: Boolean): String = {
    val tpe      = typeFor(spec, stripKeywords)
    val pointers = parentDecl.getPointerOperators
    val arr = parentDecl match {
      case p: IASTArrayDeclarator => p.getArrayModifiers.toList.map(_.getRawSignature).mkString
      case _                      => ""
    }
    if (pointers.isEmpty) { s"$tpe$arr" }
    else {
      val refs = pointers.map(_.getRawSignature).mkString("")
      s"$tpe$arr$refs".strip()
    }
  }

  protected def astsForDependenciesAndImports(iASTTranslationUnit: IASTTranslationUnit): Seq[Ast] = {
    val allIncludes = iASTTranslationUnit.getIncludeDirectives.toList.filterNot(isIncludedNode)
    allIncludes.map { include =>
      val name            = include.getName.toString
      val _dependencyNode = newDependencyNode(name, name, "include")
      val importNode      = newImportNode(code(include), name, name, include)
      diffGraph.addNode(_dependencyNode)
      diffGraph.addEdge(importNode, _dependencyNode, EdgeTypes.IMPORTS)
      Ast(importNode)
    }
  }

  protected def astsForComments(iASTTranslationUnit: IASTTranslationUnit): Seq[Ast] = {
    if (config.includeComments) {
      iASTTranslationUnit.getComments.toList.filterNot(isIncludedNode).map(comment => astForComment(comment))
    } else {
      Seq.empty
    }
  }

  private def astForDecltypeSpecifier(decl: ICPPASTDecltypeSpecifier): Ast = {
    val op       = Defines.OperatorTypeOf
    val cpgUnary = callNode(decl, code(decl), op, op, DispatchTypes.STATIC_DISPATCH, None, Some(X2CpgDefines.Any))
    val operand  = nullSafeAst(decl.getDecltypeExpression)
    callAst(cpgUnary, List(operand))
  }

  private def astForCASTDesignatedInitializer(d: ICASTDesignatedInitializer): Ast = {
    val node = blockNode(d, Defines.Empty, Defines.Void)
    scope.pushNewScope(node)
    val op = Operators.assignment
    val calls = withIndex(d.getDesignators) { (des, o) =>
      val callNode_ =
        callNode(d, code(d), op, op, DispatchTypes.STATIC_DISPATCH, None, Some(X2CpgDefines.Any))
          .argumentIndex(o)
      val left  = astForNode(des)
      val right = astForNode(d.getOperand)
      callAst(callNode_, List(left, right))
    }
    scope.popScope()
    blockAst(node, calls.toList)
  }

  private def astForCPPASTDesignatedInitializer(d: ICPPASTDesignatedInitializer): Ast = {
    val node = blockNode(d, Defines.Empty, Defines.Void)
    scope.pushNewScope(node)
    val op = Operators.assignment
    val calls = withIndex(d.getDesignators) { (des, o) =>
      val callNode_ =
        callNode(d, code(d), op, op, DispatchTypes.STATIC_DISPATCH, None, Some(X2CpgDefines.Any))
          .argumentIndex(o)
      val left  = astForNode(des)
      val right = astForNode(d.getOperand)
      callAst(callNode_, List(left, right))
    }
    scope.popScope()
    blockAst(node, calls.toList)
  }

  private def astForCPPASTConstructorInitializer(c: ICPPASTConstructorInitializer): Ast = {
    val name      = Defines.OperatorConstructorInitializer
    val callNode_ = callNode(c, code(c), name, name, DispatchTypes.STATIC_DISPATCH, None, Some(X2CpgDefines.Any))
    val args      = c.getArguments.toList.map(a => astForNode(a))
    callAst(callNode_, args)
  }

  private def astForCASTArrayRangeDesignator(des: CASTArrayRangeDesignator): Ast = {
    val op         = Operators.arrayInitializer
    val callNode_  = callNode(des, code(des), op, op, DispatchTypes.STATIC_DISPATCH, None, Some(X2CpgDefines.Any))
    val floorAst   = nullSafeAst(des.getRangeFloor)
    val ceilingAst = nullSafeAst(des.getRangeCeiling)
    callAst(callNode_, List(floorAst, ceilingAst))
  }

  private def astForCPPASTArrayRangeDesignator(des: CPPASTArrayRangeDesignator): Ast = {
    val op         = Operators.arrayInitializer
    val callNode_  = callNode(des, code(des), op, op, DispatchTypes.STATIC_DISPATCH, None, Some(X2CpgDefines.Any))
    val floorAst   = nullSafeAst(des.getRangeFloor)
    val ceilingAst = nullSafeAst(des.getRangeCeiling)
    callAst(callNode_, List(floorAst, ceilingAst))
  }

  protected def astForNode(node: IASTNode): Ast = {
    node match {
      case expr: IASTExpression             => astForExpression(expr)
      case name: IASTName                   => astForIdentifier(name)
      case decl: IASTDeclSpecifier          => astForIdentifier(decl)
      case l: IASTInitializerList           => astForInitializerList(l)
      case c: ICPPASTConstructorInitializer => astForCPPASTConstructorInitializer(c)
      case d: ICASTDesignatedInitializer    => astForCASTDesignatedInitializer(d)
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

  protected def typeForDeclSpecifier(spec: IASTNode, stripKeywords: Boolean = true, index: Int = 0): String = {
    val tpe = spec match {
      case s: IASTSimpleDeclSpecifier if s.getParent.isInstanceOf[IASTParameterDeclaration] =>
        val parentDecl = s.getParent.asInstanceOf[IASTParameterDeclaration].getDeclarator
        pointersAsString(s, parentDecl, stripKeywords)
      case s: IASTSimpleDeclSpecifier if s.getParent.isInstanceOf[IASTFunctionDefinition] =>
        val parentDecl = s.getParent.asInstanceOf[IASTFunctionDefinition].getDeclarator
        ASTStringUtil.getReturnTypeString(s, parentDecl)
      case s: IASTSimpleDeclaration if s.getParent.isInstanceOf[ICASTKnRFunctionDeclarator] =>
        val decl = s.getDeclarators.toList(index)
        pointersAsString(s.getDeclSpecifier, decl, stripKeywords)
      case s: IASTSimpleDeclSpecifier if s.getParent.isInstanceOf[IASTSimpleDeclaration] =>
        val parentDecl = s.getParent.asInstanceOf[IASTSimpleDeclaration].getDeclarators.toList(index)
        pointersAsString(s, parentDecl, stripKeywords)
      case s: IASTSimpleDeclSpecifier =>
        ASTStringUtil.getReturnTypeString(s, null)
      case s: IASTNamedTypeSpecifier if s.getParent.isInstanceOf[IASTParameterDeclaration] =>
        val parentDecl = s.getParent.asInstanceOf[IASTParameterDeclaration].getDeclarator
        pointersAsString(s, parentDecl, stripKeywords)
      case s: IASTNamedTypeSpecifier if s.getParent.isInstanceOf[IASTSimpleDeclaration] =>
        val parentDecl = s.getParent.asInstanceOf[IASTSimpleDeclaration].getDeclarators.toList(index)
        pointersAsString(s, parentDecl, stripKeywords)
      case s: IASTNamedTypeSpecifier =>
        ASTStringUtil.getSimpleName(s.getName)
      case s: IASTCompositeTypeSpecifier if s.getParent.isInstanceOf[IASTSimpleDeclaration] =>
        val parentDecl = s.getParent.asInstanceOf[IASTSimpleDeclaration].getDeclarators.toList(index)
        pointersAsString(s, parentDecl, stripKeywords)
      case s: IASTCompositeTypeSpecifier => ASTStringUtil.getSimpleName(s.getName)
      case s: IASTEnumerationSpecifier if s.getParent.isInstanceOf[IASTSimpleDeclaration] =>
        val parentDecl = s.getParent.asInstanceOf[IASTSimpleDeclaration].getDeclarators.toList(index)
        pointersAsString(s, parentDecl, stripKeywords)
      case s: IASTEnumerationSpecifier => ASTStringUtil.getSimpleName(s.getName)
      case s: IASTElaboratedTypeSpecifier if s.getParent.isInstanceOf[IASTParameterDeclaration] =>
        val parentDecl = s.getParent.asInstanceOf[IASTParameterDeclaration].getDeclarator
        pointersAsString(s, parentDecl, stripKeywords)
      case s: IASTElaboratedTypeSpecifier if s.getParent.isInstanceOf[IASTSimpleDeclaration] =>
        val parentDecl = s.getParent.asInstanceOf[IASTSimpleDeclaration].getDeclarators.toList(index)
        pointersAsString(s, parentDecl, stripKeywords)
      case s: IASTElaboratedTypeSpecifier => ASTStringUtil.getSignatureString(s, null)
      // TODO: handle other types of IASTDeclSpecifier
      case _ => Defines.Any
    }
    if (tpe.isEmpty) Defines.Any else tpe
  }

  // We use our own call ast creation function since the version in x2cpg treats
  // base as receiver if no receiver is given which does not fit the needs of this
  // frontend.
  def createCallAst(
    callNode: NewCall,
    arguments: Seq[Ast] = List(),
    base: Option[Ast] = None,
    receiver: Option[Ast] = None
  ): Ast = {

    setArgumentIndices(arguments)

    val baseRoot = base.flatMap(_.root).toList
    val bse      = base.getOrElse(Ast())
    baseRoot match {
      case List(x: ExpressionNew) =>
        x.argumentIndex = 0
      case _ =>
    }

    var ast =
      Ast(callNode)
        .withChild(bse)

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
}
