package io.joern.c2cpg.astcreation

import io.shiftleft.codepropertygraph.generated.nodes.{ExpressionNew, NewNode}
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}
import io.joern.x2cpg.{Ast, SourceFiles, ValidationMode}
import io.joern.x2cpg.utils.NodeBuilders.newDependencyNode
import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.utils.IOUtils
import org.apache.commons.lang3.StringUtils
import org.eclipse.cdt.core.dom.ast.*
import org.eclipse.cdt.core.dom.ast.c.{ICASTArrayDesignator, ICASTDesignatedInitializer, ICASTFieldDesignator}
import org.eclipse.cdt.core.dom.ast.cpp.*
import org.eclipse.cdt.core.dom.ast.gnu.c.ICASTKnRFunctionDeclarator
import org.eclipse.cdt.internal.core.dom.parser.c.CASTArrayRangeDesignator
import org.eclipse.cdt.internal.core.dom.parser.cpp.semantics.EvalBinding
import org.eclipse.cdt.internal.core.dom.parser.cpp.{CPPASTIdExpression, CPPFunction}
import org.eclipse.cdt.internal.core.dom.parser.cpp.CPPASTArrayRangeDesignator
import org.eclipse.cdt.internal.core.dom.parser.cpp.semantics.EvalMemberAccess
import org.eclipse.cdt.internal.core.dom.parser.cpp.CPPASTFieldReference
import org.eclipse.cdt.internal.core.dom.parser.cpp.CPPMethod
import org.eclipse.cdt.internal.core.dom.parser.cpp.ICPPEvaluation
import org.eclipse.cdt.internal.core.model.ASTStringUtil

import java.nio.file.{Path, Paths}
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

  private val IncludeKeyword = "include"

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

  protected def fileName(node: IASTNode): String = {
    val path = nullSafeFileLocation(node).map(_.getFileName).getOrElse(filename)
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

  // Sadly, there is no predefined List / Enum of this within Eclipse CDT:
  private val reservedTypeKeywords: List[String] =
    List(
      "const",
      "static",
      "volatile",
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

  protected def cleanType(rawType: String, stripKeywords: Boolean = true): String = {
    val tpe =
      if (stripKeywords) {
        reservedTypeKeywords.foldLeft(rawType) { (cur, repl) =>
          if (cur.contains(s"$repl ")) {
            dereferenceTypeFullName(cur.replace(s"$repl ", ""))
          } else {
            cur
          }
        }
      } else {
        rawType
      }
    StringUtils.normalizeSpace(tpe) match {
      case ""                                                                      => Defines.anyTypeName
      case t if t.contains("org.eclipse.cdt.internal.core.dom.parser.ProblemType") => Defines.anyTypeName
      case t if t.contains(" ->") && t.contains("}::") =>
        fixQualifiedName(t.substring(t.indexOf("}::") + 3, t.indexOf(" ->")))
      case t if t.contains(" ->") =>
        fixQualifiedName(t.substring(0, t.indexOf(" ->")))
      case t if t.contains("( ") =>
        fixQualifiedName(t.substring(0, t.indexOf("( ")))
      case t if t.contains("?") => Defines.anyTypeName
      case t if t.contains("#") => Defines.anyTypeName
      case t if t.contains("{") && t.contains("}") =>
        val anonType =
          s"${uniqueName("type", "", "")._1}${t.substring(0, t.indexOf("{"))}${t.substring(t.indexOf("}") + 1)}"
        anonType.replace(" ", "")
      case t if t.startsWith("[") && t.endsWith("]")       => Defines.anyTypeName
      case t if t.contains(Defines.qualifiedNameSeparator) => fixQualifiedName(t)
      case t if t.startsWith("unsigned ")                  => "unsigned " + t.substring(9).replace(" ", "")
      case t if t.contains("[") && t.contains("]")         => t.replace(" ", "")
      case t if t.contains("*")                            => t.replace(" ", "")
      case someType                                        => someType
    }
  }

  private def safeGetEvaluation(expr: ICPPASTExpression): Option[ICPPEvaluation] = {
    // In case of unresolved includes etc. this may fail throwing an unrecoverable exception
    Try(expr.getEvaluation).toOption
  }

  @nowarn
  protected def typeFor(node: IASTNode, stripKeywords: Boolean = true): String = {
    import org.eclipse.cdt.core.dom.ast.ASTSignatureUtil.getNodeSignature
    node match {
      case f: CPPASTFieldReference =>
        safeGetEvaluation(f.getFieldOwner) match {
          case Some(evaluation: EvalBinding) => cleanType(evaluation.getType.toString, stripKeywords)
          case _ => cleanType(ASTTypeUtil.getType(f.getFieldOwner.getExpressionType), stripKeywords)
        }
      case f: IASTFieldReference =>
        cleanType(ASTTypeUtil.getType(f.getFieldOwner.getExpressionType), stripKeywords)
      case a: IASTArrayDeclarator if ASTTypeUtil.getNodeType(a).startsWith("? ") =>
        val tpe = getNodeSignature(a).replace("[]", "").strip()
        val arr = ASTTypeUtil.getNodeType(a).replace("? ", "")
        s"$tpe$arr"
      case a: IASTArrayDeclarator
          if ASTTypeUtil.getNodeType(a).contains("} ") || ASTTypeUtil.getNodeType(a).contains(" [") =>
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
      case s: CPPASTIdExpression =>
        safeGetEvaluation(s) match {
          case Some(evaluation: EvalMemberAccess) =>
            cleanType(evaluation.getOwnerType.toString, stripKeywords)
          case Some(evalBinding: EvalBinding) =>
            evalBinding.getBinding match {
              case m: CPPMethod => cleanType(fullName(m.getDefinition))
              case _            => cleanType(ASTTypeUtil.getNodeType(s), stripKeywords)
            }
          case _ => cleanType(ASTTypeUtil.getNodeType(s), stripKeywords)
        }
      case _: IASTIdExpression | _: IASTName | _: IASTDeclarator =>
        cleanType(ASTTypeUtil.getNodeType(node), stripKeywords)
      case s: IASTNamedTypeSpecifier =>
        cleanType(ASTStringUtil.getReturnTypeString(s, null), stripKeywords)
      case s: IASTCompositeTypeSpecifier =>
        cleanType(ASTStringUtil.getReturnTypeString(s, null), stripKeywords)
      case s: IASTEnumerationSpecifier =>
        cleanType(ASTStringUtil.getReturnTypeString(s, null), stripKeywords)
      case s: IASTElaboratedTypeSpecifier =>
        cleanType(ASTStringUtil.getReturnTypeString(s, null), stripKeywords)
      case l: IASTLiteralExpression =>
        cleanType(ASTTypeUtil.getType(l.getExpressionType))
      case e: IASTExpression =>
        cleanType(ASTTypeUtil.getNodeType(e), stripKeywords)
      case c: ICPPASTConstructorInitializer if c.getParent.isInstanceOf[ICPPASTConstructorChainInitializer] =>
        cleanType(
          fullName(c.getParent.asInstanceOf[ICPPASTConstructorChainInitializer].getMemberInitializerId),
          stripKeywords
        )
      case _ =>
        cleanType(getNodeSignature(node), stripKeywords)
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

  protected def dereferenceTypeFullName(fullName: String): String =
    fullName.replace("*", "")

  protected def fixQualifiedName(name: String): String =
    name.stripPrefix(Defines.qualifiedNameSeparator).replace(Defines.qualifiedNameSeparator, ".")

  protected def isQualifiedName(name: String): Boolean =
    name.startsWith(Defines.qualifiedNameSeparator)

  protected def lastNameOfQualifiedName(name: String): String = {
    val cleanedName = if (name.contains("<") && name.contains(">")) {
      name.substring(0, name.indexOf("<"))
    } else {
      name
    }
    cleanedName.split(Defines.qualifiedNameSeparator).lastOption.getOrElse(cleanedName)
  }

  protected def fullName(node: IASTNode): String = {
    val qualifiedName: String = node match {
      case d: CPPASTIdExpression =>
        safeGetEvaluation(d) match {
          case Some(evalBinding: EvalBinding) =>
            evalBinding.getBinding match {
              case f: CPPFunction if f.getDeclarations != null =>
                f.getDeclarations.headOption.map(n => s"${fullName(n)}").getOrElse(f.getName)
              case f: CPPFunction if f.getDefinition != null =>
                s"${fullName(f.getDefinition)}"
              case other =>
                other.getName
            }
          case _ => ASTStringUtil.getSimpleName(d.getName)
        }

      case alias: ICPPASTNamespaceAlias => alias.getMappingName.toString
      case namespace: ICPPASTNamespaceDefinition if ASTStringUtil.getSimpleName(namespace.getName).nonEmpty =>
        s"${fullName(namespace.getParent)}.${ASTStringUtil.getSimpleName(namespace.getName)}"
      case namespace: ICPPASTNamespaceDefinition if ASTStringUtil.getSimpleName(namespace.getName).isEmpty =>
        s"${fullName(namespace.getParent)}.${uniqueName("namespace", "", "")._1}"
      case compType: IASTCompositeTypeSpecifier if ASTStringUtil.getSimpleName(compType.getName).nonEmpty =>
        s"${fullName(compType.getParent)}.${ASTStringUtil.getSimpleName(compType.getName)}"
      case compType: IASTCompositeTypeSpecifier if ASTStringUtil.getSimpleName(compType.getName).isEmpty =>
        val name = compType.getParent match {
          case decl: IASTSimpleDeclaration =>
            decl.getDeclarators.headOption
              .map(n => ASTStringUtil.getSimpleName(n.getName))
              .getOrElse(uniqueName("composite_type", "", "")._1)
          case _ => uniqueName("composite_type", "", "")._1
        }
        s"${fullName(compType.getParent)}.$name"
      case enumSpecifier: IASTEnumerationSpecifier =>
        s"${fullName(enumSpecifier.getParent)}.${ASTStringUtil.getSimpleName(enumSpecifier.getName)}"
      case f: ICPPASTLambdaExpression =>
        s"${fullName(f.getParent)}."
      case f: IASTFunctionDeclarator
          if ASTStringUtil.getSimpleName(f.getName).isEmpty && f.getNestedDeclarator != null =>
        s"${fullName(f.getParent)}.${shortName(f.getNestedDeclarator)}"
      case f: IASTFunctionDeclarator if f.getParent.isInstanceOf[IASTFunctionDefinition] =>
        s"${fullName(f.getParent)}"
      case f: IASTFunctionDeclarator =>
        s"${fullName(f.getParent)}.${ASTStringUtil.getSimpleName(f.getName)}"
      case f: IASTFunctionDefinition if f.getDeclarator != null =>
        s"${fullName(f.getParent)}.${ASTStringUtil.getQualifiedName(f.getDeclarator.getName)}"
      case f: IASTFunctionDefinition =>
        s"${fullName(f.getParent)}.${shortName(f)}"
      case e: IASTElaboratedTypeSpecifier =>
        s"${fullName(e.getParent)}.${ASTStringUtil.getSimpleName(e.getName)}"
      case d: IASTIdExpression                               => ASTStringUtil.getSimpleName(d.getName)
      case _: IASTTranslationUnit                            => ""
      case u: IASTUnaryExpression                            => code(u.getOperand)
      case x: ICPPASTQualifiedName                           => ASTStringUtil.getQualifiedName(x)
      case other if other != null && other.getParent != null => fullName(other.getParent)
      case other if other != null                            => notHandledYet(other); ""
      case null                                              => ""
    }
    fixQualifiedName(qualifiedName).stripPrefix(".")
  }

  protected def shortName(node: IASTNode): String = {
    val name = node match {
      case d: IASTDeclarator if ASTStringUtil.getSimpleName(d.getName).isEmpty && d.getNestedDeclarator != null =>
        shortName(d.getNestedDeclarator)
      case d: IASTDeclarator => ASTStringUtil.getSimpleName(d.getName)
      case f: ICPPASTFunctionDefinition
          if ASTStringUtil
            .getSimpleName(f.getDeclarator.getName)
            .isEmpty && f.getDeclarator.getNestedDeclarator != null =>
        shortName(f.getDeclarator.getNestedDeclarator)
      case f: ICPPASTFunctionDefinition => lastNameOfQualifiedName(ASTStringUtil.getSimpleName(f.getDeclarator.getName))
      case f: IASTFunctionDefinition
          if ASTStringUtil
            .getSimpleName(f.getDeclarator.getName)
            .isEmpty && f.getDeclarator.getNestedDeclarator != null =>
        shortName(f.getDeclarator.getNestedDeclarator)
      case f: IASTFunctionDefinition => ASTStringUtil.getSimpleName(f.getDeclarator.getName)
      case d: CPPASTIdExpression =>
        safeGetEvaluation(d) match {
          case Some(evalBinding: EvalBinding) =>
            evalBinding.getBinding match {
              case f: CPPFunction if f.getDeclarations != null =>
                f.getDeclarations.headOption.map(n => ASTStringUtil.getSimpleName(n.getName)).getOrElse(f.getName)
              case f: CPPFunction if f.getDefinition != null =>
                ASTStringUtil.getSimpleName(f.getDefinition.getName)
              case other =>
                other.getName
            }
          case _ => lastNameOfQualifiedName(ASTStringUtil.getSimpleName(d.getName))
        }
      case d: IASTIdExpression            => lastNameOfQualifiedName(ASTStringUtil.getSimpleName(d.getName))
      case u: IASTUnaryExpression         => shortName(u.getOperand)
      case c: IASTFunctionCallExpression  => shortName(c.getFunctionNameExpression)
      case s: IASTSimpleDeclSpecifier     => s.getRawSignature
      case e: IASTEnumerationSpecifier    => ASTStringUtil.getSimpleName(e.getName)
      case c: IASTCompositeTypeSpecifier  => ASTStringUtil.getSimpleName(c.getName)
      case e: IASTElaboratedTypeSpecifier => ASTStringUtil.getSimpleName(e.getName)
      case s: IASTNamedTypeSpecifier      => ASTStringUtil.getSimpleName(s.getName)
      case other                          => notHandledYet(other); ""
    }
    name
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
      val refs = "*" * (pointers.length - pointers.count(_.isInstanceOf[ICPPASTReferenceOperator]))
      s"$tpe$arr$refs".strip()
    }
  }

  protected def astsForDependenciesAndImports(iASTTranslationUnit: IASTTranslationUnit): Seq[Ast] = {
    val allIncludes = iASTTranslationUnit.getIncludeDirectives.toList.filterNot(isIncludedNode)
    allIncludes.map { include =>
      val name            = include.getName.toString
      val _dependencyNode = newDependencyNode(name, name, IncludeKeyword)
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
    val op       = "<operator>.typeOf"
    val cpgUnary = callNode(decl, code(decl), op, op, DispatchTypes.STATIC_DISPATCH)
    val operand  = nullSafeAst(decl.getDecltypeExpression)
    callAst(cpgUnary, List(operand))
  }

  private def astForCASTDesignatedInitializer(d: ICASTDesignatedInitializer): Ast = {
    val node = blockNode(d, Defines.empty, Defines.voidTypeName)
    scope.pushNewScope(node)
    val op = Operators.assignment
    val calls = withIndex(d.getDesignators) { (des, o) =>
      val callNode_ =
        callNode(d, code(d), op, op, DispatchTypes.STATIC_DISPATCH)
          .argumentIndex(o)
      val left  = astForNode(des)
      val right = astForNode(d.getOperand)
      callAst(callNode_, List(left, right))
    }
    scope.popScope()
    blockAst(node, calls.toList)
  }

  private def astForCPPASTDesignatedInitializer(d: ICPPASTDesignatedInitializer): Ast = {
    val node = blockNode(d, Defines.empty, Defines.voidTypeName)
    scope.pushNewScope(node)
    val op = Operators.assignment
    val calls = withIndex(d.getDesignators) { (des, o) =>
      val callNode_ =
        callNode(d, code(d), op, op, DispatchTypes.STATIC_DISPATCH)
          .argumentIndex(o)
      val left  = astForNode(des)
      val right = astForNode(d.getOperand)
      callAst(callNode_, List(left, right))
    }
    scope.popScope()
    blockAst(node, calls.toList)
  }

  private def astForCPPASTConstructorInitializer(c: ICPPASTConstructorInitializer): Ast = {
    val name = "<operator>.constructorInitializer"
    val callNode_ =
      callNode(c, code(c), name, name, DispatchTypes.STATIC_DISPATCH)
    val args = c.getArguments.toList.map(a => astForNode(a))
    callAst(callNode_, args)
  }

  private def astForCASTArrayRangeDesignator(des: CASTArrayRangeDesignator): Ast = {
    val op         = Operators.arrayInitializer
    val callNode_  = callNode(des, code(des), op, op, DispatchTypes.STATIC_DISPATCH)
    val floorAst   = nullSafeAst(des.getRangeFloor)
    val ceilingAst = nullSafeAst(des.getRangeCeiling)
    callAst(callNode_, List(floorAst, ceilingAst))
  }

  private def astForCPPASTArrayRangeDesignator(des: CPPASTArrayRangeDesignator): Ast = {
    val op         = Operators.arrayInitializer
    val callNode_  = callNode(des, code(des), op, op, DispatchTypes.STATIC_DISPATCH)
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
      case _ => Defines.anyTypeName
    }
    if (tpe.isEmpty) Defines.anyTypeName else tpe
  }

}
