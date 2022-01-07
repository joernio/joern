package io.joern.c2cpg.astcreation

import io.joern.c2cpg.utils.IOUtils
import io.shiftleft.codepropertygraph.generated.nodes.{NewBlock, NewNode}
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}
import io.shiftleft.x2cpg.Ast
import org.eclipse.cdt.core.dom.ast._
import org.eclipse.cdt.core.dom.ast.c.{ICASTArrayDesignator, ICASTDesignatedInitializer, ICASTFieldDesignator}
import org.eclipse.cdt.core.dom.ast.cpp._
import org.eclipse.cdt.core.dom.ast.gnu.c.ICASTKnRFunctionDeclarator
import org.eclipse.cdt.internal.core.dom.parser.cpp.semantics.EvalBinding
import org.eclipse.cdt.internal.core.dom.parser.cpp.{CPPASTIdExpression, CPPASTQualifiedName, CPPFunction}
import org.eclipse.cdt.internal.core.model.ASTStringUtil

import java.nio.file.Paths
import scala.annotation.nowarn

object AstCreatorHelper {
  implicit class OptionSafeAst(val ast: Ast) extends AnyVal {
    def withArgEdge(src: NewNode, dst: Option[NewNode]): Ast = dst match {
      case Some(value) => ast.withArgEdge(src, value)
      case None        => ast
    }

    def withConditionEdge(src: NewNode, dst: Option[NewNode]): Ast = dst match {
      case Some(value) => ast.withConditionEdge(src, value)
      case None        => ast
    }

    def withArgEdges(src: NewNode, dsts: Seq[Ast]): Ast = {
      val args = dsts.collect { case a if a.root.isDefined => a.root.get }
      ast.withArgEdges(src, args)
    }
  }
}

trait AstCreatorHelper {

  this: AstCreator =>

  import AstCreatorHelper.OptionSafeAst

  private var usedNames: Int = 0

  protected def uniqueName(target: String, name: String, fullName: String): (String, String) = {
    if (name.isEmpty && (fullName.isEmpty || fullName.endsWith("."))) {
      val newName = s"anonymous_${target}_$usedNames"
      val newFullName = s"${fullName}anonymous_${target}_$usedNames"
      usedNames = usedNames + 1
      (newName, newFullName)
    } else {
      (name, fullName)
    }
  }

  private def fileLines(node: IASTNode): Seq[Int] = {
    val f = fileName(node)
    global.file2LinesCache.computeIfAbsent(f, _ => IOUtils.readLineLengthsInFile(Paths.get(f)))
  }

  private def nullSafeFileLocation(node: IASTNode): Option[IASTFileLocation] =
    Option(parserResult.flattenLocationsToFile(node.getNodeLocations)).map(_.asFileLocation())

  private def nullSafeFileLocationLast(node: IASTNode): Option[IASTFileLocation] =
    Option(parserResult.flattenLocationsToFile(node.getNodeLocations.lastOption.toArray)).map(_.asFileLocation())

  protected def fileName(node: IASTNode): String = {
    nullSafeFileLocation(node).map(_.getFileName).getOrElse(filename)
  }

  protected def line(node: IASTNode): Option[Integer] = {
    nullSafeFileLocation(node).map(_.getStartingLineNumber)
  }

  protected def lineEnd(node: IASTNode): Option[Integer] = {
    nullSafeFileLocationLast(node).map(_.getEndingLineNumber)
  }

  protected def column(node: IASTNode): Option[Integer] = {
    if (line(node).isEmpty) None
    else {
      val l = line(node).get - 1
      val loc = nullSafeFileLocation(node)
      if (l == 0) {
        loc.map(_.getNodeOffset)
      } else if (loc.map(_.getNodeOffset).contains(0)) {
        Some(0)
      } else {
        val slice = fileLines(node).slice(0, l)
        val length = slice.size - 1
        loc.map(_.getNodeOffset - 1 - slice.sum - length)
      }
    }
  }

  protected def columnEnd(node: IASTNode): Option[Integer] = {
    if (lineEnd(node).isEmpty) None
    else {
      val l = lineEnd(node).get - 1
      val loc = nullSafeFileLocation(node)
      if (l == 0) {
        loc.map(l => l.getNodeOffset + l.getNodeLength)
      } else if (loc.map(l => l.getNodeOffset + l.getNodeLength).contains(0)) {
        Some(0)
      } else {
        val slice = fileLines(node).slice(0, l)
        val length = slice.size - 1
        loc.map(l => l.getNodeOffset + l.getNodeLength - 1 - slice.sum - length)
      }
    }
  }

  protected def withOrder[T <: IASTNode, X](nodes: Seq[T])(f: (T, Int) => X): Seq[X] =
    nodes.zipWithIndex.map { case (x, i) =>
      f(x, i + 1)
    }

  protected def withOrder[T <: IASTNode, X](nodes: Array[T])(f: (T, Int) => X): Seq[X] =
    nodes.toIndexedSeq.zipWithIndex.map { case (x, i) =>
      f(x, i + 1)
    }

  protected def registerType(typeName: String): String = {
    val fixedTypeName = fixQualifiedName(typeName)
    global.usedTypes.putIfAbsent(fixedTypeName, true)
    fixedTypeName
  }

  // Sadly, there is no predefined List / Enum of this within Eclipse CDT:
  private val reservedTypeKeywords =
    List("const", "static", "volatile", "restrict", "extern", "typedef", "inline", "constexpr", "auto", "virtual")

  private def cleanType(t: String): String = {
    val tpe = reservedTypeKeywords.foldLeft(t)((cur, repl) => cur.replace(s"$repl ", ""))
    tpe match {
      case ""                                        => Defines.anyTypeName
      case t if t.contains("?")                      => Defines.anyTypeName
      case t if t.contains("#")                      => Defines.anyTypeName
      case t if t.startsWith("{") && t.endsWith("}") => Defines.anyTypeName
      case t if t.startsWith("[") && t.endsWith("]") => "[]"
      case t if t.contains(Defines.qualifiedNameSeparator) =>
        fixQualifiedName(t).split(".").lastOption.getOrElse(Defines.anyTypeName)
      case t if t.contains("[") && t.contains("]") => t.replace(" ", "")
      case t if t.contains("*")                    => t.replace("*", "").replace(" ", "")
      case someType                                => someType
    }
  }

  @nowarn
  protected def typeFor(node: IASTNode): String = {
    import org.eclipse.cdt.core.dom.ast.ASTSignatureUtil.getNodeSignature
    node match {
      case a: IASTArrayDeclarator if ASTTypeUtil.getNodeType(a).startsWith("? ") =>
        val tpe = getNodeSignature(a).replace("[]", "").strip()
        val arr = ASTTypeUtil.getNodeType(a).replace("? ", "")
        s"$tpe$arr"
      case a: IASTArrayDeclarator if ASTTypeUtil.getNodeType(a).contains("} ") =>
        val tpe = getNodeSignature(a).replace("[]", "").strip()
        val nodeType = ASTTypeUtil.getNodeType(node)
        val arr = nodeType.substring(nodeType.indexOf("["), nodeType.indexOf("]") + 1)
        s"$tpe$arr"
      case _: IASTIdExpression | _: IASTName | _: IASTArrayDeclarator =>
        cleanType(ASTTypeUtil.getNodeType(node))
      case d: IASTDeclSpecifier =>
        cleanType(ASTStringUtil.getReturnTypeString(d, null))
      case _ =>
        cleanType(getNodeSignature(node))
    }
  }

  private def notHandledText(node: IASTNode): String =
    s"""Node '${node.getClass.getSimpleName}' not handled yet!
       |  Code: '${node.getRawSignature}'
       |  File: '$filename'
       |  Line: ${line(node).getOrElse(-1)}
       |  """.stripMargin

  protected def notHandledYet(node: IASTNode, order: Int): Ast = {
    if (!node.isInstanceOf[IASTProblem] && !node.isInstanceOf[IASTProblemHolder]) {
      val text = notHandledText(node)
      logger.info(text)
    }
    Ast(newUnknown(node, order))
  }

  protected def nullSafeCode(node: IASTNode): String = {
    Option(node).map(nodeSignature).getOrElse("")
  }

  protected def nullSafeAst(node: IASTExpression, order: Int): Ast = {
    Option(node).map(astForNode(_, order)).getOrElse(Ast())
  }

  protected def nullSafeAst(node: IASTStatement, order: Int): Seq[Ast] = {
    Option(node).map(astsForStatement(_, order)).getOrElse(Seq.empty)
  }

  protected def fixQualifiedName(name: String): String =
    name.replace(Defines.qualifiedNameSeparator, ".")

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
    val qualifiedName = node match {
      case d: CPPASTIdExpression if d.getEvaluation.isInstanceOf[EvalBinding] =>
        val evaluation = d.getEvaluation.asInstanceOf[EvalBinding]
        evaluation.getBinding match {
          case f: CPPFunction if f.getDeclarations != null =>
            usingDeclarationMappings.getOrElse(
              fixQualifiedName(nodeSignature(d.getName)),
              f.getDeclarations.headOption.map(x => nodeSignature(x.getName)).getOrElse(f.getName)
            )
          case f: CPPFunction if f.getDefinition != null =>
            usingDeclarationMappings.getOrElse(
              fixQualifiedName(nodeSignature(d.getName)),
              nodeSignature(f.getDefinition.getName)
            )
          case other => other.getName
        }
      case alias: ICPPASTNamespaceAlias =>
        nodeSignature(alias.getMappingName)
      case namespace: ICPPASTNamespaceDefinition if nodeSignature(namespace.getName).nonEmpty =>
        fullName(namespace.getParent) + "." + nodeSignature(namespace.getName)
      case namespace: ICPPASTNamespaceDefinition if nodeSignature(namespace.getName).isEmpty =>
        fullName(namespace.getParent) + "." + uniqueName("namespace", "", "")._1
      case cppClass: ICPPASTCompositeTypeSpecifier if nodeSignature(cppClass.getName).nonEmpty =>
        fullName(cppClass.getParent) + "." + cppClass.getName.toString
      case cppClass: ICPPASTCompositeTypeSpecifier if nodeSignature(cppClass.getName).isEmpty =>
        val name = cppClass.getParent match {
          case decl: IASTSimpleDeclaration =>
            decl.getDeclarators.headOption
              .map(x => nodeSignature(x.getName))
              .getOrElse(uniqueName("composite_type", "", "")._1)
          case _ => uniqueName("composite_type", "", "")._1
        }
        val fullname = s"${fullName(cppClass.getParent)}.$name"
        fullname
      case enum: IASTEnumerationSpecifier =>
        fullName(enum.getParent) + "." + nodeSignature(enum.getName)
      case c: IASTCompositeTypeSpecifier => nodeSignature(c.getName)
      case f: IASTFunctionDeclarator if f.getName.toString.isEmpty && f.getNestedDeclarator != null =>
        fullName(f.getParent) + "." + nodeSignature(f.getNestedDeclarator.getName)
      case f: IASTFunctionDeclarator =>
        fullName(f.getParent) + "." + nodeSignature(f.getName)
      case f: ICPPASTLambdaExpression =>
        fullName(f.getParent) + "."
      case f: IASTFunctionDefinition =>
        fullName(f.getParent) + "." + nodeSignature(f.getDeclarator.getName)
      case d: IASTIdExpression    => nodeSignature(d.getName)
      case _: IASTTranslationUnit => ""
      case u: IASTUnaryExpression => nodeSignature(u.getOperand)
      case e: IASTElaboratedTypeSpecifier =>
        fullName(e.getParent) + "." + nodeSignature(e.getName)
      case other if other.getParent != null =>
        fullName(other.getParent)
      case other if other != null =>
        notHandledYet(other, -1); ""
      case null =>
        ""
    }
    val cleaned = fixQualifiedName(qualifiedName)
    if (cleaned.startsWith(".")) {
      cleaned.substring(1)
    } else cleaned
  }

  protected def shortName(node: IASTNode): String = {
    val name = node match {
      case f: ICPPASTFunctionDefinition => lastNameOfQualifiedName(f.getDeclarator.getName.toString)
      case f: IASTFunctionDefinition    => f.getDeclarator.getName.toString
      case f: IASTFunctionDeclarator if f.getName.toString.isEmpty && f.getNestedDeclarator != null =>
        f.getNestedDeclarator.getName.toString
      case f: IASTFunctionDeclarator => f.getName.toString
      case d: CPPASTIdExpression if d.getEvaluation.isInstanceOf[EvalBinding] =>
        val evaluation = d.getEvaluation.asInstanceOf[EvalBinding]
        evaluation.getBinding match {
          case f: CPPFunction if f.getDeclarations != null =>
            f.getDeclarations.headOption.map(_.getName.toString).getOrElse(f.getName)
          case f: CPPFunction if f.getDefinition != null =>
            f.getDefinition.getName.toString
          case other =>
            other.getName
        }
      case d: IASTIdExpression           => lastNameOfQualifiedName(d.getName.toString)
      case u: IASTUnaryExpression        => shortName(u.getOperand)
      case c: IASTFunctionCallExpression => shortName(c.getFunctionNameExpression)
      case other                         => notHandledYet(other, -1); ""
    }
    name
  }

  private def pointersAsString(spec: IASTDeclSpecifier, parentDecl: IASTDeclarator): String = {
    val tpe = typeFor(spec)
    val pointers = parentDecl.getPointerOperators
    val arr = parentDecl match {
      case p: IASTArrayDeclarator => "[]" * p.getArrayModifiers.length
      case _                      => ""
    }
    if (pointers.isEmpty) { s"$tpe$arr" }
    else {
      s"$tpe$arr${"*" * pointers.size}".strip()
    }
  }

  private def astforDecltypeSpecifier(decl: ICPPASTDecltypeSpecifier, order: Int): Ast = {
    val op = "operators.<typeOf>"
    val cpgUnary = newCallNode(decl, op, op, DispatchTypes.STATIC_DISPATCH, order)
    val operand = nullSafeAst(decl.getDecltypeExpression, 1)
    Ast(cpgUnary).withChild(operand).withArgEdge(cpgUnary, operand.root)
  }

  private def astForCASTDesignatedInitializer(d: ICASTDesignatedInitializer, order: Int): Ast = {
    val b = NewBlock()
      .order(order)
      .argumentIndex(order)
      .typeFullName(registerType(Defines.voidTypeName))
      .lineNumber(line(d))
      .columnNumber(column(d))
    scope.pushNewScope(b)
    val op = Operators.assignment
    val calls = withOrder(d.getDesignators) { (des, o) =>
      val callNode = newCallNode(d, op, op, DispatchTypes.STATIC_DISPATCH, o)
      val left = astForNode(des, 1)
      val right = astForNode(d.getOperand, 2)
      Ast(callNode)
        .withChild(left)
        .withChild(right)
        .withArgEdge(callNode, left.root)
        .withArgEdge(callNode, right.root)
    }
    scope.popScope()
    Ast(b).withChildren(calls)
  }

  private def astForCPPASTDesignatedInitializer(d: ICPPASTDesignatedInitializer, order: Int): Ast = {
    val b = NewBlock()
      .order(order)
      .argumentIndex(order)
      .typeFullName(registerType(Defines.voidTypeName))
      .lineNumber(line(d))
      .columnNumber(column(d))
    scope.pushNewScope(b)
    val op = Operators.assignment
    val calls = withOrder(d.getDesignators) { (des, o) =>
      val callNode = newCallNode(d, op, op, DispatchTypes.STATIC_DISPATCH, o)
      val left = astForNode(des, 1)
      val right = astForNode(d.getOperand, 2)
      Ast(callNode)
        .withChild(left)
        .withChild(right)
        .withArgEdge(callNode, left.root)
        .withArgEdge(callNode, right.root)
    }
    scope.popScope()
    Ast(b).withChildren(calls)
  }

  private def astForCPPASTConstructorInitializer(c: ICPPASTConstructorInitializer, order: Int): Ast = {
    val name = "<operator>.constructorInitializer"
    val callNode = newCallNode(c, name, name, DispatchTypes.STATIC_DISPATCH, order)
    val args = withOrder(c.getArguments) { case (a, o) => astForNode(a, o) }
    Ast(callNode).withChildren(args).withArgEdges(callNode, args)
  }

  protected def astForNode(node: IASTNode, order: Int): Ast = {
    node match {
      case id: IASTIdExpression if id.getName.isInstanceOf[CPPASTQualifiedName] =>
        astForQualifiedName(id.getName.asInstanceOf[CPPASTQualifiedName], order)
      case name: IASTName                   => astForIdentifier(name, order)
      case decl: IASTDeclSpecifier          => astForIdentifier(decl, order)
      case expr: IASTExpression             => astForExpression(expr, order)
      case l: IASTInitializerList           => astForInitializerList(l, order)
      case c: ICPPASTConstructorInitializer => astForCPPASTConstructorInitializer(c, order)
      case d: ICASTDesignatedInitializer    => astForCASTDesignatedInitializer(d, order)
      case d: ICPPASTDesignatedInitializer  => astForCPPASTDesignatedInitializer(d, order)
      case d: ICASTArrayDesignator          => nullSafeAst(d.getSubscriptExpression, order)
      case d: ICPPASTFieldDesignator        => astForNode(d.getName, order)
      case d: ICASTFieldDesignator          => astForNode(d.getName, order)
      case decl: ICPPASTDecltypeSpecifier   => astforDecltypeSpecifier(decl, order)
      case _                                => notHandledYet(node, order)
    }
  }

  protected def typeForDeclSpecifier(spec: IASTNode): String = {
    val tpe = spec match {
      case s: IASTSimpleDeclSpecifier if s.getParent.isInstanceOf[IASTParameterDeclaration] =>
        val parentDecl = s.getParent.asInstanceOf[IASTParameterDeclaration].getDeclarator
        pointersAsString(s, parentDecl)
      case s: IASTSimpleDeclSpecifier if s.getParent.isInstanceOf[IASTFunctionDefinition] =>
        val parentDecl = s.getParent.asInstanceOf[IASTFunctionDefinition].getDeclarator
        ASTStringUtil.getReturnTypeString(s, parentDecl)
      case s: IASTSimpleDeclaration if s.getParent.isInstanceOf[ICASTKnRFunctionDeclarator] =>
        val decl = s.getDeclarators.head
        pointersAsString(s.getDeclSpecifier, decl)
      case s: IASTSimpleDeclSpecifier if s.getParent.isInstanceOf[IASTSimpleDeclaration] =>
        val parentDecl = s.getParent.asInstanceOf[IASTSimpleDeclaration].getDeclarators.head
        pointersAsString(s, parentDecl)
      case s: IASTSimpleDeclSpecifier =>
        ASTStringUtil.getReturnTypeString(s, null)
      case s: IASTNamedTypeSpecifier if s.getParent.isInstanceOf[IASTParameterDeclaration] =>
        val parentDecl = s.getParent.asInstanceOf[IASTParameterDeclaration].getDeclarator
        pointersAsString(s, parentDecl)
      case s: IASTNamedTypeSpecifier     => s.getName.toString
      case s: IASTCompositeTypeSpecifier => s.getName.toString
      case s: IASTEnumerationSpecifier   => s.getName.toString
      case s: IASTElaboratedTypeSpecifier if s.getParent.isInstanceOf[IASTParameterDeclaration] =>
        val parentDecl = s.getParent.asInstanceOf[IASTParameterDeclaration].getDeclarator
        pointersAsString(s, parentDecl)
      case s: IASTElaboratedTypeSpecifier if s.getParent.isInstanceOf[IASTSimpleDeclaration] =>
        val parentDecl = s.getParent.asInstanceOf[IASTSimpleDeclaration].getDeclarators.head
        pointersAsString(s, parentDecl)
      case s: IASTElaboratedTypeSpecifier => ASTStringUtil.getSignatureString(s, null)
      // TODO: handle other types of IASTDeclSpecifier
      case _ => Defines.anyTypeName
    }
    if (tpe.isEmpty) Defines.anyTypeName else tpe
  }

}
