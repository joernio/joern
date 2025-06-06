package io.joern.php2cpg.astcreation

import io.joern.php2cpg.astcreation.AstCreator.{NameConstants, TypeConstants}
import io.joern.php2cpg.datastructures.ArrayIndexTracker
import io.joern.php2cpg.parser.Domain.*
import io.joern.php2cpg.utils.PhpScopeElement
import io.joern.x2cpg.Defines.UnresolvedNamespace
import io.joern.x2cpg.utils.AstPropertiesUtil.RootProperties
import io.joern.x2cpg.{Ast, Defines, ValidationMode}
import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.nodes.{
  NewBlock,
  NewIdentifier,
  NewLiteral,
  NewMethod,
  NewNamespaceBlock,
  NewNode
}
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal

import java.nio.charset.StandardCharsets

trait AstCreatorHelper(disableFileContent: Boolean)(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  protected val globalNamespace: NewNamespaceBlock = globalNamespaceBlock()

  protected def line(phpNode: PhpNode): Option[Int] = phpNode.attributes.lineNumber

  protected def column(phpNode: PhpNode): Option[Int] = None

  protected def lineEnd(phpNode: PhpNode): Option[Int] = None

  protected def columnEnd(phpNode: PhpNode): Option[Int] = None

  protected def code(phpNode: PhpNode): String = "" // Sadly, the Php AST does not carry any code fields

  override protected def offset(phpNode: PhpNode): Option[(Int, Int)] = {
    Option.when(!disableFileContent) {
      val startPos =
        new String(fileContent.get.getBytes.slice(0, phpNode.attributes.startFilePos), StandardCharsets.UTF_8).length
      val endPos =
        new String(fileContent.get.getBytes.slice(0, phpNode.attributes.endFilePos), StandardCharsets.UTF_8).length
      (startPos, endPos)
    }
  }

  protected def intToLiteralAst(num: Int): Ast = {
    Ast(NewLiteral().code(num.toString).typeFullName(TypeConstants.Int))
  }

  protected def getTmpIdentifier(
    originNode: PhpNode,
    maybeTypeFullName: Option[String],
    prefix: String = ""
  ): NewIdentifier = {
    val name         = s"${this.scope.getNewVarTmp(prefix)}"
    val typeFullName = maybeTypeFullName.getOrElse(Defines.Any)
    identifierNode(originNode, name, s"$$$name", typeFullName)
  }

  protected def composeMethodFullName(methodName: String, appendMetaTypeDeclExt: Boolean = false): String = {
    if (methodName == NamespaceTraversal.globalNamespaceName) {
      globalNamespace.fullName
    } else {
      val className = if (appendMetaTypeDeclExt) {
        getTypeDeclPrefix.map(name => s"$name$MetaTypeDeclExtension")
      } else {
        getTypeDeclPrefix
      }

      val nameWithClass = List(className, Some(methodName)).flatten.mkString(MethodDelimiter)

      prependNamespacePrefix(nameWithClass)
    }
  }

  protected def prependNamespacePrefix(name: String): String = {
    scope.getEnclosingNamespaceNames.filterNot(_ == NamespaceTraversal.globalNamespaceName) match {
      case Nil   => name
      case names => names.appended(name).mkString(NamespaceDelimiter)
    }
  }

  private def getTypeDeclPrefix: Option[String] = {
    scope.getEnclosingTypeDeclTypeName
      .filterNot(_ == NamespaceTraversal.globalNamespaceName)
  }

  protected def codeForMethodCall(call: PhpCallExpr, targetAst: Ast, name: String): String = {
    val callOperator = if (call.isNullSafe) s"?$InstanceMethodDelimiter" else InstanceMethodDelimiter
    s"${targetAst.rootCodeOrEmpty}$callOperator$name"
  }

  protected def codeForStaticMethodCall(call: PhpCallExpr, name: String): String = {
    val className =
      call.target
        .map(astForExpr)
        .map(_.rootCode.getOrElse(UnresolvedNamespace))
        .getOrElse(UnresolvedNamespace)
    s"$className$StaticMethodDelimiter$name"
  }

  protected def composeMethodName(call: PhpCallExpr, targetAst: Option[Ast], name: String): String = {

    /** The code property may contain "?", "::", or "->", so this needs to be replaced before composing a full name.
      */
    def normalizeMethodCode(code: String): String =
      code
        .filterNot(c => c == '?' || c == ' ')
        .replace(StaticMethodDelimiter, MethodDelimiter)
        .replace(InstanceMethodDelimiter, MethodDelimiter)

    if (call.isStatic) {
      val className =
        call.target
          .map(astForExpr)
          .map(_.rootCode.map(normalizeMethodCode).getOrElse(UnresolvedNamespace))
          .getOrElse(UnresolvedNamespace)
      s"$className$MethodDelimiter$name"
    } else if (targetAst.isDefined) {
      val prefix = targetAst
        .map(_.rootCodeOrEmpty)
        .map(normalizeMethodCode)
        .get
      s"$prefix$MethodDelimiter$name"
    } else {
      name
    }
  }

  protected def dimensionFromSimpleScalar(scalar: PhpSimpleScalar, idxTracker: ArrayIndexTracker): PhpExpr = {
    val maybeIntValue = scalar match {
      case string: PhpString =>
        string.value
          .drop(1)
          .dropRight(1)
          .toIntOption

      case number => number.value.toIntOption
    }

    maybeIntValue match {
      case Some(intValue) =>
        idxTracker.updateValue(intValue)
        PhpInt(intValue.toString, scalar.attributes)

      case None =>
        scalar
    }
  }

  protected def handleVariableOccurrence(expr: PhpNode, name: String): NewNode = {
    scope.lookupVariable(name) match {
      case None =>
        val localCode = if name == NameConstants.Self then NameConstants.Self else s"$$$name"
        val local     = localNode(expr, name, localCode, Defines.Any)
        scope.addToScope(name, local) match {
          case PhpScopeElement(node: NewBlock) => diffGraph.addEdge(node, local, EdgeTypes.AST)
          case PhpScopeElement(node) =>
            node match {
              case x: NewMethod => println(x.name); println(x.fullName)
              case x            =>
            }
            println(s"Not adding anything, currently in ${node.getClass}")
          // do nothing
        }
        local
      case Some(local) => local
    }
  }
}
