package io.joern.php2cpg.astcreation

import io.joern.php2cpg.astcreation.AstCreator.{NameConstants, TypeConstants}
import io.joern.php2cpg.datastructures.ArrayIndexTracker
import io.joern.php2cpg.parser.Domain.*
import io.joern.php2cpg.utils.{BlockScope, MethodScope}
import io.joern.php2cpg.passes.SymbolSummaryPass.PhpFunction
import io.joern.x2cpg.Defines.UnresolvedNamespace
import io.joern.x2cpg.utils.AstPropertiesUtil.RootProperties
import io.joern.x2cpg.{Ast, Defines, ValidationMode}
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, EvaluationStrategies, ModifierTypes}
import io.shiftleft.codepropertygraph.generated.nodes.{
  MethodParameterIn,
  NewBlock,
  NewClosureBinding,
  NewIdentifier,
  NewLiteral,
  NewLocal,
  NewMethod,
  NewMethodParameterIn,
  NewMethodRef,
  NewModifier,
  NewNamespaceBlock,
  NewNode
}
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal

import java.nio.charset.StandardCharsets

trait AstCreatorHelper(disableFileContent: Boolean)(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  protected val globalNamespace: NewNamespaceBlock = globalNamespaceBlock()

  protected def line(phpNode: PhpNode): Option[Int] = phpNode.attributes.lineNumber

  protected def column(phpNode: PhpNode): Option[Int] = None

  protected def lineEnd(phpNode: PhpNode): Option[Int] = phpNode.attributes.lineEndNumber

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

  protected def composeMethodFullName(methodName: String): String = {
    scope.resolveIdentifier(methodName) match {
      case Some(importedMethod)                                         => importedMethod.name
      case None if methodName == NamespaceTraversal.globalNamespaceName => globalNamespace.fullName
      case None =>
        val nameWithClass = scope.createMethodNameWithSurroundingInformation(methodName)
        scope.getDeduplicatedMethodName(nameWithClass)
    }
  }

  protected def composeMethodFullNameForCall(methodName: String, appendMetaTypeDeclExt: Boolean = false): String = {
    scope.resolveIdentifier(methodName) match {
      case Some(importedMethod)                                         => importedMethod.name
      case None if methodName == NamespaceTraversal.globalNamespaceName => globalNamespace.fullName
      case None =>
        val className = Option
          .when(appendMetaTypeDeclExt)(getTypeDeclPrefix.map(name => s"$name$MetaTypeDeclExtension").orNull)
          .orElse(getTypeDeclPrefix)

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

  private def getTypeDeclPrefix: Option[String] =
    scope.getEnclosingTypeDeclTypeName.filterNot(_ == NamespaceTraversal.globalNamespaceName)

  protected def codeForMethodCall(call: PhpCallExpr, targetAst: Ast, name: String): String = {
    val callOperator = if (call.isNullSafe) s"?$InstanceMethodDelimiter" else InstanceMethodDelimiter
    s"${targetAst.rootCodeOrEmpty}$callOperator$name"
  }

  protected def codeForStaticMethodCall(call: PhpCallExpr, name: String): String = {
    call.target
      .map(astForExpr)
      .flatMap(_.rootCode)
      .map(className => s"$className$StaticMethodDelimiter$name")
      .getOrElse(name)
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

  protected def handleVariableOccurrence(
    expr: PhpNode,
    name: String,
    code: Option[String] = None,
    tfn: Option[String] = None,
    modifiers: List[String] = List.empty
  ): NewNode = {
    scope.lookupVariable(name) match {
      case None =>
        val localCode = if name == NameConstants.Self then NameConstants.Self else s"$$$name"
        val local     = localNode(expr, name, code.getOrElse(localCode), tfn.getOrElse(Defines.Any))

        modifiers.foreach { modifier =>
          val modNode = modifierNode(expr, modifier)
          diffGraph.addEdge(local, modNode, EdgeTypes.AST)
        }

        scope.addToScope(name, local) match {
          case BlockScope(block, _)              => diffGraph.addEdge(block, local, EdgeTypes.AST)
          case MethodScope(_, block, _, _, _, _) => diffGraph.addEdge(block, local, EdgeTypes.AST)
          case _                                 => // do nothing
        }

        local
      case Some(local: NewLocal) if scope.isSurroundedByArrowClosure && local.closureBindingId.isDefined =>
        val p = local
        local
      case Some(param: NewMethodParameterIn)
          if scope.isSurroundedByArrowClosure && !scope.surroundingMethodParams.contains(param.name) =>
        createClosureBindingAndLocalNode(expr, name, param)
      case Some(local: NewLocal) if scope.isSurroundedByArrowClosure =>
        createClosureBindingAndLocalNode(expr, name, local)
      case Some(local) => local
    }
  }

  private def createClosureBindingAndLocalNode(
    expr: PhpNode,
    name: String,
    existingNode: (NewLocal | NewMethodParameterIn)
  ): NewLocal = {
    val methodName = scope.surroundingScopeFullName.get // should throw error if this does not exist
    val methodRef  = scope.getSurroundingArrowClosureMethodRef

    val closureBindingId = s"$methodName:$name"

    val localTfn = existingNode match {
      case x: NewLocal             => x.typeFullName
      case _: NewMethodParameterIn => Defines.Any
    }

    val localNode_ =
      localNode(expr, name, existingNode.code, localTfn, closureBindingId = Option(closureBindingId))

    val closureBindingNode = createClosureBinding(closureBindingId)

    scope.addClosureBinding(closureBindingNode, localNode_)
    scope.addToScope(localNode_.name, localNode_)

//    diffGraph.addNode(closureBindingNode)
//    methodRef.foreach(diffGraph.addEdge(_, closureBindingNode, EdgeTypes.CAPTURE))
//
//    scope.addToScope(localNode_.name, localNode_) match {
//      case BlockScope(block, _)              => diffGraph.addEdge(block, localNode_, EdgeTypes.AST)
//      case MethodScope(_, block, _, _, _, _) => diffGraph.addEdge(block, localNode_, EdgeTypes.AST)
//      case _                                 => // do nothing
//    }

    localNode_
  }

  protected def createClosureBinding(closureBindingId: String): NewClosureBinding =
    NewClosureBinding().closureBindingId(closureBindingId).evaluationStrategy(EvaluationStrategies.BY_SHARING)

  protected def staticInitMethodAst(node: PhpNode, methodNode: NewMethod, body: Ast, returnType: String): Ast = {
    val staticModifier = NewModifier().modifierType(ModifierTypes.STATIC)
    val methodReturn   = methodReturnNode(node, returnType)
    methodAst(methodNode, Nil, body, methodReturn, List(staticModifier))
  }

  protected def astForIdentifierWithLocalRef(ident: NewIdentifier, refLocal: NewNode): Ast =
    Ast(ident).withRefEdge(ident, refLocal)

  protected def getArgsCode(call: PhpCallExpr, arguments: Seq[Ast]): String = {
    arguments
      .zip(call.args.collect { case x: PhpArg => x.unpack })
      .map {
        case (arg, true)  => s"...${arg.rootCodeOrEmpty}"
        case (arg, false) => arg.rootCodeOrEmpty
      }
      .mkString(",")
  }

  protected def getCallName(call: PhpCallExpr, nameAst: Option[Ast]): String = {
    nameAst
      .map(_.rootCodeOrEmpty)
      .getOrElse(call.methodName match {
        case nameExpr: PhpNameExpr => nameExpr.name
        case other =>
          logger.error(s"Found unexpected call target type: Crash for now to handle properly later: $other")
          ???
      })
      .split("\\\\") // call names may be fully qualified
      .last
  }

  protected def getMfn(call: PhpCallExpr, name: String): String = {
    lazy val default               = s"$UnresolvedNamespace$MethodDelimiter$name"
    lazy val maybeResolvedFunction = scope.resolveIdentifier(name).filter(_.isInstanceOf[PhpFunction])
    call.target match {
      case Some(nameExpr: PhpNameExpr) if call.isStatic =>
        // Static method call with a simple receiver
        if (nameExpr.name == NameConstants.Self)
          composeMethodFullNameForCall(name, appendMetaTypeDeclExt = !scope.isSurroundedByMetaclassTypeDecl)
        else s"${nameExpr.name}$MetaTypeDeclExtension$MethodDelimiter$name"
      case Some(_) =>
        // As soon as we have a dynamic component to the call, we can't truly define a method full name
        default
      // Function call resolved as either defined in current script or by import
      case None if maybeResolvedFunction.isDefined      => maybeResolvedFunction.get.name
      case None if PhpBuiltins.FuncNames.contains(name) =>
        // No signature/namespace for MFN for builtin functions to ensure stable names as type info improves.
        name
      // Assume name-space local function call
      case None =>
        composeMethodFullNameForCall(name, call.isStatic)
    }
  }

  protected def isBuiltinFunc(name: String): Boolean = PhpBuiltins.FuncNames.contains(name)

  protected def isCallOnVariable(call: PhpCallExpr): Boolean =
    call.target.isEmpty && call.methodName.isInstanceOf[PhpVariable]

  protected def createListExprCodeField(listExpr: PhpListExpr): String = {
    val name = PhpOperators.listFunc
    val args = listExpr.items.flatten
      .map {
        case PhpArrayItem(_, _ @PhpVariable(name: PhpNameExpr, _), _, _, _) => s"$$${name.name}"
        case PhpArrayItem(_, value: PhpListExpr, _, _, _)                   => createListExprCodeField(value)
        case x =>
          logger.warn(s"Invalid arg type for code field: ${x.getClass}")
          ""
      }
      .mkString(",")

    s"$name($args)"
  }
}
