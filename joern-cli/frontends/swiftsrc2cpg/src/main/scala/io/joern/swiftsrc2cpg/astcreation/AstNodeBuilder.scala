package io.joern.swiftsrc2cpg.astcreation

import io.joern.swiftsrc2cpg.parser.SwiftNodeSyntax.*
import io.joern.x2cpg
import io.joern.x2cpg.frontendspecific.swiftsrc2cpg.Defines
import io.joern.x2cpg.{Ast, ValidationMode}
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, EdgeTypes, Operators}

trait AstNodeBuilder(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  protected def setOrderExplicitly(ast: Ast, order: Int): Unit = {
    ast.root.foreach { case expr: ExpressionNew => expr.order = order }
  }

  protected def codeOf(node: NewNode): String = node match {
    case astNodeNew: AstNodeNew => astNodeNew.code
    case _                      => ""
  }

  private def jumpTargetFromIfConfigClauseSyntax(node: IfConfigClauseSyntax): Seq[SwiftNode] = {
    node.elements match {
      case Some(value: CodeBlockItemListSyntax)   => value.children.map(_.item)
      case Some(value: MemberBlockItemListSyntax) => value.children.map(_.decl)
      case Some(value: SwitchCaseListSyntax)      => value.children
      case Some(value: ExprSyntax)                => Seq(value)
      case Some(value: AttributeListSyntax)       => value.children
      case _                                      => Seq.empty
    }
  }

  private def jumpTargetFromIfConfigDeclSyntax(node: IfConfigDeclSyntax): Seq[SwiftNode] = {
    val children              = node.clauses.children
    val ifIfConfigClauses     = children.filter(c => code(c.poundKeyword) == "#if")
    val elseIfIfConfigClauses = children.filter(c => code(c.poundKeyword) == "#elseif")
    val elseIfConfigClauses   = children.filter(c => code(c.poundKeyword) == "#else")
    ifIfConfigClauses match {
      case Nil => Seq.empty
      case ifIfConfigClause :: Nil if ifConfigDeclConditionIsSatisfied(ifIfConfigClause) =>
        jumpTargetFromIfConfigClauseSyntax(ifIfConfigClause)
      case _ :: Nil =>
        val firstElseIfSatisfied = elseIfIfConfigClauses.find(ifConfigDeclConditionIsSatisfied)
        firstElseIfSatisfied match {
          case Some(elseIfIfConfigClause) => jumpTargetFromIfConfigClauseSyntax(elseIfIfConfigClause)
          case None =>
            elseIfConfigClauses match {
              case Nil                       => Seq.empty
              case elseIfConfigClause :: Nil => jumpTargetFromIfConfigClauseSyntax(elseIfConfigClause)
              case _                         => Seq.empty
            }
        }
      case _ => Seq.empty
    }
  }

  private def nameForJumpTarget(node: SwiftNode): String = {
    node match {
      case s: SwitchCaseSyntax => code(s.label).stripSuffix(":")
      case other               => code(other).stripSuffix(":")
    }
  }

  private def codeForJumpTarget(node: SwiftNode): String = {
    node match {
      case s: SwitchCaseSyntax => code(s.label)
      case other               => code(other)
    }
  }

  protected def createJumpTarget(switchCase: SwitchCaseSyntax | IfConfigDeclSyntax): NewJumpTarget = {
    val (switchName, switchCode) = switchCase match {
      case s: SwitchCaseSyntax =>
        (nameForJumpTarget(s), codeForJumpTarget(s))
      case i: IfConfigDeclSyntax =>
        val elements = jumpTargetFromIfConfigDeclSyntax(i)
        val elemCode = elements.headOption.fold(codeForJumpTarget(i.clauses.children.head))(codeForJumpTarget)
        val elemName = elements.headOption.fold(nameForJumpTarget(i.clauses.children.head))(nameForJumpTarget)
        (elemName, elemCode)
    }
    jumpTargetNode(switchCase, switchName, switchCode, Some(switchCase.toString))
  }

  protected def createIndexAccessCallAst(
    node: SwiftNode,
    baseAst: Ast,
    partAst: Ast,
    additionalArgsAst: Seq[Ast] = Seq.empty
  ): Ast = {
    val tpe = fullnameProvider.typeFullname(node).getOrElse(Defines.Any)
    registerType(tpe)
    val callNode_ = callNode(
      node,
      s"${codeOf(baseAst.nodes.head)}[${codeOf(partAst.nodes.head)}]",
      Operators.indexAccess,
      Operators.indexAccess,
      DispatchTypes.STATIC_DISPATCH,
      None,
      Some(tpe)
    )
    val arguments = List(baseAst, partAst) ++ additionalArgsAst
    callAst(callNode_, arguments)
  }

  protected def createFieldAccessCallAst(node: SwiftNode, baseAst: Ast, partNode: NewNode): Ast = {
    val tpe = fullnameProvider.typeFullname(node).getOrElse(Defines.Any)
    registerType(tpe)
    val callNode_ = callNode(
      node,
      s"${codeOf(baseAst.nodes.head)}.${codeOf(partNode)}",
      Operators.fieldAccess,
      Operators.fieldAccess,
      DispatchTypes.STATIC_DISPATCH,
      None,
      Some(tpe)
    )
    val arguments = List(baseAst, Ast(partNode))
    callAst(callNode_, arguments)
  }

  protected def literalNode(node: SwiftNode, code: String, possibleTypes: Option[String]): NewLiteral = {
    val typeFullName = possibleTypes match {
      case Some(value) if Defines.SwiftTypes.contains(value) => value
      case _                                                 => Defines.Any
    }
    literalNode(node, code, typeFullName).possibleTypes(possibleTypes.toList)
  }

  protected def createAssignmentCallAst(node: SwiftNode, dest: Ast, source: Ast, code: String): Ast = {
    val callNode_ = callNode(
      node,
      code,
      Operators.assignment,
      Operators.assignment,
      DispatchTypes.STATIC_DISPATCH,
      None,
      Some(Defines.Void)
    )
    val arguments = List(dest, source)
    callAst(callNode_, arguments)
  }

  protected def fullNameOfEnclosingTypeDecl(): String = {
    scope.getEnclosingTypeDeclFullName.getOrElse(Defines.Any)
  }

  protected def identifierNode(node: SwiftNode, name: String): NewIdentifier = {
    val tpe = fullnameProvider
      .typeFullname(node)
      .map { t =>
        registerType(t)
        t
      }
      .getOrElse(name match {
        case "self" | "Self" => fullNameOfEnclosingTypeDecl()
        case _               => Defines.Any
      })
    identifierNode(node, name, name, tpe)
  }

  protected def createStaticCallNode(
    node: SwiftNode,
    code: String,
    callName: String,
    fullName: String,
    typeFullName: String
  ): NewCall = callNode(node, code, callName, fullName, DispatchTypes.STATIC_DISPATCH, Some(""), Some(typeFullName))

  protected def createStaticCallForOperatorAst(node: SwiftNode, op: String, argument: SwiftNode): Ast = {
    val tpe = fullnameProvider.typeFullname(node).getOrElse(Defines.Any)
    registerType(tpe)
    val callNode_ = createStaticCallNode(node, code(node), op, op, tpe)
    val argAst    = astForNode(argument)
    callAst(callNode_, List(argAst))
  }

  protected def createFunctionBinding(method: NewMethod): Ast = {
    val parentNode: NewTypeDecl = methodAstParentStack.collectFirst { case t: NewTypeDecl => t }.get
    method.astParentFullName = parentNode.fullName
    method.astParentType = parentNode.label
    val functionBinding = NewBinding().name(method.name).methodFullName(method.fullName).signature(method.signature)
    Ast(functionBinding).withBindsEdge(parentNode, functionBinding).withRefEdge(functionBinding, method)
  }

  protected def createFunctionTypeAndTypeDecl(node: SwiftNode, methodNode: NewMethod): Unit = {
    registerType(methodNode.fullName)

    val (inherits, bindingName) = if (node.isInstanceOf[ClosureExprSyntax]) {
      val inheritsFunctionFullName = s"${Defines.Function}<${methodNode.signature}>"
      registerType(inheritsFunctionFullName)
      (Seq(inheritsFunctionFullName), Defines.ClosureApplyMethodName)
    } else (Seq.empty, methodNode.name)

    val methodTypeDeclNode = typeDeclNode(
      node,
      methodNode.name,
      methodNode.fullName,
      methodNode.filename,
      methodNode.fullName,
      inherits = inherits
    )
    Ast.storeInDiffGraph(Ast(methodTypeDeclNode), diffGraph)
    diffGraph.addEdge(methodAstParentStack.head, methodTypeDeclNode, EdgeTypes.AST)

    val functionBinding = NewBinding()
      .name(bindingName)
      .methodFullName(methodNode.fullName)
      .signature(methodNode.signature)

    val functionBindAst = Ast(functionBinding)
      .withBindsEdge(methodTypeDeclNode, functionBinding)
      .withRefEdge(functionBinding, methodNode)

    Ast.storeInDiffGraph(functionBindAst, diffGraph)
  }

}
