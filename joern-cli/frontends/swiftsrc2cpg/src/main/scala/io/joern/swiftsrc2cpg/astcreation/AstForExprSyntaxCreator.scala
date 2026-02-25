package io.joern.swiftsrc2cpg.astcreation

import io.joern.swiftsrc2cpg.parser.SwiftNodeSyntax.*
import io.joern.swiftsrc2cpg.passes.GlobalBuiltins
import io.joern.x2cpg
import io.joern.x2cpg.datastructures.Stack.*
import io.joern.x2cpg.datastructures.VariableScopeManager
import io.joern.x2cpg.frontendspecific.swiftsrc2cpg.Defines
import io.joern.x2cpg.{Ast, ValidationMode}
import io.shiftleft.codepropertygraph.generated.*
import io.shiftleft.codepropertygraph.generated.nodes.{ExpressionNew, NewCall}

import scala.annotation.unused

trait AstForExprSyntaxCreator(implicit withSchemaValidation: ValidationMode) {
  this: AstCreator =>

  private val MaxInitializers = 1000

  private def astForEmptyListLikeExpr(node: SwiftNode): Ast = {
    val op  = Operators.arrayInitializer
    val tpe = fullnameProvider.typeFullname(node).getOrElse(Defines.Any)
    registerType(tpe)
    val initCallNode = createStaticCallNode(node, code(node), op, op, tpe)
    callAst(initCallNode, List.empty)
  }

  private def astForListLikeExpr(node: SwiftNode, elements: Seq[SwiftNode]): Ast = {
    if (elements.isEmpty) { astForEmptyListLikeExpr(node) }
    else {
      node match {
        case _: (ArrayExprSyntax | TupleExprSyntax) =>
          val op  = Operators.arrayInitializer
          val tpe = fullnameProvider.typeFullname(node).getOrElse(Defines.Any)
          registerType(tpe)
          val initCallNode = createStaticCallNode(node, code(node), op, op, tpe)

          val clauses = elements.slice(0, MaxInitializers)

          val args = clauses.map(astForNode)

          val ast = callAst(initCallNode, args)
          if (elements.sizeIs > MaxInitializers) {
            val placeholder =
              literalNode(node, "<too-many-initializers>", Defines.Any).argumentIndex(MaxInitializers)
            ast.withChild(Ast(placeholder)).withArgEdge(initCallNode, placeholder)
          } else {
            ast
          }
        case other =>
          val blockNode_ = blockNode(node, code(node), Defines.Any)

          scope.pushNewBlockScope(blockNode_)
          localAstParentStack.push(blockNode_)

          val tmpName      = scopeLocalUniqueName("tmp")
          val localTmpNode = localNode(node, tmpName, tmpName, Defines.Any).order(0)
          diffGraph.addEdge(localAstParentStack.head, localTmpNode, EdgeTypes.AST)

          val slicedElements = elements.slice(0, MaxInitializers).toList

          val propertiesAsts = slicedElements.map {
            case dictElement: DictionaryElementSyntax =>
              val lhsAst = astForNode(dictElement.key)
              val rhsAst = astForNode(dictElement.value)

              val lhsTmpNode = identifierNode(dictElement, tmpName)
              scope.addVariableReference(tmpName, lhsTmpNode, Defines.Any, EvaluationStrategies.BY_REFERENCE)

              val lhsIndexAccessCallAst = createIndexAccessCallAst(dictElement, Ast(lhsTmpNode), lhsAst)

              createAssignmentCallAst(
                dictElement,
                lhsIndexAccessCallAst,
                rhsAst,
                s"${codeOf(lhsIndexAccessCallAst.nodes.head)} = ${codeOf(rhsAst.nodes.head)}"
              )
            case other => astForNode(other)
          }

          val tmpNode = identifierNode(node, tmpName)
          scope.addVariableReference(tmpName, tmpNode, Defines.Any, EvaluationStrategies.BY_REFERENCE)

          scope.popScope()
          localAstParentStack.pop()

          val placeHolderAst = if (elements.sizeIs > MaxInitializers) {
            val placeholder = literalNode(node, "<too-many-initializers>", Defines.Any)
            Ast(placeholder)
          } else {
            Ast()
          }

          val childrenAsts = propertiesAsts :+ placeHolderAst :+ Ast(tmpNode)
          blockAst(blockNode_, childrenAsts)
      }
    }
  }

  private def astForArrayExprSyntax(node: ArrayExprSyntax): Ast = {
    astForListLikeExpr(node, node.elements.children)
  }

  private def astForArrowExprSyntax(node: ArrowExprSyntax): Ast = notHandledYet(node)

  private def astForAsExprSyntax(node: AsExprSyntax): Ast = {
    val op             = Operators.cast
    val tpeNode        = node.`type`
    val tpeCode        = code(tpeNode)
    val tpeFromTypeMap = fullnameProvider.typeFullname(node)
    val tpe            = tpeFromTypeMap.getOrElse(AstCreatorHelper.cleanType(tpeCode))
    registerType(tpe)
    val cpgCastExpression = createStaticCallNode(node, code(node), op, op, tpe)
    val expr              = astForNode(node.expression)
    val typeRefNode_      = typeRefNode(tpeNode, tpeCode, tpe)
    val arg               = Ast(typeRefNode_)
    callAst(cpgCastExpression, List(arg, expr))
  }

  private def astForAssignmentExprSyntax(node: AssignmentExprSyntax): Ast = notHandledYet(node)

  private def astForAwaitExprSyntax(node: AwaitExprSyntax): Ast = {
    val op  = "<operator>.await"
    val tpe = fullnameProvider.typeFullname(node).getOrElse(Defines.Any)
    registerType(tpe)
    val callNode_ = createStaticCallNode(node, code(node), op, op, tpe)
    val argAsts   = List(astForNode(node.expression))
    callAst(callNode_, argAsts)
  }

  private def astForBinaryOperatorExprSyntax(node: BinaryOperatorExprSyntax): Ast = notHandledYet(node)

  private def astForBooleanLiteralExprSyntax(node: BooleanLiteralExprSyntax): Ast = {
    astForNode(node.literal)
  }

  private def astForBorrowExprSyntax(node: BorrowExprSyntax): Ast = {
    astForNode(node.expression)
  }

  private def astForCanImportExprSyntax(node: CanImportExprSyntax): Ast               = notHandledYet(node)
  private def astForCanImportVersionInfoSyntax(node: CanImportVersionInfoSyntax): Ast = notHandledYet(node)

  private def astForClosureExprSyntax(node: ClosureExprSyntax): Ast = {
    astForNode(node)
  }

  private def astForConsumeExprSyntax(node: ConsumeExprSyntax): Ast = {
    astForNode(node.expression)
  }

  private def astForCopyExprSyntax(node: CopyExprSyntax): Ast = {
    astForNode(node.expression)
  }

  private def astForDeclReferenceExprSyntax(node: DeclReferenceExprSyntax): Ast = {
    astForIdentifier(node)
  }

  private def astForDictionaryExprSyntax(node: DictionaryExprSyntax): Ast = {
    node.content match {
      case t: SwiftToken                  => astForListLikeExpr(node, Seq(t))
      case d: DictionaryElementListSyntax => astForListLikeExpr(node, d.children)
    }
  }

  private def astForDiscardAssignmentExprSyntax(node: DiscardAssignmentExprSyntax): Ast = {
    val name   = scopeLocalUniqueName("wildcard")
    val idNode = identifierNode(node, name)
    scope.addVariableReference(name, idNode, Defines.Any, EvaluationStrategies.BY_REFERENCE)
    Ast(idNode)
  }

  private def astForDoExprSyntax(node: DoExprSyntax): Ast = notHandledYet(node)

  private def astForEditorPlaceholderExprSyntax(node: EditorPlaceholderExprSyntax): Ast = {
    Ast(literalNode(node, code(node), Option(Defines.String)))
  }

  private def astForFloatLiteralExprSyntax(node: FloatLiteralExprSyntax): Ast = {
    astForNode(node.literal)
  }

  private def astForForceUnwrapExprSyntax(node: ForceUnwrapExprSyntax): Ast = {
    astForNode(node.expression)
  }

  private def setFullNameInfoForCall(callExpr: FunctionCallExprSyntax, callNode: NewCall): Unit = {
    fullnameProvider.declFullname(callExpr).foreach { fullNameWithSignature =>
      val (fullName, signature) = methodInfoFromFullNameWithSignature(fullNameWithSignature)
      val typeFullName          = fullnameProvider.typeFullname(callExpr).getOrElse(Defines.Any)
      registerType(typeFullName)
      callNode.methodFullName(s"$fullName:$signature")
      callNode.signature(signature)
      callNode.typeFullName(typeFullName)
    }
  }

  private def createBuiltinStaticCall(callExpr: FunctionCallExprSyntax, callee: ExprSyntax, fullName: String): Ast = {
    val callName = callee match {
      case m: MemberAccessExprSyntax => code(m.declName)
      case _                         => code(callee)
    }
    val callNode = createStaticCallNode(callee, code(callExpr), callName, fullName, Defines.Any)
    setFullNameInfoForCall(callExpr, callNode)

    val trailingClosureAsts            = callExpr.trailingClosure.toList.map(astForNode)
    val additionalTrailingClosuresAsts = callExpr.additionalTrailingClosures.children.map(c => astForNode(c.closure))
    val argAsts = callExpr.arguments.children.map(astForNode) ++ trailingClosureAsts ++ additionalTrailingClosuresAsts
    callAst(callNode, argAsts)
  }

  private def handleCallNodeArgs(callExpr: FunctionCallExprSyntax, baseAst: Ast, callName: String): Ast = {
    val trailingClosureAsts            = callExpr.trailingClosure.toList.map(astForNode)
    val additionalTrailingClosuresAsts = callExpr.additionalTrailingClosures.children.map(c => astForNode(c.closure))

    val args = callExpr.arguments.children.map(astForNode) ++ trailingClosureAsts ++ additionalTrailingClosuresAsts

    val callExprCode = code(callExpr)
    val callCode = if (callExprCode.startsWith(".")) {
      s"${codeOf(baseAst.root.get)}$callExprCode"
    } else if (callExprCode.contains("#if ")) {
      s"${codeOf(baseAst.root.get)}.$callName(${code(callExpr.arguments)})"
    } else callExprCode
    val callNode_ = callNode(
      callExpr,
      callCode,
      callName,
      x2cpg.Defines.DynamicCallUnknownFullName,
      DispatchTypes.DYNAMIC_DISPATCH,
      None,
      Option(Defines.Any)
    )
    setFullNameInfoForCall(callExpr, callNode_)

    callAst(callNode_, args, Option(baseAst))
  }

  private def astForConstructorInvocation(expr: FunctionCallExprSyntax): Ast = {
    astForConstructorInvocationCommon(expr)(finalizeSwiftInitCall)
  }

  private def astForObjcConstructorInvocation(expr: FunctionCallExprSyntax): Ast = {
    astForConstructorInvocationCommon(expr)(finalizeObjcInitCall)
  }

  private def finalizeSwiftInitCall(expr: FunctionCallExprSyntax, constructorCallNode: NewCall, tpe: String): Unit = {
    // Use decl/type info as provided by the fullnameProvider
    // (methodFullName: "<fullName>:<signature>", signature: "<signature>", typeFullName: call's type).
    setFullNameInfoForCall(expr, constructorCallNode)
  }

  private def parseObjcInitDeclFullname(fullNameFromCompiler: String): (String, String) = {
    def splitAt(marker: String, add: Int): Option[(String, String)] = {
      val idx = fullNameFromCompiler.indexOf(marker)
      if (idx >= 0) {
        val fn   = fullNameFromCompiler.substring(0, idx + add)
        val rest = fullNameFromCompiler.substring(idx + marker.length)
        Some((fn, rest))
      } else None
    }

    splitAt(".init", 0)
      .orElse(splitAt(")init", 1))
      .getOrElse(methodInfoFromFullNameWithSignature(fullNameFromCompiler))
  }

  private def finalizeObjcInitCall(expr: FunctionCallExprSyntax, constructorCallNode: NewCall, tpe: String): Unit = {
    // For ObjC constructors: derive signature and return type from constructed type.
    val (fullName, argumentsString) = fullnameProvider
      .declFullname(expr)
      .map(parseObjcInitDeclFullname)
      .getOrElse((tpe, "()"))

    val arguments = Option(argumentsString).map(_.trim).filter(_.nonEmpty).getOrElse("()")
    val signature = s"$arguments->$tpe"

    constructorCallNode.methodFullName(s"$fullName.init:$signature")
    constructorCallNode.signature(signature)
    constructorCallNode.typeFullName(tpe)
  }

  private def astForConstructorInvocationCommon(
    expr: FunctionCallExprSyntax
  )(finalizeInitCall: (FunctionCallExprSyntax, NewCall, String) => Unit): Ast = {
    // get call is safe as this function is guarded by isRefToConstructor/isRefToObjcConstructor
    val tpe = fullnameProvider.typeFullname(expr).get
    registerType(tpe)

    val callExprCode = code(expr)
    val blockNode_   = blockNode(expr, callExprCode, tpe)
    scope.pushNewBlockScope(blockNode_)

    val tmpNodeName  = scopeLocalUniqueName("tmp")
    val localTmpNode = localNode(expr, tmpNodeName, tmpNodeName, tpe).order(0)
    diffGraph.addEdge(blockNode_, localTmpNode, EdgeTypes.AST)
    scope.addVariable(tmpNodeName, localTmpNode, tpe, VariableScopeManager.ScopeType.BlockScope)

    val tmpNode = identifierNode(expr, tmpNodeName, tmpNodeName, tpe)
    scope.addVariableReference(tmpNodeName, tmpNode, tpe, EvaluationStrategies.BY_SHARING)

    val allocOp          = Operators.alloc
    val allocCallNode    = callNode(expr, allocOp, allocOp, allocOp, DispatchTypes.STATIC_DISPATCH)
    val assignmentCallOp = Operators.assignment
    val assignmentCallNode =
      callNode(expr, s"$tmpNodeName = $allocOp", assignmentCallOp, assignmentCallOp, DispatchTypes.STATIC_DISPATCH)
    val assignmentAst = callAst(assignmentCallNode, List(Ast(tmpNode), Ast(allocCallNode)))

    val baseNode = identifierNode(expr, tmpNodeName, tmpNodeName, tpe)
    scope.addVariableReference(tmpNodeName, baseNode, tpe, EvaluationStrategies.BY_SHARING)

    val constructorCallNode = callNode(
      expr,
      callExprCode,
      "init",
      x2cpg.Defines.UnresolvedNamespace,
      DispatchTypes.STATIC_DISPATCH,
      Some(x2cpg.Defines.UnresolvedSignature),
      Some(Defines.Void)
    )
    finalizeInitCall(expr, constructorCallNode, tpe)

    val trailingClosureAsts            = expr.trailingClosure.toList.map(astForNode)
    val additionalTrailingClosuresAsts = expr.additionalTrailingClosures.children.map(c => astForNode(c.closure))
    val args = expr.arguments.children.map(astForNode) ++ trailingClosureAsts ++ additionalTrailingClosuresAsts

    val constructorCallAst = callAst(constructorCallNode, args, base = Some(Ast(baseNode)))

    val retNode = identifierNode(expr, tmpNodeName, tmpNodeName, tpe)
    scope.addVariableReference(tmpNodeName, retNode, tpe, EvaluationStrategies.BY_SHARING)

    scope.popScope()
    Ast(blockNode_).withChildren(Seq(assignmentAst, constructorCallAst, Ast(retNode)))
  }

  private def isRefToExtensionMethod(node: FunctionCallExprSyntax): Boolean = {
    fullnameProvider.declFullnameRaw(node).exists(_.contains("<extension>"))
  }

  private def astForExtensionMethodCall(node: FunctionCallExprSyntax, baseAst: Ast, callName: String): Ast = {
    val callNode =
      createStaticCallNode(node, code(node), callName, x2cpg.Defines.DynamicCallUnknownFullName, Defines.Any)

    fullnameProvider.declFullname(node).foreach { fullNameWithSignature =>
      val (fullName, signature) = methodInfoFromFullNameWithSignature(fullNameWithSignature)
      val typeFullName          = fullnameProvider.typeFullname(node).getOrElse(Defines.Any)
      registerType(typeFullName)
      callNode.methodFullName(MethodInfo.fullNameToExtensionFullName(s"$fullName:$signature", callName))
      callNode.signature(signature)
      callNode.typeFullName(typeFullName)
    }

    val trailingClosureAsts            = node.trailingClosure.toList.map(astForNode)
    val additionalTrailingClosuresAsts = node.additionalTrailingClosures.children.map(c => astForNode(c.closure))
    val argAsts = node.arguments.children.map(astForNode) ++ trailingClosureAsts ++ additionalTrailingClosuresAsts
    setArgumentIndices(argAsts)

    val baseRoot = baseAst.root.toList
    baseRoot match {
      case List(x: ExpressionNew) => x.argumentIndex = 0
      case _                      =>
    }

    Ast(callNode)
      .withChild(baseAst)
      .withChildren(argAsts)
      .withArgEdges(callNode, baseRoot)
      .withArgEdges(callNode, argAsts.flatMap(_.root))
  }

  private def astForFunctionCallExprSyntax(node: FunctionCallExprSyntax): Ast = {
    val callee     = node.calledExpression
    val calleeCode = code(callee)
    if (GlobalBuiltins.builtins.contains(calleeCode)) {
      createBuiltinStaticCall(node, callee, calleeCode)
    } else {
      callee match {
        case m: MemberAccessExprSyntax if isRefToExtensionMethod(node) =>
          val memberCode = code(m.declName)
          val baseAst = m.base match {
            case Some(base) if code(base) != "self" => astForNode(base)
            case _ =>
              val selfTpe  = fullNameOfEnclosingTypeDecl()
              val selfNode = identifierNode(node, "self", "self", selfTpe)
              scope.addVariableReference("self", selfNode, selfTpe, EvaluationStrategies.BY_REFERENCE)
              Ast(selfNode)
          }
          astForExtensionMethodCall(node, baseAst, memberCode)
        case m: MemberAccessExprSyntax if m.base.isEmpty || code(m.base.get) == "self" =>
          // referencing implicit self
          val selfTpe  = fullNameOfEnclosingTypeDecl()
          val selfNode = identifierNode(node, "self", "self", selfTpe)
          scope.addVariableReference("self", selfNode, selfTpe, EvaluationStrategies.BY_REFERENCE)
          handleCallNodeArgs(node, Ast(selfNode), code(m.declName.baseName))
        case m: MemberAccessExprSyntax if isRefToStaticFunction(calleeCode) =>
          createBuiltinStaticCall(node, callee, calleeCode)
        case m: MemberAccessExprSyntax =>
          val memberCode = code(m.declName)
          handleCallNodeArgs(node, astForNode(m.base.get), memberCode)
        case other if isRefToConstructor(node, other) =>
          astForConstructorInvocation(node)
        case other if isRefToObjcConstructor(node, other) =>
          astForObjcConstructorInvocation(node)
        case other if isRefToClosure(node, other) =>
          astForClosureCall(node)
        case declReferenceExprSyntax: DeclReferenceExprSyntax if code(declReferenceExprSyntax) != "self" =>
          val selfTpe  = fullNameOfEnclosingTypeDecl()
          val selfNode = identifierNode(declReferenceExprSyntax, "self", "self", selfTpe)
          scope.addVariableReference(selfNode.name, selfNode, selfTpe, EvaluationStrategies.BY_REFERENCE)
          handleCallNodeArgs(node, Ast(selfNode), calleeCode)
        case other =>
          handleCallNodeArgs(node, astForNode(other), calleeCode)
      }
    }
  }

  private def astForClosureCall(expr: FunctionCallExprSyntax): Ast = {
    val tpe = fullnameProvider.typeFullname(expr).getOrElse(Defines.Any)
    registerType(tpe)
    val signature = fullnameProvider.typeFullnameRaw(expr.calledExpression).getOrElse(x2cpg.Defines.UnresolvedSignature)
    val callName  = Defines.ClosureApplyMethodName
    val callMethodFullname = s"${Defines.Function}<$signature>.$callName:$signature"
    val baseAst            = astForIdentifier(expr.calledExpression)

    val trailingClosureAsts            = expr.trailingClosure.toList.map(astForNode)
    val additionalTrailingClosuresAsts = expr.additionalTrailingClosures.children.map(c => astForNode(c.closure))
    val args = expr.arguments.children.map(astForNode) ++ trailingClosureAsts ++ additionalTrailingClosuresAsts

    val callExprCode = code(expr)
    val callNode_ = callNode(
      expr,
      callExprCode,
      callName,
      callMethodFullname,
      DispatchTypes.DYNAMIC_DISPATCH,
      Option(signature),
      Option(tpe)
    )
    callAst(callNode_, args, Option(baseAst))
  }

  private def isRefToClosure(func: FunctionCallExprSyntax, node: ExprSyntax): Boolean = {
    if (!config.swiftBuild) {
      // Early exit; without types from the compiler we will be unable to identify closure calls anyway.
      // This saves us the fullnameProvider lookups below.
      return false
    }
    node match {
      case refExpr: DeclReferenceExprSyntax
          if refExpr.baseName.isInstanceOf[identifier] &&
            fullnameProvider.declFullname(func).isEmpty &&
            fullnameProvider.typeFullname(refExpr).exists(_.startsWith(s"${Defines.Function}<")) =>
        true
      case _ => false
    }
  }

  private def isRefToStaticFunction(calleeCode: String): Boolean = {
    // TODO: extend the GsonTypeInfoReader to query for information whether the call is a call to a static function
    calleeCode.headOption.exists(_.isUpper) && !calleeCode.contains("(") && !calleeCode.contains(")")
  }

  private def isRefToConstructorCommon(func: FunctionCallExprSyntax, node: ExprSyntax)(
    matchesFullName: String => Boolean
  ): Boolean = {
    if (!config.swiftBuild) {
      // Early exit; without types from the compiler we will be unable to identify constructor calls anyway.
      // This saves us the fullnameProvider lookups below.
      return false
    }

    node match {
      case refExpr: DeclReferenceExprSyntax
          if refExpr.baseName.isInstanceOf[identifier] &&
            fullnameProvider.typeFullname(func).nonEmpty &&
            fullnameProvider.declFullname(func).exists(matchesFullName) =>
        true
      case _ => false
    }
  }

  private def isRefToConstructor(func: FunctionCallExprSyntax, node: ExprSyntax): Boolean = {
    isRefToConstructorCommon(func, node) { fullName =>
      fullName.contains(".init(") && fullName.contains(")->")
    }
  }

  private def isRefToObjcConstructor(func: FunctionCallExprSyntax, node: ExprSyntax): Boolean = {
    isRefToConstructorCommon(func, node) { fullName =>
      val typeFullName = fullnameProvider.typeFullname(func).get
      fullName == s"$typeFullName.init" ||
      (AstCreatorHelper.isObjcCall(fullName) && (fullName.contains(")init") || fullName.contains(".init")))
    }
  }

  private def astForGenericSpecializationExprSyntax(node: GenericSpecializationExprSyntax): Ast = {
    astForNode(node.expression)
  }

  private def astForIfExprSyntax(node: IfExprSyntax): Ast = {
    val code         = this.code(node)
    val ifNode       = controlStructureNode(node, ControlStructureTypes.IF, code)
    val conditionAst = astForNode(node.conditions)
    val thenAst      = astForNode(node.body)
    val elseAst = node.elseBody match {
      case Some(value) => astForNode(value)
      case None        => Ast()
    }
    controlStructureAst(ifNode, Option(conditionAst), Seq(thenAst, elseAst))
  }

  private def astForInOutExprSyntax(node: InOutExprSyntax): Ast = {
    val op = Defines.PrefixOperatorMap(code(node.ampersand))
    createStaticCallForOperatorAst(node, op, node.expression)
  }

  private def astForInfixOperatorExprSyntax(node: InfixOperatorExprSyntax): Ast = {
    val op  = Defines.InfixOperatorMap(code(node.operator))
    val tpe = fullnameProvider.typeFullname(node).getOrElse(Defines.Any)
    registerType(tpe)

    val lhsAst    = astForNode(node.leftOperand)
    val rhsAst    = astForNode(node.rightOperand)
    val callNode_ = createStaticCallNode(node, code(node), op, op, tpe)
    val argAsts   = List(lhsAst, rhsAst)
    callAst(callNode_, argAsts)
  }

  private def astForIntegerLiteralExprSyntax(node: IntegerLiteralExprSyntax): Ast = {
    astForNode(node.literal)
  }

  private def astForIsExprSyntax(node: IsExprSyntax): Ast = {
    val op     = Operators.instanceOf
    val lhsAst = astForNode(node.expression)

    val tpeNode = node.`type`
    val tpe     = simpleTypeNameForTypeSyntax(tpeNode)
    registerType(tpe)

    val callNode_    = createStaticCallNode(node, code(node), op, op, Defines.Bool)
    val typeRefNode_ = typeRefNode(node, code(tpeNode), tpe)

    val argAsts = List(lhsAst, Ast(typeRefNode_))
    callAst(callNode_, argAsts)
  }

  private def astForKeyPathExprSyntax(node: KeyPathExprSyntax): Ast = notHandledYet(node)

  private def astForMacroExpansionExprSyntax(node: MacroExpansionExprSyntax): Ast = {
    val nodeCode = code(node.macroName)
    val fullName = fullnameProvider.declFullname(node).getOrElse(nodeCode)
    val tpe      = fullnameProvider.typeFullname(node).getOrElse(Defines.Any)
    registerType(tpe)

    val trailingClosureAsts            = node.trailingClosure.toList.map(astForNode)
    val additionalTrailingClosuresAsts = node.additionalTrailingClosures.children.map(c => astForNode(c.closure))

    val argAsts = astForNode(node.arguments) +: (trailingClosureAsts ++ additionalTrailingClosuresAsts)
    val callNode =
      NewCall()
        .name(nodeCode)
        .dispatchType(DispatchTypes.INLINED)
        .methodFullName(fullName)
        .code(code(node))
        .typeFullName(tpe)
        .lineNumber(line(node))
        .columnNumber(column(node))
    callAst(callNode, argAsts)
  }

  private def astForMemberAccessExprSyntax(node: MemberAccessExprSyntax): Ast = {
    val base   = node.base
    val member = node.declName
    val baseAst = base match {
      case None =>
        // Swift's documentation refers to this as "implicit member expression" or "shorthand syntax for enumeration cases".
        // This syntax is not limited to enums. It works with several Swift types where the compiler can infer the type from context
        // to access static members. This is a commonly used pattern.
        // For enums only we could emit a Literal AST node representing the enum member. But as it works for other types as well,
        // we emit an unknown node here to avoid making potentially wrong assumptions.
        val code_        = code(member.baseName)
        val unknownNode_ = unknownNode(node, code_)
        return Ast(unknownNode_)
      case Some(otherBase) =>
        astForNode(otherBase)
    }

    member.baseName match {
      case l @ integerLiteral(_) =>
        val memberNode = astForIntegerLiteralToken(l)
        createIndexAccessCallAst(node, baseAst, memberNode)
      case other =>
        val memberNode = fieldIdentifierNode(other, code(other), code(other))
        createFieldAccessCallAst(node, baseAst, memberNode)
    }
  }

  private def astForMissingExprSyntax(@unused node: MissingExprSyntax): Ast = Ast()

  private def astForNilLiteralExprSyntax(node: NilLiteralExprSyntax): Ast = {
    Ast(literalNode(node, code(node), Option(Defines.Nil)))
  }

  private def astForOptionalChainingExprSyntax(node: OptionalChainingExprSyntax): Ast = {
    astForNode(node.expression)
  }

  private def astForPackElementExprSyntax(node: PackElementExprSyntax): Ast = {
    astForNode(node.pack)
  }

  private def astForPackExpansionExprSyntax(node: PackExpansionExprSyntax): Ast = {
    astForNode(node.repetitionPattern)
  }

  private def astForPatternExprSyntax(node: PatternExprSyntax): Ast = {
    astForNode(node.pattern)
  }

  private def astForPostfixIfConfigExprSyntax(node: PostfixIfConfigExprSyntax): Ast = {
    val children              = node.config.clauses.children
    val ifIfConfigClauses     = children.filter(c => code(c.poundKeyword) == "#if")
    val elseIfIfConfigClauses = children.filter(c => code(c.poundKeyword) == "#elseif")
    val elseIfConfigClauses   = children.filter(c => code(c.poundKeyword) == "#else")

    node.base match {
      case Some(base) =>
        val maybeFunctionCallExpr = ifIfConfigClauses match {
          case Nil => None
          case ifIfConfigClause :: Nil if ifConfigDeclConditionIsSatisfied(ifIfConfigClause) =>
            ifIfConfigClause.elements
          case _ :: Nil =>
            val firstElseIfSatisfied = elseIfIfConfigClauses.find(ifConfigDeclConditionIsSatisfied)
            firstElseIfSatisfied match {
              case Some(elseIfIfConfigClause) =>
                elseIfIfConfigClause.elements
              case None =>
                elseIfConfigClauses match {
                  case Nil                       => None
                  case elseIfConfigClause :: Nil => elseIfConfigClause.elements
                  case _                         => None
                }
            }
          case _ => None
        }
        maybeFunctionCallExpr match {
          case Some(functionCallExpr: FunctionCallExprSyntax) =>
            functionCallExpr.calledExpression match {
              case MemberAccessExprSyntax(json) =>
                val memberChildren = json("children").arr
                memberChildren.addOne(base.json)
                astForNode(functionCallExpr)
              case _ =>
                notHandledYet(node)
            }
          case _ => notHandledYet(node)
        }
      case None => astForNode(node.config)
    }

  }

  private def astForPostfixOperatorExprSyntax(node: PostfixOperatorExprSyntax): Ast = {
    val op = Defines.PostfixOperatorMap(code(node.operator))
    createStaticCallForOperatorAst(node, op, node.expression)
  }

  private def astForPrefixOperatorExprSyntax(node: PrefixOperatorExprSyntax): Ast = {
    val op = Defines.PrefixOperatorMap(code(node.operator))
    createStaticCallForOperatorAst(node, op, node.expression)
  }

  private def astForRegexLiteralExprSyntax(node: RegexLiteralExprSyntax): Ast = notHandledYet(node)

  private def astForSequenceExprSyntax(node: SequenceExprSyntax): Ast = {
    astForNode(node.elements)
  }

  private def astForSimpleStringLiteralExprSyntax(node: SimpleStringLiteralExprSyntax): Ast = {
    astForNode(node.segments)
  }

  private def astForStringLiteralExprSyntax(node: StringLiteralExprSyntax): Ast = {
    astForNode(node.segments)
  }

  private def astForSubscriptCallExprSyntax(node: SubscriptCallExprSyntax): Ast = {
    val baseAst   = astForNode(node.calledExpression)
    val memberAst = astForNode(node.arguments)

    val trailingClosureAsts            = node.trailingClosure.toList.map(astForNode)
    val additionalTrailingClosuresAsts = node.additionalTrailingClosures.children.map(c => astForNode(c.closure))

    val additionalArgsAsts = trailingClosureAsts ++ additionalTrailingClosuresAsts
    createIndexAccessCallAst(node, baseAst, memberAst, additionalArgsAsts)
  }

  private def astForSuperExprSyntax(node: SuperExprSyntax): Ast = {
    Ast(identifierNode(node, "super"))
  }

  protected def astsForSwitchCase(switchCase: SwitchCaseSyntax | IfConfigDeclSyntax): List[Ast] = {
    val labelAst = Ast(createJumpTarget(switchCase))
    val (testAsts, consequentAsts) = switchCase match {
      case s: SwitchCaseSyntax =>
        val (tAsts, flowAst) = s.label match {
          case i: SwitchCaseLabelSyntax =>
            val children         = i.caseItems.children
            val childrenTestAsts = children.map(c => astForNode(c.pattern))
            val childrenFlowAsts = children.collect {
              case child if child.whereClause.isDefined =>
                val whereClause = child.whereClause.get
                val ifNode =
                  controlStructureNode(whereClause.condition, ControlStructureTypes.IF, code(whereClause.condition))
                val whereAst = astForNode(whereClause)

                val op = Operators.logicalNot
                val whereClauseCallNode =
                  createStaticCallNode(
                    whereClause.condition,
                    s"!(${code(whereClause.condition)})",
                    op,
                    op,
                    Defines.Bool
                  )

                val argAsts = List(whereAst)
                val testAst = callAst(whereClauseCallNode, argAsts)
                val consequentAst =
                  Ast(controlStructureNode(whereClause.condition, ControlStructureTypes.CONTINUE, "continue"))
                setOrderExplicitly(testAst, 1)
                setOrderExplicitly(consequentAst, 2)
                Ast(ifNode)
                  .withChild(testAst)
                  .withConditionEdge(ifNode, testAst.nodes.head)
                  .withChild(consequentAst)
            }
            (childrenTestAsts, childrenFlowAsts)
          case other => (List(astForNode(other)), List.empty)
        }
        val needsSyntheticBreak = !s.statements.children.lastOption.exists(_.item.isInstanceOf[FallThroughStmtSyntax])
        val asts                = flowAst :+ astForNode(s.statements)
        val cAsts =
          if (needsSyntheticBreak) asts :+ Ast(controlStructureNode(s, ControlStructureTypes.BREAK, "break")) else asts
        (tAsts.toList, cAsts.toList)
      case i: IfConfigDeclSyntax =>
        (List.empty, List(astForIfConfigDeclSyntax(i)))
    }
    labelAst +: (testAsts ++ consequentAsts)
  }

  private def astForSwitchExprSyntax(node: SwitchExprSyntax): Ast = {
    val switchNode = controlStructureNode(node, ControlStructureTypes.SWITCH, code(node))

    // The semantics of switch statement children is partially defined by their order value.
    // The blockAst must have order == 2. Only to avoid collision we set switchExpressionAst to 1
    // because the semantics of it is already indicated via the condition edge.
    val switchExpressionAst = astForNode(node.subject)
    setOrderExplicitly(switchExpressionAst, 1)

    val blockNode_ = blockNode(node).order(2)
    scope.pushNewBlockScope(blockNode_)
    localAstParentStack.push(blockNode_)
    val casesAsts = node.cases.children.toList.flatMap(astsForSwitchCase)
    scope.popScope()
    localAstParentStack.pop()

    Ast(switchNode)
      .withChild(switchExpressionAst)
      .withConditionEdge(switchNode, switchExpressionAst.nodes.head)
      .withChild(blockAst(blockNode_, casesAsts))
  }

  private def astForTernaryExprSyntax(node: TernaryExprSyntax): Ast = {
    val op  = Operators.conditional
    val tpe = fullnameProvider.typeFullname(node).getOrElse(Defines.Any)
    registerType(tpe)
    val call = createStaticCallNode(node, code(node), op, op, tpe)

    val condAst = astForNode(node.condition)
    val posAst  = astForNode(node.thenExpression)
    val negAst  = astForNode(node.elseExpression)

    val children = List(condAst, posAst, negAst)
    callAst(call, children)
  }

  private def astForTryExprSyntax(node: TryExprSyntax): Ast = {
    // Try expression does not change the value of the expression.
    // We do not model the try semantics, so we just return the expression AST.
    // That way the data-flow is preserved.
    astForNode(node.expression)
  }

  private def astForTupleExprSyntax(node: TupleExprSyntax): Ast = {
    node.elements.children.toList match {
      case Nil         => astForListLikeExpr(node, Seq.empty)
      case head :: Nil => astForNode(head)
      case other       => astForListLikeExpr(node, other)
    }
  }

  private def astForTypeExprSyntax(node: TypeExprSyntax): Ast = {
    val nodeCode = code(node)
    registerType(nodeCode)
    Ast(identifierNode(node, nodeCode, nodeCode, Defines.Any, Seq(nodeCode)))
  }

  private def astForUnresolvedAsExprSyntax(node: UnresolvedAsExprSyntax): Ast           = notHandledYet(node)
  private def astForUnresolvedIsExprSyntax(node: UnresolvedIsExprSyntax): Ast           = notHandledYet(node)
  private def astForUnresolvedTernaryExprSyntax(node: UnresolvedTernaryExprSyntax): Ast = notHandledYet(node)

  protected def astForExprSyntax(exprSyntax: ExprSyntax): Ast = exprSyntax match {
    case node: ArrayExprSyntax                 => astForArrayExprSyntax(node)
    case node: ArrowExprSyntax                 => astForArrowExprSyntax(node)
    case node: AsExprSyntax                    => astForAsExprSyntax(node)
    case node: AssignmentExprSyntax            => astForAssignmentExprSyntax(node)
    case node: AwaitExprSyntax                 => astForAwaitExprSyntax(node)
    case node: BinaryOperatorExprSyntax        => astForBinaryOperatorExprSyntax(node)
    case node: BooleanLiteralExprSyntax        => astForBooleanLiteralExprSyntax(node)
    case node: BorrowExprSyntax                => astForBorrowExprSyntax(node)
    case node: CanImportExprSyntax             => astForCanImportExprSyntax(node)
    case node: CanImportVersionInfoSyntax      => astForCanImportVersionInfoSyntax(node)
    case node: ClosureExprSyntax               => astForClosureExprSyntax(node)
    case node: ConsumeExprSyntax               => astForConsumeExprSyntax(node)
    case node: CopyExprSyntax                  => astForCopyExprSyntax(node)
    case node: DeclReferenceExprSyntax         => astForDeclReferenceExprSyntax(node)
    case node: DictionaryExprSyntax            => astForDictionaryExprSyntax(node)
    case node: DiscardAssignmentExprSyntax     => astForDiscardAssignmentExprSyntax(node)
    case node: DoExprSyntax                    => astForDoExprSyntax(node)
    case node: EditorPlaceholderExprSyntax     => astForEditorPlaceholderExprSyntax(node)
    case node: FloatLiteralExprSyntax          => astForFloatLiteralExprSyntax(node)
    case node: ForceUnwrapExprSyntax           => astForForceUnwrapExprSyntax(node)
    case node: FunctionCallExprSyntax          => astForFunctionCallExprSyntax(node)
    case node: GenericSpecializationExprSyntax => astForGenericSpecializationExprSyntax(node)
    case node: IfExprSyntax                    => astForIfExprSyntax(node)
    case node: InOutExprSyntax                 => astForInOutExprSyntax(node)
    case node: InfixOperatorExprSyntax         => astForInfixOperatorExprSyntax(node)
    case node: IntegerLiteralExprSyntax        => astForIntegerLiteralExprSyntax(node)
    case node: IsExprSyntax                    => astForIsExprSyntax(node)
    case node: KeyPathExprSyntax               => astForKeyPathExprSyntax(node)
    case node: MacroExpansionExprSyntax        => astForMacroExpansionExprSyntax(node)
    case node: MemberAccessExprSyntax          => astForMemberAccessExprSyntax(node)
    case node: MissingExprSyntax               => astForMissingExprSyntax(node)
    case node: NilLiteralExprSyntax            => astForNilLiteralExprSyntax(node)
    case node: OptionalChainingExprSyntax      => astForOptionalChainingExprSyntax(node)
    case node: PackElementExprSyntax           => astForPackElementExprSyntax(node)
    case node: PackExpansionExprSyntax         => astForPackExpansionExprSyntax(node)
    case node: PatternExprSyntax               => astForPatternExprSyntax(node)
    case node: PostfixIfConfigExprSyntax       => astForPostfixIfConfigExprSyntax(node)
    case node: PostfixOperatorExprSyntax       => astForPostfixOperatorExprSyntax(node)
    case node: PrefixOperatorExprSyntax        => astForPrefixOperatorExprSyntax(node)
    case node: RegexLiteralExprSyntax          => astForRegexLiteralExprSyntax(node)
    case node: SequenceExprSyntax              => astForSequenceExprSyntax(node)
    case node: SimpleStringLiteralExprSyntax   => astForSimpleStringLiteralExprSyntax(node)
    case node: StringLiteralExprSyntax         => astForStringLiteralExprSyntax(node)
    case node: SubscriptCallExprSyntax         => astForSubscriptCallExprSyntax(node)
    case node: SuperExprSyntax                 => astForSuperExprSyntax(node)
    case node: SwitchExprSyntax                => astForSwitchExprSyntax(node)
    case node: TernaryExprSyntax               => astForTernaryExprSyntax(node)
    case node: TryExprSyntax                   => astForTryExprSyntax(node)
    case node: TupleExprSyntax                 => astForTupleExprSyntax(node)
    case node: TypeExprSyntax                  => astForTypeExprSyntax(node)
    case node: UnresolvedAsExprSyntax          => astForUnresolvedAsExprSyntax(node)
    case node: UnresolvedIsExprSyntax          => astForUnresolvedIsExprSyntax(node)
    case node: UnresolvedTernaryExprSyntax     => astForUnresolvedTernaryExprSyntax(node)
  }
}
