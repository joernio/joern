package io.joern.c2cpg.astcreation

import io.joern.c2cpg.passes.FunctionDeclNodePass
import io.joern.x2cpg.Ast
import io.joern.x2cpg.datastructures.Stack.*
import io.joern.x2cpg.datastructures.VariableScopeManager
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.*
import org.eclipse.cdt.core.dom.ast.*
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTLambdaExpression.CaptureDefault
import org.eclipse.cdt.core.dom.ast.cpp.*
import org.eclipse.cdt.core.dom.ast.gnu.c.ICASTKnRFunctionDeclarator
import org.eclipse.cdt.internal.core.dom.parser.c.{CASTFunctionDeclarator, CASTParameterDeclaration, CVariable}
import org.eclipse.cdt.internal.core.dom.parser.cpp.*
import org.eclipse.cdt.internal.core.model.ASTStringUtil

import scala.annotation.tailrec
import scala.util.Try

trait AstForFunctionsCreator { this: AstCreator =>

  import FullNameProvider.*

  final protected def parameters(functionNode: IASTNode): Seq[IASTNode] = functionNode match {
    case arr: IASTArrayDeclarator       => parameters(arr.getNestedDeclarator)
    case decl: CPPASTFunctionDeclarator => decl.getParameters.toIndexedSeq ++ parameters(decl.getNestedDeclarator)
    case decl: CASTFunctionDeclarator   => decl.getParameters.toIndexedSeq ++ parameters(decl.getNestedDeclarator)
    case defn: IASTFunctionDefinition   => parameters(defn.getDeclarator)
    case lambdaExpression: ICPPASTLambdaExpression => parameters(lambdaExpression.getDeclarator)
    case knr: ICASTKnRFunctionDeclarator           => knr.getParameterDeclarations.toIndexedSeq
    case _: IASTDeclarator                         => Seq.empty
    case other if other != null                    => notHandledYet(other); Seq.empty
    case null                                      => Seq.empty
  }

  @tailrec
  final protected def isVariadic(functionNode: IASTNode): Boolean = functionNode match {
    case decl: CPPASTFunctionDeclarator             => decl.takesVarArgs()
    case decl: CASTFunctionDeclarator               => decl.takesVarArgs()
    case functionDefinition: IASTFunctionDefinition => isVariadic(functionDefinition.getDeclarator)
    case lambdaExpression: ICPPASTLambdaExpression  => isVariadic(lambdaExpression.getDeclarator)
    case _                                          => false
  }

  protected def astForFunctionDeclarator(funcDecl: IASTFunctionDeclarator): Ast = {
    safeGetBinding(funcDecl.getName) match {
      case Some(_: IFunction) =>
        val MethodFullNameInfo(name, fullName, signature, returnType) = methodFullNameInfo(funcDecl)
        val codeString                                                = code(funcDecl.getParent)
        val filename                                                  = fileName(funcDecl)

        val parameterNodeInfos = thisForCPPFunctions(funcDecl, false) ++ withIndex(parameters(funcDecl)) { (p, i) =>
          parameterNodeInfo(p, i)
        }
        setVariadicParameterInfo(parameterNodeInfos, funcDecl)

        val (astParentType, astParentFullName) = methodDeclarationParentInfo()
        val methodInfo = FunctionDeclNodePass.MethodInfo(
          name,
          code = codeString,
          fileName = filename,
          returnType = registerType(returnType),
          astParentType = astParentType,
          astParentFullName = astParentFullName,
          lineNumber = line(funcDecl),
          columnNumber = column(funcDecl),
          lineNumberEnd = lineEnd(funcDecl),
          columnNumberEnd = columnEnd(funcDecl),
          signature = signature,
          offset(funcDecl),
          parameter = parameterNodeInfos,
          modifier = modifierFor(funcDecl).map(_.modifierType)
        )
        registerMethodDeclaration(fullName, methodInfo)
        Ast()
      case Some(variable: (CVariable | CPPVariable)) =>
        val name       = shortName(funcDecl)
        val tpe        = safeGetType(variable.getType)
        val codeString = code(funcDecl.getParent)
        val node       = localNode(funcDecl, name, codeString, registerType(tpe))
        scope.addVariable(name, node, tpe, VariableScopeManager.ScopeType.BlockScope)
        Ast(node)
      case Some(field: IField) =>
        // TODO create a member for the field
        // We get here a least for function pointer member declarations in classes like:
        // class A {
        //   public:
        //     void (*foo)(int);
        // };
        Ast()
      case Some(typeDef: ITypedef) =>
        // TODO handle typeDecl for now we just ignore this.
        Ast()
      case _ =>
        notHandledYet(funcDecl)
    }

  }

  protected def astForFunctionDefinition(funcDef: IASTFunctionDefinition): Ast = {
    val filename                                                  = fileName(funcDef)
    val MethodFullNameInfo(name, fullName, signature, returnType) = methodFullNameInfo(funcDef)
    registerMethodDefinition(fullName)
    val isConstructor                 = bindsToConstructor(funcDef) && fullName.contains(s".$name:")
    val shouldCreateFunctionReference = typeRefIdStack.headOption.isEmpty
    val methodRefNode_ = if (!shouldCreateFunctionReference) { None }
    else { Option(methodRefNode(funcDef, name, fullName, fullName)) }

    val codeString      = code(funcDef)
    val methodBlockNode = blockNode(funcDef)
    val methodNode_     = methodNode(funcDef, name, codeString, fullName, Some(signature), filename)
    val capturingRefNode = if (shouldCreateFunctionReference) { methodRefNode_ }
    else { typeRefIdStack.headOption }

    methodAstParentStack.push(methodNode_)
    scope.pushNewMethodScope(fullName, name, methodBlockNode, capturingRefNode)

    val implicitThisParam = thisForCPPFunctions(funcDef, isConstructor).map { thisParam =>
      val parameterNode = parameterInNode(
        funcDef,
        thisParam.name,
        thisParam.code,
        thisParam.index,
        thisParam.isVariadic,
        thisParam.evaluationStrategy,
        thisParam.typeFullName
      )
      scope.addVariable(
        thisParam.name,
        parameterNode,
        thisParam.typeFullName,
        VariableScopeManager.ScopeType.MethodScope
      )
      parameterNode
    }
    val parameterNodes = implicitThisParam ++ withIndex(parameters(funcDef)) { (p, i) =>
      parameterNode(p, i)
    }
    setVariadic(parameterNodes, funcDef)

    memberInitializations(funcDef).foreach { callAst =>
      Ast.storeInDiffGraph(callAst, diffGraph)
      callAst.root.foreach(r => diffGraph.addEdge(methodBlockNode, r, EdgeTypes.AST))
    }
    val methodBodyAst = astForMethodBody(Option(funcDef.getBody), methodBlockNode)

    if (isConstructor) {
      implicitThisParam.foreach { param =>
        val thisIdentifier = identifierNode(funcDef, Defines.This, Defines.This, param.typeFullName)
        scope.addVariableReference(Defines.This, thisIdentifier, param.typeFullName, EvaluationStrategies.BY_SHARING)
        val cpgReturn = returnNode(funcDef, "return this")
        val returnAst = Ast(cpgReturn).withChild(Ast(thisIdentifier)).withArgEdge(cpgReturn, thisIdentifier)
        Ast.storeInDiffGraph(returnAst, diffGraph)
        diffGraph.addEdge(methodBlockNode, cpgReturn, EdgeTypes.AST)
      }
    }

    scope.popScope()
    methodAstParentStack.pop()

    val astForMethod = methodAst(
      methodNode_,
      parameterNodes.map(Ast(_)),
      methodBodyAst,
      methodReturnNode(funcDef, registerType(returnType)),
      modifiers = modifierFor(funcDef)
    )

    methodRefNode_ match {
      case Some(ref) =>
        createFunctionTypeAndTypeDecl(funcDef, methodNode_)
        Ast.storeInDiffGraph(astForMethod, diffGraph)
        if (isConstructor) {
          methodNode_.astParentType = TypeDecl.Label
          methodNode_.astParentFullName = fullName.substring(0, fullName.lastIndexOf(s".$name:"))
        } else {
          diffGraph.addEdge(methodAstParentStack.head, methodNode_, EdgeTypes.AST)
        }
        Ast(ref)
      case None =>
        val typeDeclAst = createFunctionTypeAndTypeDecl(methodNode_)
        astForMethod.merge(typeDeclAst)
    }
  }

  protected def astForLambdaExpression(lambdaExpression: ICPPASTLambdaExpression): Ast = {
    val (lambdaMethodNode, lambdaMethodRef) = createAndPushLambdaMethod(lambdaExpression)
    createAndPushLambdaTypeDecl(lambdaExpression, lambdaMethodNode)
    Ast(lambdaMethodRef)
  }

  private def methodDeclarationParentInfo(): (String, String) = {
    methodAstParentStack.collectFirst { case t: NewTypeDecl => (t.label, t.fullName) }.getOrElse {
      (methodAstParentStack.head.label, methodAstParentStack.head.properties("FULL_NAME").toString)
    }
  }

  private def createFunctionTypeAndTypeDecl(method: NewMethod): Ast = {
    val parentNode: NewTypeDecl = methodAstParentStack.collectFirst { case t: NewTypeDecl => t }.get
    method.astParentFullName = parentNode.fullName
    method.astParentType = parentNode.label
    val functionBinding = NewBinding().name(method.name).methodFullName(method.fullName).signature(method.signature)
    Ast(functionBinding).withBindsEdge(parentNode, functionBinding).withRefEdge(functionBinding, method)
  }

  private def createFunctionTypeAndTypeDecl(funcDef: IASTFunctionDefinition, methodNode: NewMethod): Unit = {
    registerType(methodNode.fullName)
    val (astParentType, astParentFullName) = methodDeclarationParentInfo()
    val methodTypeDeclNode = typeDeclNode(
      funcDef,
      methodNode.name,
      methodNode.fullName,
      methodNode.filename,
      methodNode.fullName,
      astParentType,
      astParentFullName
    )

    methodNode.astParentFullName = astParentFullName
    methodNode.astParentType = astParentType

    val functionBinding = NewBinding()
      .name(methodNode.name)
      .methodFullName(methodNode.fullName)
      .signature(methodNode.signature)

    val functionBindAst = Ast(functionBinding)
      .withBindsEdge(methodTypeDeclNode, functionBinding)
      .withRefEdge(functionBinding, methodNode)

    Ast.storeInDiffGraph(Ast(methodTypeDeclNode), diffGraph)
    Ast.storeInDiffGraph(functionBindAst, diffGraph)
  }

  private def setVariadic(parameterNodes: Seq[NewMethodParameterIn], func: IASTNode): Unit = {
    parameterNodes.lastOption.foreach {
      case p: NewMethodParameterIn if isVariadic(func) =>
        p.isVariadic = true
        p.code = s"${p.code}..."
      case _ =>
    }
  }

  private def setVariadicParameterInfo(
    parameterNodeInfos: Seq[FunctionDeclNodePass.ParameterInfo],
    func: IASTNode
  ): Unit = {
    parameterNodeInfos.lastOption.foreach {
      case p: FunctionDeclNodePass.ParameterInfo if isVariadic(func) =>
        p.isVariadic = true
        p.code = s"${p.code}..."
      case _ =>
    }
  }

  private def modifierFromString(node: IASTNode, image: String): List[NewModifier] = {
    image match {
      case "static" => List(modifierNode(node, ModifierTypes.STATIC))
      case _        => Nil
    }
  }

  private def modifierFor(funcDef: IASTFunctionDefinition): List[NewModifier] = {
    val constructorModifier = if (bindsToConstructor(funcDef)) {
      List(modifierNode(funcDef, ModifierTypes.CONSTRUCTOR), modifierNode(funcDef, ModifierTypes.PUBLIC))
    } else Nil
    val visibilityModifier = Try(modifierFromString(funcDef, funcDef.getSyntax.getImage)).getOrElse(Nil)
    constructorModifier ++ visibilityModifier
  }

  private def modifierFor(funcDecl: IASTFunctionDeclarator): List[NewModifier] = {
    Try(modifierFromString(funcDecl, funcDecl.getParent.getSyntax.getImage)).getOrElse(Nil)
  }

  private def syntheticThisAccess(ident: IASTName, identifierName: String): Ast = {
    val tpe = ident.getBinding match {
      case f: CPPField => safeGetType(f.getType)
      case _           => typeFor(ident)
    }
    scope.lookupVariable(Defines.This) match {
      case Some((_, tpe)) =>
        val op             = Operators.fieldAccess
        val code           = s"${Defines.This}.$identifierName"
        val thisIdentifier = identifierNode(ident, Defines.This, Defines.This, tpe)
        scope.addVariableReference(Defines.This, thisIdentifier, tpe, EvaluationStrategies.BY_SHARING)
        val member = fieldIdentifierNode(ident, identifierName, identifierName)
        val ma     = callNode(ident, code, op, op, DispatchTypes.STATIC_DISPATCH, None, Some(tpe))
        callAst(ma, Seq(Ast(thisIdentifier), Ast(member)))
      case None =>
        val idNode = identifierNode(ident, identifierName, identifierName, tpe)
        scope.addVariableReference(identifierName, idNode, tpe, EvaluationStrategies.BY_REFERENCE)
        Ast(idNode)
    }
  }

  private def astForICPPASTConstructorChainInitializer(init: ICPPASTConstructorChainInitializer): Ast = {
    init.getInitializer match {
      case l: IASTInitializerList if l.getClauses == null || l.getClauses.isEmpty || l.getClauses.forall(_ == null) =>
        Ast()
      case c: ICPPASTConstructorInitializer if init.getMemberInitializerId.isInstanceOf[ICPPASTQualifiedName] =>
        val constructorCallName = shortName(init.getMemberInitializerId)
        val typeFullName        = fullName(init.getMemberInitializerId)
        val signature           = s"${Defines.Void}(${initializerSignature(c)})"
        val fullNameWithSig     = s"$typeFullName.$constructorCallName:$signature"
        val constructorCallNode = callNode(
          c,
          code(init),
          typeFullName,
          fullNameWithSig,
          DispatchTypes.STATIC_DISPATCH,
          Some(signature),
          Some(registerType(Defines.Void))
        )
        val args = astsForConstructorInitializer(c)
        callAst(constructorCallNode, args)
      case _ =>
        val leftAst = syntheticThisAccess(init.getMemberInitializerId, nameForIdentifier(init.getMemberInitializerId))
        val rightAst = init.getInitializer match {
          case l: IASTInitializerList           => astForNode(l.getClauses.head)
          case c: ICPPASTConstructorInitializer => c.getArguments.headOption.map(astForNode).getOrElse(Ast())
          case _ =>
            val name   = nameForIdentifier(init.getMemberInitializerId)
            val tpe    = registerType(typeFor(init.getMemberInitializerId))
            val idNode = identifierNode(init.getMemberInitializerId, name, name, tpe)
            scope.addVariableReference(name, idNode, tpe, EvaluationStrategies.BY_REFERENCE)
            Ast(idNode)
        }
        val op = Operators.assignment
        val leftCode =
          leftAst.root.collect { case expr: ExpressionNew => expr.code }.getOrElse(code(init.getMemberInitializerId))
        val rightCode =
          rightAst.root.collect { case expr: ExpressionNew => expr.code }.getOrElse(code(init.getInitializer))

        val codeString = s"$leftCode = $rightCode"
        val assignmentCall =
          callNode(init, codeString, op, op, DispatchTypes.STATIC_DISPATCH, None, Some(registerType(Defines.Void)))
        callAst(assignmentCall, List(leftAst, rightAst))
    }
  }

  private def memberInitializations(func: IASTNode): Seq[Ast] = {
    func match {
      case f: ICPPASTFunctionDefinition =>
        f.getMemberInitializers.toIndexedSeq.map(astForICPPASTConstructorChainInitializer)
      case _ =>
        Seq.empty
    }
  }

  private def thisForCPPFunctions(func: IASTNode, isConstructor: Boolean): Seq[FunctionDeclNodePass.ParameterInfo] = {
    func match {
      case cppFunc: ICPPASTFunctionDefinition if !modifierFor(cppFunc).exists(_.modifierType == ModifierTypes.STATIC) =>
        val maybeOwner = safeGetBinding(cppFunc.getDeclarator.getName) match {
          case Some(o: ICPPBinding) if o.getOwner.isInstanceOf[CPPClassType] =>
            Some(o.getOwner.asInstanceOf[CPPClassType].getQualifiedName.mkString("."))
          case Some(o: ICPPBinding) if o.getOwner.isInstanceOf[CPPEnumeration] =>
            Some(o.getOwner.asInstanceOf[CPPEnumeration].getQualifiedName.mkString("."))
          case Some(o: ICPPBinding) if o.getOwner.isInstanceOf[CPPStructuredBindingComposite] =>
            Some(o.getOwner.asInstanceOf[CPPStructuredBindingComposite].getQualifiedName.mkString("."))
          case _ if cppFunc.getDeclarator.getName.isInstanceOf[CPPASTQualifiedName] =>
            Some(cppFunc.getDeclarator.getName.asInstanceOf[CPPASTQualifiedName].getQualifier.mkString("."))
          case _ if cppFunc.getParent.isInstanceOf[ICPPASTCompositeTypeSpecifier] =>
            Some(fullName(cppFunc.getParent))
          case _ => None
        }
        maybeOwner.toSeq.map { owner =>
          val tpe = if (isConstructor) { cleanType(owner) }
          else { s"${cleanType(owner)}*" }
          new FunctionDeclNodePass.ParameterInfo(
            Defines.This,
            Defines.This,
            0,
            false,
            EvaluationStrategies.BY_SHARING,
            line(cppFunc),
            column(cppFunc),
            registerType(tpe)
          )
        }
      case _ => Seq.empty
    }
  }

  private def parameterNodeInfo(parameter: IASTNode, paramIndex: Int): FunctionDeclNodePass.ParameterInfo = {
    val (name, codeString, tpe, variadic) = parameter match {
      case p: CASTParameterDeclaration =>
        (shortName(p.getDeclarator), code(p), typeForDeclSpecifier(p.getDeclSpecifier), false)
      case p: CPPASTParameterDeclaration =>
        (
          shortName(p.getDeclarator),
          code(p),
          typeForDeclSpecifier(p.getDeclSpecifier),
          p.getDeclarator.declaresParameterPack()
        )
      case s: IASTSimpleDeclaration =>
        (
          s.getDeclarators.headOption
            .map(n => ASTStringUtil.getSimpleName(n.getName))
            .getOrElse(scopeLocalUniqueName("param")),
          code(s),
          typeForDeclSpecifier(s),
          false
        )
      case other =>
        (code(other), code(other), typeForDeclSpecifier(other), false)
    }
    new FunctionDeclNodePass.ParameterInfo(
      name,
      codeString,
      paramIndex,
      variadic,
      EvaluationStrategies.BY_VALUE,
      lineNumber = line(parameter),
      columnNumber = column(parameter),
      typeFullName = registerType(tpe)
    )
  }

  private def parameterNode(parameter: IASTNode, paramIndex: Int): NewMethodParameterIn = {
    val parameterInfo = parameterNodeInfo(parameter, paramIndex)
    val parameterNode =
      parameterInNode(
        parameter,
        parameterInfo.name,
        parameterInfo.code,
        parameterInfo.index,
        parameterInfo.isVariadic,
        parameterInfo.evaluationStrategy,
        parameterInfo.typeFullName
      )
    scope.addVariable(
      parameterInfo.name,
      parameterNode,
      parameterInfo.typeFullName,
      VariableScopeManager.ScopeType.MethodScope
    )
    parameterNode
  }

  private def astForMethodBody(body: Option[IASTStatement], blockNode: NewBlock): Ast = body match {
    case Some(b: IASTCompoundStatement) =>
      methodAstParentStack.push(blockNode)
      val ast = astForBlockStatement(b, blockNode)
      methodAstParentStack.pop()
      ast
    case Some(b) =>
      scope.pushNewBlockScope(blockNode)
      methodAstParentStack.push(blockNode)
      val childAst = astForNode(b)
      methodAstParentStack.pop()
      scope.popScope()
      blockAst(blockNode).withChild(childAst)
    case None =>
      blockAst(blockNode)
  }

  private def setEvaluationStrategyForCaptures(lambdaExpression: ICPPASTLambdaExpression, bodyAst: Ast): Unit = {
    val captureDefault = lambdaExpression.getCaptureDefault
    val strategyMapping = captureDefault match {
      case CaptureDefault.BY_REFERENCE => EvaluationStrategies.BY_REFERENCE
      case _                           => EvaluationStrategies.BY_VALUE
    }
    lambdaExpression.getCaptures match {
      case captures if captures.isEmpty && captureDefault == CaptureDefault.UNSPECIFIED => // do nothing
      case captures if captures.isEmpty =>
        bodyAst.nodes.foreach {
          case i: NewIdentifier if !scope.variableIsInMethodScope(i.name) =>
            scope.updateVariableReference(i, strategyMapping)
          case _ => // do nothing
        }
      case other =>
        val validCaptures = other.filter(_.getIdentifier != null)
        bodyAst.nodes.foreach {
          case i: NewIdentifier if !scope.variableIsInMethodScope(i.name) =>
            val maybeInCaptures = validCaptures.find(c => code(c.getIdentifier) == i.name)
            val strategy = maybeInCaptures match {
              case Some(c) if c.isByReference => EvaluationStrategies.BY_REFERENCE
              case _                          => strategyMapping
            }
            scope.updateVariableReference(i, strategy)
          case _ => // do nothing
        }
    }
  }

  private def createAndPushLambdaMethod(lambdaExpression: ICPPASTLambdaExpression): (NewMethod, NewMethodRef) = {
    val MethodFullNameInfo(name, fullName, signature, returnType) = methodFullNameInfo(lambdaExpression)
    val filename                                                  = fileName(lambdaExpression)
    val codeString                                                = code(lambdaExpression)

    val methodRef        = methodRefNode(lambdaExpression, fullName, fullName, fullName)
    val lambdaMethodNode = methodNode(lambdaExpression, name, codeString, fullName, Some(signature), filename)

    val lambdaMethodBlockNode = blockNode(lambdaExpression)
    methodAstParentStack.push(lambdaMethodNode)
    scope.pushNewMethodScope(fullName, name, lambdaMethodBlockNode, Some(methodRef))

    val parameterNodes = withIndex(parameters(lambdaExpression.getDeclarator)) { (p, i) => parameterNode(p, i) }
    setVariadic(parameterNodes, lambdaExpression)
    val parameterAsts = parameterNodes.map(Ast(_))
    val lambdaBodyAst = astForMethodBody(Option(lambdaExpression.getBody), lambdaMethodBlockNode)
    setEvaluationStrategyForCaptures(lambdaExpression, lambdaBodyAst)

    scope.popScope()
    methodAstParentStack.pop()

    val isStatic        = !lambdaExpression.getCaptures.exists(c => c.capturesThisPointer())
    val returnNode      = methodReturnNode(lambdaExpression, registerType(returnType))
    val virtualModifier = Some(modifierNode(lambdaExpression, ModifierTypes.VIRTUAL))
    val staticModifier  = Option.when(isStatic)(modifierNode(lambdaExpression, ModifierTypes.STATIC))
    val privateModifier = Some(modifierNode(lambdaExpression, ModifierTypes.PRIVATE))
    val lambdaModifier  = Some(modifierNode(lambdaExpression, ModifierTypes.LAMBDA))
    val modifiers       = List(virtualModifier, staticModifier, privateModifier, lambdaModifier).flatten.map(Ast(_))

    val lambdaMethodAst = Ast(lambdaMethodNode)
      .withChildren(parameterAsts)
      .withChild(lambdaBodyAst)
      .withChild(Ast(returnNode))
      .withChildren(modifiers)

    val parentNode = methodAstParentStack.collectFirst { case t: NewTypeDecl => t }
    Ast.storeInDiffGraph(lambdaMethodAst, diffGraph)
    parentNode.foreach { typeDeclNode =>
      diffGraph.addEdge(typeDeclNode, lambdaMethodNode, EdgeTypes.AST)
    }
    (lambdaMethodNode, methodRef)
  }

  private def createAndPushLambdaTypeDecl(
    lambdaExpression: ICPPASTLambdaExpression,
    lambdaMethodNode: NewMethod
  ): Unit = {
    registerType(lambdaMethodNode.fullName)
    val (astParentType, astParentFullName) = methodDeclarationParentInfo()
    val lambdaTypeDeclNode = typeDeclNode(
      lambdaExpression,
      lambdaMethodNode.name,
      lambdaMethodNode.fullName,
      lambdaMethodNode.filename,
      lambdaMethodNode.fullName,
      astParentType,
      astParentFullName,
      Seq(registerType(Defines.Function))
    )

    val functionBinding = NewBinding()
      .name(Defines.OperatorCall)
      .methodFullName(lambdaMethodNode.fullName)
      .signature(lambdaMethodNode.signature)

    val functionBindAst = Ast(functionBinding)
      .withBindsEdge(lambdaTypeDeclNode, functionBinding)
      .withRefEdge(functionBinding, lambdaMethodNode)

    Ast.storeInDiffGraph(Ast(lambdaTypeDeclNode), diffGraph)
    Ast.storeInDiffGraph(functionBindAst, diffGraph)
  }

}
