package io.joern.c2cpg.astcreation

import io.joern.x2cpg.Ast
import io.joern.x2cpg.ValidationMode
import io.joern.x2cpg.datastructures.Stack.*
import io.joern.x2cpg.utils.NodeBuilders.newModifierNode
import io.shiftleft.codepropertygraph.generated.EvaluationStrategies
import io.shiftleft.codepropertygraph.generated.ModifierTypes
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.EdgeTypes
import org.apache.commons.lang3.StringUtils
import org.eclipse.cdt.core.dom.ast.*
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTFunctionDefinition
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTLambdaExpression
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTQualifiedName
import org.eclipse.cdt.core.dom.ast.cpp.ICPPBinding
import org.eclipse.cdt.core.dom.ast.gnu.c.ICASTKnRFunctionDeclarator
import org.eclipse.cdt.internal.core.dom.parser.c.CASTFunctionDeclarator
import org.eclipse.cdt.internal.core.dom.parser.c.CASTParameterDeclaration
import org.eclipse.cdt.internal.core.dom.parser.c.CVariable
import org.eclipse.cdt.internal.core.dom.parser.cpp.CPPASTFunctionDeclarator
import org.eclipse.cdt.internal.core.dom.parser.cpp.CPPASTFunctionDefinition
import org.eclipse.cdt.internal.core.dom.parser.cpp.CPPASTParameterDeclaration
import org.eclipse.cdt.internal.core.dom.parser.cpp.CPPASTQualifiedName
import org.eclipse.cdt.internal.core.dom.parser.cpp.CPPClassType
import org.eclipse.cdt.internal.core.dom.parser.cpp.CPPEnumeration
import org.eclipse.cdt.internal.core.dom.parser.cpp.CPPStructuredBindingComposite
import org.eclipse.cdt.internal.core.dom.parser.cpp.CPPVariable
import org.eclipse.cdt.internal.core.model.ASTStringUtil

import scala.annotation.tailrec
import scala.util.Try

trait AstForFunctionsCreator(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  protected def methodDeclarationParentInfo(): (String, String) = {
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

  protected def setVariadic(parameterNodes: Seq[NewMethodParameterIn], func: IASTNode): Unit = {
    parameterNodes.lastOption.foreach {
      case p: NewMethodParameterIn if isVariadic(func) =>
        p.isVariadic = true
        p.code = s"${p.code}..."
      case _ =>
    }
  }

  private def setVariadicParameterInfo(parameterNodeInfos: Seq[CGlobal.ParameterInfo], func: IASTNode): Unit = {
    parameterNodeInfos.lastOption.foreach {
      case p: CGlobal.ParameterInfo if isVariadic(func) =>
        p.isVariadic = true
        p.code = s"${p.code}..."
      case _ =>
    }
  }

  protected def astForFunctionDeclarator(funcDecl: IASTFunctionDeclarator): Ast = {
    safeGetBinding(funcDecl.getName) match {
      case Some(_: IFunction) =>
        val MethodFullNameInfo(name, fullName, signature, returnType) = methodFullNameInfo(funcDecl)
        val codeString                                                = code(funcDecl.getParent)
        val filename                                                  = fileName(funcDecl)

        val parameterNodeInfos = thisForCPPFunctions(funcDecl) ++ withIndex(parameters(funcDecl)) { (p, i) =>
          parameterNodeInfo(p, i)
        }
        setVariadicParameterInfo(parameterNodeInfos, funcDecl)

        val (astParentType, astParentFullName) = methodDeclarationParentInfo()
        val methodInfo = CGlobal.MethodInfo(
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
      case Some(cVariable: CVariable) =>
        val name       = shortName(funcDecl)
        val tpe        = cleanType(safeGetType(cVariable.getType))
        val codeString = code(funcDecl.getParent)
        val node       = localNode(funcDecl, name, codeString, registerType(tpe))
        scope.addVariable(name, node, tpe, C2CpgScope.ScopeType.BlockScope)
        Ast(node)
      case Some(cppVariable: CPPVariable) =>
        val name       = shortName(funcDecl)
        val tpe        = cleanType(safeGetType(cppVariable.getType))
        val codeString = code(funcDecl.getParent)
        val node       = localNode(funcDecl, name, codeString, registerType(tpe))
        scope.addVariable(name, node, tpe, C2CpgScope.ScopeType.BlockScope)
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

  private def modifierFromString(image: String): List[NewModifier] = {
    image match {
      case "static" => List(newModifierNode(ModifierTypes.STATIC))
      case _        => Nil
    }
  }

  private def modifierFor(funcDef: IASTFunctionDefinition): List[NewModifier] = {
    val constructorModifier = if (isCppConstructor(funcDef)) {
      List(newModifierNode(ModifierTypes.CONSTRUCTOR), newModifierNode(ModifierTypes.PUBLIC))
    } else Nil
    val visibilityModifier = Try(modifierFromString(funcDef.getSyntax.getImage)).getOrElse(Nil)
    constructorModifier ++ visibilityModifier
  }

  private def modifierFor(funcDecl: IASTFunctionDeclarator): List[NewModifier] = {
    Try(modifierFromString(funcDecl.getParent.getSyntax.getImage)).getOrElse(Nil)
  }

  protected def isCppConstructor(funcDef: IASTFunctionDefinition): Boolean = {
    funcDef match {
      case cppFunc: CPPASTFunctionDefinition => cppFunc.getMemberInitializers.nonEmpty
      case _                                 => false
    }
  }

  private def thisForCPPFunctions(func: IASTNode): Seq[CGlobal.ParameterInfo] = {
    func match {
      case cppFunc: ICPPASTFunctionDefinition if !modifierFor(cppFunc).exists(_.modifierType == ModifierTypes.STATIC) =>
        val maybeOwner = safeGetBinding(cppFunc.getDeclarator.getName) match {
          case Some(o: ICPPBinding) if o.getOwner.isInstanceOf[CPPClassType] =>
            Some(o.getOwner.asInstanceOf[CPPClassType].getQualifiedName.mkString("."))
          case Some(o: ICPPBinding) if o.getOwner.isInstanceOf[CPPEnumeration] =>
            Some(o.getOwner.asInstanceOf[CPPEnumeration].getQualifiedName.mkString("."))
          case Some(o: ICPPBinding) if o.getOwner.isInstanceOf[CPPStructuredBindingComposite] =>
            Some(o.getOwner.asInstanceOf[CPPStructuredBindingComposite].getQualifiedName.mkString("."))
          case _ if cppFunc.getDeclarator.getName.isInstanceOf[ICPPASTQualifiedName] =>
            Some(cppFunc.getDeclarator.getName.asInstanceOf[CPPASTQualifiedName].getQualifier.mkString("."))
          case _ => None
        }
        maybeOwner.toSeq.map { owner =>
          new CGlobal.ParameterInfo(
            "this",
            "this",
            0,
            false,
            EvaluationStrategies.BY_VALUE,
            line(cppFunc),
            column(cppFunc),
            registerType(s"$owner*")
          )
        }
      case _ => Seq.empty
    }
  }

  protected def astForFunctionDefinition(funcDef: IASTFunctionDefinition): Ast = {
    val filename                                                  = fileName(funcDef)
    val MethodFullNameInfo(name, fullName, signature, returnType) = methodFullNameInfo(funcDef)
    registerMethodDefinition(fullName)

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

    val implicitThisParam = thisForCPPFunctions(funcDef).map { thisParam =>
      val parameterNode = parameterInNode(
        funcDef,
        thisParam.name,
        thisParam.code,
        thisParam.index,
        thisParam.isVariadic,
        thisParam.evaluationStrategy,
        thisParam.typeFullName
      )
      scope.addVariable(thisParam.name, parameterNode, thisParam.typeFullName, C2CpgScope.ScopeType.MethodScope)
      parameterNode
    }
    val parameterNodes = implicitThisParam ++ withIndex(parameters(funcDef)) { (p, i) =>
      parameterNode(p, i)
    }
    setVariadic(parameterNodes, funcDef)

    val astForMethod = methodAst(
      methodNode_,
      parameterNodes.map(Ast(_)),
      astForMethodBody(Option(funcDef.getBody), methodBlockNode),
      methodReturnNode(funcDef, registerType(returnType)),
      modifiers = modifierFor(funcDef)
    )

    scope.popScope()
    methodAstParentStack.pop()

    methodRefNode_ match {
      case Some(ref) =>
        createFunctionTypeAndTypeDecl(funcDef, methodNode_)
        Ast.storeInDiffGraph(astForMethod, diffGraph)
        diffGraph.addEdge(methodAstParentStack.head, methodNode_, EdgeTypes.AST)
        Ast(ref)
      case None =>
        val typeDeclAst = createFunctionTypeAndTypeDecl(methodNode_)
        astForMethod.merge(typeDeclAst)
    }
  }

  private def parameterNodeInfo(parameter: IASTNode, paramIndex: Int): CGlobal.ParameterInfo = {
    val (name, codeString, tpe, variadic) = parameter match {
      case p: CASTParameterDeclaration =>
        (shortName(p.getDeclarator), code(p), cleanType(typeForDeclSpecifier(p.getDeclSpecifier)), false)
      case p: CPPASTParameterDeclaration =>
        (
          shortName(p.getDeclarator),
          code(p),
          cleanType(typeForDeclSpecifier(p.getDeclSpecifier)),
          p.getDeclarator.declaresParameterPack()
        )
      case s: IASTSimpleDeclaration =>
        (
          s.getDeclarators.headOption
            .map(n => ASTStringUtil.getSimpleName(n.getName))
            .getOrElse(uniqueName("", "", "param")._1),
          code(s),
          cleanType(typeForDeclSpecifier(s)),
          false
        )
      case other =>
        (code(other), code(other), cleanType(typeForDeclSpecifier(other)), false)
    }
    new CGlobal.ParameterInfo(
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

  protected def parameterNode(parameter: IASTNode, paramIndex: Int): NewMethodParameterIn = {
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
    scope.addVariable(parameterInfo.name, parameterNode, parameterInfo.typeFullName, C2CpgScope.ScopeType.MethodScope)
    parameterNode
  }

  protected def astForMethodBody(body: Option[IASTStatement], blockNode: NewBlock): Ast = body match {
    case Some(b: IASTCompoundStatement) =>
      astForBlockStatement(b, blockNode)
    case Some(b) =>
      scope.pushNewBlockScope(blockNode)
      val childAst = astForNode(b)
      scope.popScope()
      blockAst(blockNode).withChild(childAst)
    case None =>
      blockAst(blockNode)
  }

}
