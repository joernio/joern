package io.joern.c2cpg.astcreation

import io.shiftleft.codepropertygraph.generated.EvaluationStrategies
import io.shiftleft.codepropertygraph.generated.nodes._
import io.joern.x2cpg.Ast
import org.eclipse.cdt.core.dom.ast._
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTLambdaExpression
import org.eclipse.cdt.core.dom.ast.gnu.c.ICASTKnRFunctionDeclarator
import org.eclipse.cdt.internal.core.dom.parser.c.{CASTFunctionDeclarator, CASTParameterDeclaration}
import org.eclipse.cdt.internal.core.dom.parser.cpp.{CPPASTFunctionDeclarator, CPPASTParameterDeclaration}
import org.eclipse.cdt.internal.core.model.ASTStringUtil
import io.joern.x2cpg.datastructures.Stack._
import org.apache.commons.lang.StringUtils

import scala.annotation.tailrec

trait AstForFunctionsCreator { this: AstCreator =>

  private def createFunctionTypeAndTypeDecl(
    node: IASTNode,
    method: NewMethod,
    methodName: String,
    methodFullName: String,
    signature: String
  ): Ast = {
    val normalizedName     = StringUtils.normalizeSpace(methodName)
    val normalizedFullName = StringUtils.normalizeSpace(methodFullName)

    val parentNode: NewTypeDecl = methodAstParentStack.collectFirst { case t: NewTypeDecl => t }.getOrElse {
      val astParentType     = methodAstParentStack.head.label
      val astParentFullName = methodAstParentStack.head.properties("FULL_NAME").toString
      val typeDeclNode = newTypeDeclNode(
        node,
        normalizedName,
        normalizedFullName,
        method.filename,
        normalizedName,
        astParentType,
        astParentFullName
      )
      Ast.storeInDiffGraph(Ast(typeDeclNode), diffGraph)
      typeDeclNode
    }

    method.astParentFullName = parentNode.fullName
    method.astParentType = parentNode.label
    val functionBinding = NewBinding().name(normalizedName).methodFullName(normalizedFullName).signature(signature)
    Ast(functionBinding).withBindsEdge(parentNode, functionBinding).withRefEdge(functionBinding, method)
  }

  @tailrec
  private def parameters(funct: IASTNode): Seq[IASTNode] = funct match {
    case decl: CPPASTFunctionDeclarator            => decl.getParameters.toIndexedSeq
    case decl: CASTFunctionDeclarator              => decl.getParameters.toIndexedSeq
    case defn: IASTFunctionDefinition              => parameters(defn.getDeclarator)
    case lambdaExpression: ICPPASTLambdaExpression => parameters(lambdaExpression.getDeclarator)
    case knr: ICASTKnRFunctionDeclarator           => knr.getParameterDeclarations.toIndexedSeq
    case other if other != null                    => notHandledYet(other); Seq.empty
    case null                                      => Seq.empty
  }

  @tailrec
  private def isVariadic(funct: IASTNode): Boolean = funct match {
    case decl: CPPASTFunctionDeclarator            => decl.takesVarArgs()
    case decl: CASTFunctionDeclarator              => decl.takesVarArgs()
    case defn: IASTFunctionDefinition              => isVariadic(defn.getDeclarator)
    case lambdaExpression: ICPPASTLambdaExpression => isVariadic(lambdaExpression.getDeclarator)
    case _                                         => false
  }

  private def parameterListSignature(func: IASTNode, includeParamNames: Boolean): String = {
    val variadic = if (isVariadic(func)) "..." else ""
    val elements =
      if (!includeParamNames) {
        parameters(func).map {
          case p: IASTParameterDeclaration => typeForDeclSpecifier(p.getDeclSpecifier)
          case other                       => typeForDeclSpecifier(other)
        }
      } else {
        parameters(func).map(p => nodeSignature(p))
      }
    s"(${elements.mkString(",")}$variadic)"
  }

  private def setVariadic(parameterNodes: Seq[NewMethodParameterIn], func: IASTNode): Unit = {
    parameterNodes.lastOption.foreach {
      case p: NewMethodParameterIn if isVariadic(func) =>
        p.isVariadic = true
        p.code = s"${p.code}..."
      case _ =>
    }
  }

  protected def astForMethodRefForLambda(lambdaExpression: ICPPASTLambdaExpression): Ast = {
    val filename = fileName(lambdaExpression)

    val returnType = lambdaExpression.getDeclarator match {
      case declarator: IASTDeclarator =>
        declarator.getTrailingReturnType match {
          case id: IASTTypeId => typeForDeclSpecifier(id.getDeclSpecifier)
          case null           => Defines.anyTypeName
        }
      case null => Defines.anyTypeName
    }
    val (name, fullname) = uniqueName("lambda", "", fullName(lambdaExpression))
    val signature = s"$returnType $fullname ${parameterListSignature(lambdaExpression, includeParamNames = false)}"
    val code      = s"$returnType $name ${parameterListSignature(lambdaExpression, includeParamNames = true)}"
    val methodNode = newMethodNode(
      lambdaExpression,
      StringUtils.normalizeSpace(name),
      code,
      StringUtils.normalizeSpace(fullname),
      filename
    ).isExternal(false).signature(StringUtils.normalizeSpace(signature))

    scope.pushNewScope(methodNode)
    val parameterNodes = withIndex(parameters(lambdaExpression.getDeclarator)) { (p, i) =>
      parameterNode(p, i)
    }
    setVariadic(parameterNodes, lambdaExpression)

    scope.popScope()

    val stubAst =
      methodStubAst(methodNode, parameterNodes, newMethodReturnNode(lambdaExpression, registerType(returnType)))
    val typeDeclAst = createFunctionTypeAndTypeDecl(lambdaExpression, methodNode, name, fullname, signature)
    Ast.storeInDiffGraph(stubAst.merge(typeDeclAst), diffGraph)

    Ast(newMethodRefNode(code, fullname, methodNode.astParentFullName, lambdaExpression))
  }

  protected def astForFunctionDeclarator(funcDecl: IASTFunctionDeclarator): Ast = {
    val filename       = fileName(funcDecl)
    val returnType     = typeForDeclSpecifier(funcDecl.getParent.asInstanceOf[IASTSimpleDeclaration].getDeclSpecifier)
    val name           = shortName(funcDecl)
    val fullname       = fullName(funcDecl)
    val templateParams = templateParameters(funcDecl).getOrElse("")
    val signature =
      s"$returnType $fullname$templateParams ${parameterListSignature(funcDecl, includeParamNames = false)}"
    val code = s"$returnType $name ${parameterListSignature(funcDecl, includeParamNames = true)}"
    val methodNode =
      newMethodNode(funcDecl, StringUtils.normalizeSpace(name), code, StringUtils.normalizeSpace(fullname), filename)
        .isExternal(false)
        .signature(StringUtils.normalizeSpace(signature))

    scope.pushNewScope(methodNode)

    val parameterNodes = withIndex(parameters(funcDecl)) { (p, i) =>
      parameterNode(p, i)
    }
    setVariadic(parameterNodes, funcDecl)

    scope.popScope()

    val stubAst     = methodStubAst(methodNode, parameterNodes, newMethodReturnNode(funcDecl, registerType(returnType)))
    val typeDeclAst = createFunctionTypeAndTypeDecl(funcDecl, methodNode, name, fullname, signature)
    stubAst.merge(typeDeclAst)
  }

  protected def astForFunctionDefinition(funcDef: IASTFunctionDefinition): Ast = {
    val filename       = fileName(funcDef)
    val returnType     = typeForDeclSpecifier(funcDef.getDeclSpecifier)
    val name           = shortName(funcDef)
    val fullname       = fullName(funcDef)
    val templateParams = templateParameters(funcDef).getOrElse("")
    val signature =
      s"$returnType $fullname$templateParams ${parameterListSignature(funcDef, includeParamNames = false)}"
    val code = s"$returnType $name ${parameterListSignature(funcDef, includeParamNames = true)}"
    val methodNode =
      newMethodNode(funcDef, StringUtils.normalizeSpace(name), code, StringUtils.normalizeSpace(fullname), filename)
        .isExternal(false)
        .signature(StringUtils.normalizeSpace(signature))

    methodAstParentStack.push(methodNode)
    scope.pushNewScope(methodNode)

    val parameterNodes = withIndex(parameters(funcDef)) { (p, i) =>
      parameterNode(p, i)
    }
    setVariadic(parameterNodes, funcDef)

    val stubAst = methodAst(
      methodNode,
      parameterNodes,
      astForMethodBody(Option(funcDef.getBody)),
      newMethodReturnNode(funcDef, registerType(typeForDeclSpecifier(funcDef.getDeclSpecifier)))
    )

    scope.popScope()
    methodAstParentStack.pop()

    val typeDeclAst = createFunctionTypeAndTypeDecl(funcDef, methodNode, name, fullname, signature)
    stubAst.merge(typeDeclAst)
  }

  private def parameterNode(parameter: IASTNode, paramIndex: Int): NewMethodParameterIn = {
    val (name, code, tpe, variadic) = parameter match {
      case p: CASTParameterDeclaration =>
        (
          ASTStringUtil.getSimpleName(p.getDeclarator.getName),
          nodeSignature(p),
          cleanType(typeForDeclSpecifier(p.getDeclSpecifier)),
          false
        )
      case p: CPPASTParameterDeclaration =>
        (
          ASTStringUtil.getSimpleName(p.getDeclarator.getName),
          nodeSignature(p),
          cleanType(typeForDeclSpecifier(p.getDeclSpecifier)),
          p.getDeclarator.declaresParameterPack()
        )
      case s: IASTSimpleDeclaration =>
        (
          s.getDeclarators.headOption
            .map(n => ASTStringUtil.getSimpleName(n.getName))
            .getOrElse(uniqueName("parameter", "", "")._1),
          nodeSignature(s),
          cleanType(typeForDeclSpecifier(s)),
          false
        )
      case other =>
        (nodeSignature(other), nodeSignature(other), cleanType(typeForDeclSpecifier(other)), false)
    }

    val parameterNode =
      newParameterInNode(parameter, name, code, registerType(tpe), paramIndex, EvaluationStrategies.BY_VALUE, variadic)
    scope.addToScope(name, (parameterNode, tpe))
    parameterNode
  }

  private def astForMethodBody(body: Option[IASTStatement]): Ast = body match {
    case Some(b: IASTCompoundStatement) => astForBlockStatement(b)
    case None                           => Ast(NewBlock())
    case Some(b)                        => astForNode(b)
  }

}
