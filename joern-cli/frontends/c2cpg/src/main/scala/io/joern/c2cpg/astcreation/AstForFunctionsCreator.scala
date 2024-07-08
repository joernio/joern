package io.joern.c2cpg.astcreation

import io.joern.x2cpg.Ast
import io.joern.x2cpg.ValidationMode
import io.joern.x2cpg.Defines as X2CpgDefines
import io.joern.x2cpg.datastructures.Stack.*
import io.joern.x2cpg.utils.NodeBuilders.newModifierNode
import io.shiftleft.codepropertygraph.generated.EvaluationStrategies
import io.shiftleft.codepropertygraph.generated.ModifierTypes
import io.shiftleft.codepropertygraph.generated.nodes.*
import org.apache.commons.lang3.StringUtils
import org.eclipse.cdt.core.dom.ast.*
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTLambdaExpression
import org.eclipse.cdt.core.dom.ast.gnu.c.ICASTKnRFunctionDeclarator
import org.eclipse.cdt.internal.core.dom.parser.c.CASTFunctionDeclarator
import org.eclipse.cdt.internal.core.dom.parser.c.CASTParameterDeclaration
import org.eclipse.cdt.internal.core.dom.parser.cpp.CPPASTFunctionDeclarator
import org.eclipse.cdt.internal.core.dom.parser.cpp.CPPASTFunctionDefinition
import org.eclipse.cdt.internal.core.dom.parser.cpp.CPPASTParameterDeclaration
import org.eclipse.cdt.internal.core.model.ASTStringUtil

import scala.annotation.tailrec
import scala.collection.mutable

trait AstForFunctionsCreator(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  private val seenFunctionFullnames = mutable.HashSet.empty[String]

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
      val typeDeclNode_ = typeDeclNode(
        node,
        normalizedName,
        normalizedFullName,
        method.filename,
        normalizedName,
        astParentType,
        astParentFullName
      )
      Ast.storeInDiffGraph(Ast(typeDeclNode_), diffGraph)
      typeDeclNode_
    }

    method.astParentFullName = parentNode.fullName
    method.astParentType = parentNode.label
    val functionBinding = NewBinding().name(normalizedName).methodFullName(normalizedFullName).signature(signature)
    Ast(functionBinding).withBindsEdge(parentNode, functionBinding).withRefEdge(functionBinding, method)
  }

  private def parameters(functionNode: IASTNode): Seq[IASTNode] = functionNode match {
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
  private def isVariadic(functionNode: IASTNode): Boolean = functionNode match {
    case decl: CPPASTFunctionDeclarator            => decl.takesVarArgs()
    case decl: CASTFunctionDeclarator              => decl.takesVarArgs()
    case defn: IASTFunctionDefinition              => isVariadic(defn.getDeclarator)
    case lambdaExpression: ICPPASTLambdaExpression => isVariadic(lambdaExpression.getDeclarator)
    case _                                         => false
  }

  private def parameterListSignature(func: IASTNode): String = {
    val variadic = if (isVariadic(func)) "..." else ""
    val elements = parameters(func).map {
      case p: IASTParameterDeclaration => typeForDeclSpecifier(p.getDeclSpecifier)
      case other                       => typeForDeclSpecifier(other)
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
          case null           => Defines.Any
        }
      case null => Defines.Any
    }
    val name        = nextClosureName()
    val rawFullname = fullName(lambdaExpression)
    val fixedFullName = if (rawFullname.contains("[") || rawFullname.contains("{")) {
      // FIXME: the lambda may be located in something we are not able to generate a correct fullname yet
      s"${X2CpgDefines.UnresolvedSignature}."
    } else rawFullname
    val fullname    = s"$fixedFullName$name"
    val signature   = s"$returnType${parameterListSignature(lambdaExpression)}"
    val codeString  = code(lambdaExpression)
    val methodNode_ = methodNode(lambdaExpression, name, codeString, fullname, Some(signature), filename)

    scope.pushNewScope(methodNode_)
    val parameterNodes = withIndex(parameters(lambdaExpression.getDeclarator)) { (p, i) =>
      parameterNode(p, i)
    }
    setVariadic(parameterNodes, lambdaExpression)

    scope.popScope()

    val astForLambda = methodAst(
      methodNode_,
      parameterNodes.map(Ast(_)),
      astForMethodBody(Option(lambdaExpression.getBody)),
      methodReturnNode(lambdaExpression, registerType(returnType)),
      newModifierNode(ModifierTypes.LAMBDA) :: Nil
    )
    val typeDeclAst = createFunctionTypeAndTypeDecl(lambdaExpression, methodNode_, name, fullname, signature)
    Ast.storeInDiffGraph(astForLambda.merge(typeDeclAst), diffGraph)

    Ast(methodRefNode(lambdaExpression, codeString, fullname, registerType(fullname)))
  }

  protected def astForFunctionDeclarator(funcDecl: IASTFunctionDeclarator): Ast = {
    funcDecl.getName.resolveBinding() match {
      case function: IFunction =>
        val returnType = typeForDeclSpecifier(funcDecl.getParent.asInstanceOf[IASTSimpleDeclaration].getDeclSpecifier)
        val name       = shortName(funcDecl)
        val fullname   = fullName(funcDecl)

        val fixedName = if (name.isEmpty) {
          nextClosureName()
        } else name
        val fixedFullName = if (fullname.isEmpty) {
          s"${X2CpgDefines.UnresolvedNamespace}.$name"
        } else fullname

        val templateParams = templateParameters(funcDecl).getOrElse("")
        val signature =
          s"$returnType${parameterListSignature(funcDecl)}"

        if (seenFunctionFullnames.add(fixedFullName)) {
          val codeString  = code(funcDecl.getParent)
          val filename    = fileName(funcDecl)
          val methodNode_ = methodNode(funcDecl, fixedName, codeString, fixedFullName, Some(signature), filename)

          scope.pushNewScope(methodNode_)

          val parameterNodes = withIndex(parameters(funcDecl)) { (p, i) =>
            parameterNode(p, i)
          }
          setVariadic(parameterNodes, funcDecl)

          scope.popScope()

          val stubAst =
            methodStubAst(methodNode_, parameterNodes.map(Ast(_)), methodReturnNode(funcDecl, registerType(returnType)))
          val typeDeclAst = createFunctionTypeAndTypeDecl(funcDecl, methodNode_, fixedName, fixedFullName, signature)
          stubAst.merge(typeDeclAst)
        } else {
          Ast()
        }
      case field: IField =>
        // TODO create a member for the field
        // We get here a least for function pointer member declarations in classes like:
        // class A {
        //   public:
        //     void (*foo)(int);
        // };
        Ast()
      case typeDef: ITypedef =>
        // TODO handle typeDecl for now we just ignore this.
        Ast()
      case other =>
        notHandledYet(funcDecl)
    }

  }

  private def isCppConstructor(funcDef: IASTFunctionDefinition): Boolean = {
    funcDef match {
      case cppFunc: CPPASTFunctionDefinition => cppFunc.getMemberInitializers.nonEmpty
      case _                                 => false
    }
  }

  protected def astForFunctionDefinition(funcDef: IASTFunctionDefinition): Ast = {
    val filename = fileName(funcDef)
    val returnType = if (isCppConstructor(funcDef)) {
      typeFor(funcDef.asInstanceOf[CPPASTFunctionDefinition].getMemberInitializers.head.getInitializer)
    } else typeForDeclSpecifier(funcDef.getDeclSpecifier)
    val name = shortName(funcDef)
    val fullname = fullName(funcDef) match {
      case ""    => s"${X2CpgDefines.UnresolvedNamespace}.$name"
      case other => other
    }
    val templateParams = templateParameters(funcDef).getOrElse("")

    val signature =
      s"$returnType${parameterListSignature(funcDef)}"
    seenFunctionFullnames.add(fullname)

    val codeString  = code(funcDef)
    val methodNode_ = methodNode(funcDef, name, codeString, fullname, Some(signature), filename)

    methodAstParentStack.push(methodNode_)
    scope.pushNewScope(methodNode_)

    val parameterNodes = withIndex(parameters(funcDef)) { (p, i) =>
      parameterNode(p, i)
    }
    setVariadic(parameterNodes, funcDef)

    val modifiers = if (isCppConstructor(funcDef)) {
      List(newModifierNode(ModifierTypes.CONSTRUCTOR), newModifierNode(ModifierTypes.PUBLIC))
    } else Nil

    val astForMethod = methodAst(
      methodNode_,
      parameterNodes.map(Ast(_)),
      astForMethodBody(Option(funcDef.getBody)),
      methodReturnNode(funcDef, registerType(returnType)),
      modifiers = modifiers
    )

    scope.popScope()
    methodAstParentStack.pop()

    val typeDeclAst = createFunctionTypeAndTypeDecl(funcDef, methodNode_, name, fullname, signature)
    astForMethod.merge(typeDeclAst)
  }

  private def parameterNode(parameter: IASTNode, paramIndex: Int): NewMethodParameterIn = {
    val (name, codeString, tpe, variadic) = parameter match {
      case p: CASTParameterDeclaration =>
        (
          ASTStringUtil.getSimpleName(p.getDeclarator.getName),
          code(p),
          cleanType(typeForDeclSpecifier(p.getDeclSpecifier)),
          false
        )
      case p: CPPASTParameterDeclaration =>
        (
          ASTStringUtil.getSimpleName(p.getDeclarator.getName),
          code(p),
          cleanType(typeForDeclSpecifier(p.getDeclSpecifier)),
          p.getDeclarator.declaresParameterPack()
        )
      case s: IASTSimpleDeclaration =>
        (
          s.getDeclarators.headOption
            .map(n => ASTStringUtil.getSimpleName(n.getName))
            .getOrElse(uniqueName("parameter", "", "")._1),
          code(s),
          cleanType(typeForDeclSpecifier(s)),
          false
        )
      case other =>
        (code(other), code(other), cleanType(typeForDeclSpecifier(other)), false)
    }

    val parameterNode =
      parameterInNode(
        parameter,
        name,
        codeString,
        paramIndex,
        variadic,
        EvaluationStrategies.BY_VALUE,
        registerType(tpe)
      )
    scope.addToScope(name, (parameterNode, tpe))
    parameterNode
  }

  private def astForMethodBody(body: Option[IASTStatement]): Ast = body match {
    case Some(b: IASTCompoundStatement) => astForBlockStatement(b)
    case Some(b)                        => astForNode(b)
    case None                           => blockAst(NewBlock().typeFullName(Defines.Any))
  }

}
