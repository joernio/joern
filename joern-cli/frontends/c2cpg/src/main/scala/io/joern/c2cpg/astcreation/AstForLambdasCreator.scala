package io.joern.c2cpg.astcreation

import io.joern.x2cpg.Ast
import io.joern.x2cpg.ValidationMode
import io.joern.x2cpg.utils.NodeBuilders.newModifierNode
import io.shiftleft.codepropertygraph.generated.ModifierTypes
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTLambdaExpression

trait AstForLambdasCreator(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  protected def astForLambdaExpression(lambdaExpression: ICPPASTLambdaExpression): Ast = {
    val filename                                                  = fileName(lambdaExpression)
    val MethodFullNameInfo(name, fullName, signature, returnType) = methodFullNameInfo(lambdaExpression)
    val codeString                                                = code(lambdaExpression)
    val methodNode_ = methodNode(lambdaExpression, name, codeString, fullName, Some(signature), filename)

    scope.pushNewScope(methodNode_)

    val parameterNodes = withIndex(parameters(lambdaExpression.getDeclarator)) { (p, i) =>
      parameterNode(p, i)
    }
    setVariadic(parameterNodes, lambdaExpression)

    val astForLambda = methodAst(
      methodNode_,
      parameterNodes.map(Ast(_)),
      astForMethodBody(Option(lambdaExpression.getBody)),
      methodReturnNode(lambdaExpression, registerType(returnType)),
      newModifierNode(ModifierTypes.LAMBDA) :: Nil
    )

    scope.popScope()

    val typeDeclAst = createFunctionTypeAndTypeDecl(lambdaExpression, methodNode_, name, fullName, signature)
    Ast.storeInDiffGraph(astForLambda.merge(typeDeclAst), diffGraph)

    Ast(methodRefNode(lambdaExpression, codeString, fullName, registerType(fullName)))
  }

}
