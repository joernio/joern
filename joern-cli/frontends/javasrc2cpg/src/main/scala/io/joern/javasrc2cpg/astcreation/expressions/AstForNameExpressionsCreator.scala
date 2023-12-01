package io.joern.javasrc2cpg.astcreation.expressions

import com.github.javaparser.ast.expr.NameExpr
import com.github.javaparser.resolution.declarations.ResolvedFieldDeclaration
import io.joern.javasrc2cpg.astcreation.{AstCreator, ExpectedType}
import io.joern.javasrc2cpg.typesolvers.TypeInfoCalculator.TypeConstants
import io.joern.javasrc2cpg.util.NameConstants
import io.joern.x2cpg.{Ast, Defines}
import io.shiftleft.codepropertygraph.generated.nodes.{NewLocal, NewMethodParameterIn}

import scala.util.Success

trait AstForNameExpressionsCreator { this: AstCreator =>
  private[expressions] def astForNameExpr(nameExpr: NameExpr, expectedType: ExpectedType): Ast = {
    val name = nameExpr.getName.toString
    val typeFullName = expressionReturnTypeFullName(nameExpr)
      .orElse(expectedType.fullName)
      .map(typeInfoCalc.registerType)

    tryWithSafeStackOverflow(nameExpr.resolve()) match {
      case Success(value) if value.isField =>
        val identifierName = if (value.asField.isStatic) {
          // A static field represented by a NameExpr must belong to the class in which it's used. Static fields
          // from other classes are represented by a FieldAccessExpr instead.
          scope.enclosingTypeDecl.map(_.name).getOrElse(s"${Defines.UnresolvedNamespace}.$name")
        } else {
          NameConstants.This
        }

        val identifierTypeFullName =
          value match {
            case fieldDecl: ResolvedFieldDeclaration =>
              // TODO It is not quite correct to use the declaring classes type.
              // Instead we should take the using classes type which is either the same or a
              // sub class of the declaring class.
              typeInfoCalc.fullName(fieldDecl.declaringType())
          }

        fieldAccessAst(identifierName, identifierTypeFullName, name, typeFullName, line(nameExpr), column(nameExpr))

      case _ =>
        val identifier = identifierNode(nameExpr, name, name, typeFullName.getOrElse(TypeConstants.Any))

        val variableOption = scope
          .lookupVariable(name)
          .variableNode
          .collect {
            case parameter: NewMethodParameterIn => parameter

            case local: NewLocal => local
          }

        variableOption.foldLeft(Ast(identifier))((ast, variableNode) => ast.withRefEdge(identifier, variableNode))
    }

  }
}
