package io.joern.javasrc2cpg.astcreation.expressions

import com.github.javaparser.ast.expr.NameExpr
import com.github.javaparser.resolution.declarations.ResolvedFieldDeclaration
import io.joern.javasrc2cpg.astcreation.{AstCreator, ExpectedType}
import io.joern.javasrc2cpg.typesolvers.TypeInfoCalculator.TypeConstants
import io.joern.javasrc2cpg.util.NameConstants
import io.joern.x2cpg.{Ast, Defines}
import io.shiftleft.codepropertygraph.generated.nodes.{NewLocal, NewMethodParameterIn}

import scala.util.Success
import io.joern.javasrc2cpg.scope.Scope.{
  CapturedVariable,
  NotInScope,
  ScopeMember,
  ScopeParameter,
  ScopeVariable,
  SimpleVariable
}
import io.shiftleft.codepropertygraph.generated.nodes.NewTypeDecl
import org.slf4j.LoggerFactory
import io.shiftleft.codepropertygraph.generated.nodes.NewUnknown
import io.joern.x2cpg.utils.AstPropertiesUtil.*
import io.shiftleft.codepropertygraph.generated.Operators
import io.joern.x2cpg.utils.NodeBuilders.newOperatorCallNode

trait AstForNameExpressionsCreator { this: AstCreator =>

  private val logger = LoggerFactory.getLogger(this.getClass)

  private[expressions] def astForNameExpr(nameExpr: NameExpr, expectedType: ExpectedType): Ast = {
    val name = nameExpr.getName.toString
    val typeFullName = expressionReturnTypeFullName(nameExpr)
      .orElse(expectedType.fullName)
      .map(typeInfoCalc.registerType)

    scope.lookupVariable(name) match {
      case NotInScope =>
        astForStaticImportOrUnknown(nameExpr, name, typeFullName)

      case SimpleVariable(variable: ScopeMember) =>
        val targetName =
          Option.when(variable.isStatic)(scope.enclosingTypeDecl.fullName).flatten.getOrElse(NameConstants.This)
        fieldAccessAst(
          targetName,
          scope.enclosingTypeDecl.fullName,
          variable.name,
          Some(variable.typeFullName),
          line(nameExpr),
          column(nameExpr)
        )

      case SimpleVariable(variable) =>
        val identifier = identifierNode(nameExpr, name, name, typeFullName.getOrElse(TypeConstants.Any))
        val captured = variable.node match {
          case param: NewMethodParameterIn => Some(param)
          case local: NewLocal             => Some(local)
          case _                           => None
        }
        captured.foldLeft(Ast(identifier))((ast, variableNode) => ast.withRefEdge(identifier, variableNode))

      case capturedVariable: CapturedVariable =>
        scope.registerCaptureUse(capturedVariable)
        astForCapturedVariable(nameExpr, capturedVariable)
    }
  }

  private def astForStaticImportOrUnknown(nameExpr: NameExpr, name: String, typeFullName: Option[String]): Ast = {
    tryWithSafeStackOverflow(nameExpr.resolve()) match {
      case Success(value) if value.isField =>
        val identifierName = if (value.asField.isStatic) {
          // TODO: v is wrong. Statically imported expressions can also be represented by just the name.
          // A static field represented by a NameExpr must belong to the class in which it's used. Static fields
          // from other classes are represented by a FieldAccessExpr instead.
          scope.enclosingTypeDecl.map(_.typeDecl.name).getOrElse(s"${Defines.UnresolvedNamespace}.$name")
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
        Ast(identifierNode(nameExpr, name, name, typeFullName.getOrElse(TypeConstants.Any)))
    }
  }

  private def astForCapturedVariable(nameExpr: NameExpr, capturedVariable: CapturedVariable): Ast = {
    val variable      = capturedVariable.variable
    val typeDeclChain = capturedVariable.typeDeclChain

    scope.lookupVariable("this") match {
      case NotInScope | CapturedVariable(_, _) =>
        logger.warn(
          s"Attempted to create AST for captured variable ${variable.name}, but could not find `this` param in direct scope."
        )
        Ast(identifierNode(nameExpr, variable.name, variable.name, variable.typeFullName))

      case SimpleVariable(scopeVariable) =>
        val thisIdentifier =
          identifierNode(nameExpr, scopeVariable.name, scopeVariable.name, scopeVariable.typeFullName)
        val thisAst = Ast(thisIdentifier).withRefEdge(thisIdentifier, scopeVariable.node)

        val lineNumber   = line(nameExpr)
        val columnNumber = column(nameExpr)
        val outerClassChain = typeDeclChain.foldLeft(thisAst) { case (accAst, typeDecl) =>
          val rootNode = newOperatorCallNode(
            Operators.fieldAccess,
            s"${accAst.rootCodeOrEmpty}.${NameConstants.OuterClass}",
            Some(typeDecl.fullName),
            lineNumber,
            columnNumber
          )

          val outerClassIdentifier = fieldIdentifierNode(nameExpr, NameConstants.OuterClass, NameConstants.OuterClass)
          callAst(rootNode, List(accAst, Ast(outerClassIdentifier)))
        }

        val finalFieldAccess = newOperatorCallNode(
          Operators.fieldAccess,
          s"${outerClassChain.rootCodeOrEmpty}.${variable.name}",
          Some(variable.typeFullName),
          lineNumber,
          columnNumber
        )

        val captureFieldIdentifier = fieldIdentifierNode(nameExpr, variable.name, variable.name)
        callAst(finalFieldAccess, List(outerClassChain, Ast(captureFieldIdentifier)))
    }
  }
}
