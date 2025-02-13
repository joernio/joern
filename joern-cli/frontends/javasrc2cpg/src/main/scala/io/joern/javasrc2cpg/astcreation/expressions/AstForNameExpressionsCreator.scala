package io.joern.javasrc2cpg.astcreation.expressions

import com.github.javaparser.ast.Node
import com.github.javaparser.ast.expr.NameExpr
import com.github.javaparser.resolution.declarations.ResolvedFieldDeclaration
import io.joern.javasrc2cpg.astcreation.{AstCreator, ExpectedType}
import io.joern.javasrc2cpg.util.NameConstants
import io.joern.x2cpg.Ast
import io.shiftleft.codepropertygraph.generated.nodes.{NewLocal, NewMethodParameterIn, NewTypeRef}

import scala.util.Success
import io.joern.javasrc2cpg.scope.Scope.{CapturedVariable, NotInScope, ScopeMember, SimpleVariable}
import org.slf4j.LoggerFactory
import io.joern.x2cpg.utils.AstPropertiesUtil.*
import io.shiftleft.codepropertygraph.generated.Operators
import io.joern.x2cpg.utils.NodeBuilders.{newIdentifierNode, newOperatorCallNode}

trait AstForNameExpressionsCreator { this: AstCreator =>

  private val logger = LoggerFactory.getLogger(this.getClass)

  private[expressions] def astForNameExpr(nameExpr: NameExpr, expectedType: ExpectedType): Ast = {
    val name = nameExpr.getName.toString
    val typeFullName = expressionReturnTypeFullName(nameExpr)
      .orElse(getTypeFullName(expectedType))
      .map(typeInfoCalc.registerType)

    scope.lookupVariable(name) match {
      case NotInScope =>
        astForStaticImportOrUnknown(nameExpr, name, typeFullName)

      case SimpleVariable(variable: ScopeMember) =>
        createImplicitBaseFieldAccess(
          variable.isStatic,
          scope.enclosingTypeDecl.name.get,
          scope.enclosingTypeDecl.fullName.get,
          nameExpr,
          variable.name,
          variable.typeFullName
        )

      case SimpleVariable(variable) =>
        val mangledName = variable.mangledName
        val identifier =
          identifierNode(nameExpr, mangledName, mangledName, typeFullName.getOrElse(defaultTypeFallback()))
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

  private[expressions] def createImplicitBaseFieldAccess(
    isStatic: Boolean,
    baseTypeDeclName: String,
    baseTypeDeclFullName: String,
    node: Node,
    fieldName: String,
    fieldTypeFullName: String
  ): Ast = {
    val base =
      if (isStatic) {
        val typ = NewTypeRef()
          .code(baseTypeDeclName)
          .typeFullName(baseTypeDeclFullName)
          .lineNumber(line(node))
          .columnNumber(column(node))
        Ast(typ)
      } else {
        val identifier = newIdentifierNode(NameConstants.This, baseTypeDeclFullName)
        val refsTo     = scope.lookupVariable(NameConstants.This).variableNode.toList
        Ast(identifier).withRefEdges(identifier, refsTo)
      }
    fieldAccessAst(
      base,
      s"${base.rootCodeOrEmpty}.$fieldName",
      line(node),
      column(node),
      fieldName,
      fieldTypeFullName,
      line(node),
      column(node)
    )
  }

  private def astForStaticImportOrUnknown(nameExpr: NameExpr, name: String, typeFullName: Option[String]): Ast = {
    tryWithSafeStackOverflow(nameExpr.resolve()) match {
      case Success(value: ResolvedFieldDeclaration) =>
        // TODO using the enclosingTypeDecl is wrong if the field was imported via a static import.
        createImplicitBaseFieldAccess(
          value.asField().isStatic,
          typeInfoCalc.name(value.declaringType()).getOrElse(defaultTypeFallback()),
          typeInfoCalc.fullName(value.declaringType()).getOrElse(defaultTypeFallback()),
          nameExpr,
          name,
          typeFullName.getOrElse(defaultTypeFallback())
        )

      case _ =>
        Ast(identifierNode(nameExpr, name, name, typeFullName.getOrElse(defaultTypeFallback())))
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
        Ast(identifierNode(nameExpr, variable.mangledName, variable.mangledName, variable.typeFullName))

      case SimpleVariable(scopeVariable) =>
        val thisIdentifier =
          identifierNode(nameExpr, scopeVariable.mangledName, scopeVariable.mangledName, scopeVariable.typeFullName)
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
          s"${outerClassChain.rootCodeOrEmpty}.${variable.mangledName}",
          Some(variable.typeFullName),
          lineNumber,
          columnNumber
        )

        val captureFieldIdentifier = fieldIdentifierNode(nameExpr, variable.mangledName, variable.name)
        callAst(finalFieldAccess, List(outerClassChain, Ast(captureFieldIdentifier)))
    }
  }
}
