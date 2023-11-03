package io.joern.rubysrc2cpg.astcreation

import io.joern.rubysrc2cpg.parser.ParserAst
import io.joern.rubysrc2cpg.parser.ParserAst.*
import io.joern.rubysrc2cpg.passes.Defines
import io.joern.x2cpg.datastructures.Stack.*
import io.joern.x2cpg.{Ast, ValidationMode}
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, EvaluationStrategies, Operators}
import org.antlr.v4.runtime.ParserRuleContext

import scala.collection.immutable.List

trait AstForTypesCreator(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  protected def astForModuleDeclaration(node: ModuleDeclaration): Ast = {
    // TODO: Might be wrong here (hence this placeholder), but I'm assuming modules ~= abstract classes.
    val classDecl = ClassDeclaration(node.ctx, node.moduleName, None, node.body)
    astForClassDeclaration(classDecl)
  }

  protected def astForClassDeclaration(node: ClassDeclaration): Ast = {
    ParserAst(node.className) match
      case name: SimpleIdentifier => astForSimpleNamedClassDeclaration(node, name)
      case name =>
        logger.warn(s"Qualified class names are not supported yet: ${name.text} ($relativeFileName), skipping")
        astForUnknown(node)
  }

  private def getBaseClassName(node: ParserNode): Option[String] = {
    node match
      case simpleIdentifier: SimpleIdentifier => Some(simpleIdentifier.text)
      case _ =>
        logger.warn(s"Qualified base class names are not supported yet: ${code(node)} ($relativeFileName), skipping")
        None
  }

  private def astForSimpleNamedClassDeclaration(node: ClassDeclaration, nameIdentifier: SimpleIdentifier): Ast = {
    val className    = nameIdentifier.text
    val inheritsFrom = node.baseClass.flatMap(ParserAst.apply.andThen(getBaseClassName)).toList
    val typeDecl = typeDeclNode(
      node = node,
      name = className,
      fullName = computeClassFullName(className),
      filename = relativeFileName,
      code = code(node),
      astParentType = getEnclosingAstType,
      astParentFullName = getEnclosingAstFullName,
      inherits = inheritsFrom,
      alias = None
    )
    methodAstParentStack.push(typeDecl)
    scope.pushNewScope(typeDecl)
    // TODO: ctor
    val classBody =
      ParserAst(node.body).asInstanceOf[StatementList] // for now (bodyStatement is a superset of stmtList)
    val classBodyAsts = classBody.statements.flatMap(astsForStatement)
    scope.popScope()
    methodAstParentStack.pop()
    Ast(typeDecl).withChildren(classBodyAsts)
  }

  protected def astsForFieldDeclarations(node: FieldsDeclaration): Seq[Ast] = {
    node.fieldNames.flatMap(astsForSingleFieldDeclaration(node, _))
  }

  private def astsForSingleFieldDeclaration(node: FieldsDeclaration, nameCtx: ParserRuleContext): Seq[Ast] = {
    ParserAst(nameCtx) match
      case nameAsSymbol: StaticLiteral if nameAsSymbol.isSymbol =>
        val fieldName   = nameAsSymbol.innerText.prepended('@')
        val memberNode_ = memberNode(nameAsSymbol, fieldName, code(node), Defines.Any)
        val memberAst   = Ast(memberNode_)
        val getterAst   = Option.when(node.hasGetter)(astForGetterMethod(node, fieldName))
        val setterAst   = Option.when(node.hasSetter)(astForSetterMethod(node, fieldName))
        Seq(memberAst) ++ getterAst.toList ++ setterAst.toList
      case _ =>
        logger.warn(s"Unsupported field declaration: ${nameCtx.getText}, skipping")
        Seq()
  }

  // creates a `def <name>() { return <fieldName> }` METHOD, for <fieldName> = @<name>.
  private def astForGetterMethod(node: FieldsDeclaration, fieldName: String): Ast = {
    val name = fieldName.drop(1)
    val method = methodNode(
      node = node,
      name = name,
      fullName = computeMethodFullName(name),
      code = s"def $name (...)",
      signature = None,
      fileName = relativeFileName,
      astParentType = Some(getEnclosingAstType),
      astParentFullName = Some(getEnclosingAstFullName)
    )

    // TODO: Should it be `return this.@abc`?
    val returnAst_ = {
      val returnNode_         = returnNode(node, s"return $fieldName")
      val fieldNameIdentifier = identifierNode(node, fieldName, fieldName, Defines.Any)
      returnAst(returnNode_, Seq(Ast(fieldNameIdentifier)))
    }

    val block_     = blockNode(node)
    val methodBody = blockAst(block_, List(returnAst_))

    methodAst(method, Seq(), methodBody, methodReturnNode(node, Defines.Any))
  }

  // creates a `def <name>=(x) { <fieldName> = x }` METHOD, for <fieldName> = @<name>
  private def astForSetterMethod(node: FieldsDeclaration, fieldName: String): Ast = {
    val name = fieldName.drop(1) + "="
    val method = methodNode(
      node = node,
      name = name,
      fullName = computeMethodFullName(name),
      code = s"def $name (...)",
      signature = None,
      fileName = relativeFileName,
      astParentType = Some(getEnclosingAstType),
      astParentFullName = Some(getEnclosingAstFullName)
    )

    val parameter = parameterInNode(node, "x", "x", 1, false, EvaluationStrategies.BY_REFERENCE)
    val methodBody = {
      val lhs = identifierNode(node, fieldName, fieldName, Defines.Any)
      val rhs = identifierNode(node, parameter.name, parameter.name, Defines.Any)
      val assignmentCall = callNode(
        node,
        s"${lhs.code} = ${rhs.code}",
        Operators.assignment,
        Operators.assignment,
        DispatchTypes.STATIC_DISPATCH
      )
      val assignmentAst = callAst(assignmentCall, Seq(Ast(lhs), Ast(rhs)))
      blockAst(blockNode(node), List(assignmentAst))
    }

    methodAst(method, Seq(Ast(parameter)), methodBody, methodReturnNode(node, Defines.Any))
  }

}
