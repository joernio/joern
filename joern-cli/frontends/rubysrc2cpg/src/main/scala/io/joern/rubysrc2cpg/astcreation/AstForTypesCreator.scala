package io.joern.rubysrc2cpg.astcreation

import io.joern.rubysrc2cpg.astcreation.RubyIntermediateAst.*
import io.joern.rubysrc2cpg.datastructures.{BlockScope, MethodScope, ModuleScope, TypeScope}
import io.joern.rubysrc2cpg.passes.Defines
import io.joern.x2cpg.{Ast, ValidationMode, Defines as XDefines}
import io.shiftleft.codepropertygraph.generated.nodes.{NewLocal, NewMethodParameterIn}
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, EvaluationStrategies, Operators}

import scala.collection.immutable.List

trait AstForTypesCreator(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  protected def astForClassDeclaration(node: RubyNode & TypeDeclaration): Ast = {
    node.name match
      case name: SimpleIdentifier => astForSimpleNamedClassDeclaration(node, name)
      case name =>
        logger.warn(s"Qualified class names are not supported yet: ${name.text} ($relativeFileName), skipping")
        astForUnknown(node)
  }

  private def getBaseClassName(node: RubyNode): Option[String] = {
    node match
      case simpleIdentifier: SimpleIdentifier =>
        val name = simpleIdentifier.text
        scope.lookupVariable(name) match {
          case Some(_) => Option(name) // in the case of singleton classes, we want to keep the variable name
          case None    => scope.tryResolveTypeReference(name).map(_.name).orElse(Option(name))
        }
      case _: SelfIdentifier =>
        scope.surroundingTypeFullName
      case qualifiedBaseClass: MemberAccess =>
        scope
          .tryResolveTypeReference(qualifiedBaseClass.toString)
          .map(_.name)
          .orElse(Option(qualifiedBaseClass.toString))
      case x =>
        logger.warn(
          s"Base class names of type ${x.getClass} are not supported yet: ${code(node)} ($relativeFileName), skipping"
        )
        None
  }

  private def astForSimpleNamedClassDeclaration(
    node: RubyNode & TypeDeclaration,
    nameIdentifier: SimpleIdentifier
  ): Ast = {
    val className     = nameIdentifier.text
    val inheritsFrom  = node.baseClass.flatMap(getBaseClassName).toList
    val classFullName = computeClassFullName(className)
    val typeDecl = typeDeclNode(
      node = node,
      name = className,
      fullName = classFullName,
      filename = relativeFileName,
      code = code(node),
      astParentType = scope.surroundingAstLabel.getOrElse(""),
      astParentFullName = scope.surroundingScopeFullName.getOrElse(""),
      inherits = inheritsFrom,
      alias = None
    )

    node match {
      case _: ModuleDeclaration => scope.pushNewScope(ModuleScope(classFullName))
      case _: TypeDeclaration   => scope.pushNewScope(TypeScope(classFullName, List.empty))
    }

    val classBody =
      node.body.asInstanceOf[StatementList] // for now (bodyStatement is a superset of stmtList)

    val classBodyAsts = classBody.statements.flatMap(astsForStatement) match {
      case bodyAsts if scope.shouldGenerateDefaultConstructor && this.parseLevel == AstParseLevel.FULL_AST =>
        val bodyStart = classBody.span.spanStart()
        val initBody  = StatementList(List())(bodyStart)
        val methodDecl = astForMethodDeclaration(
          MethodDeclaration(XDefines.ConstructorMethodName, List(), initBody)(bodyStart)
        )
        methodDecl ++ bodyAsts
      case bodyAsts => bodyAsts
    }

    val fieldMemberNodes = node match {
      case classDecl: ClassDeclaration =>
        classDecl.fields.map { x =>
          val name = code(x)
          Ast(memberNode(x, name, name, Defines.Any))
        }
      case _ => Seq.empty
    }

    scope.popScope()

    Ast(typeDecl).withChildren(fieldMemberNodes).withChildren(classBodyAsts)
  }

  protected def astsForFieldDeclarations(node: FieldsDeclaration): Seq[Ast] = {
    node.fieldNames.flatMap(astsForSingleFieldDeclaration(node, _))
  }

  private def astsForSingleFieldDeclaration(node: FieldsDeclaration, nameNode: RubyNode): Seq[Ast] = {
    nameNode match
      case nameAsSymbol: StaticLiteral if nameAsSymbol.isSymbol =>
        val fieldName   = nameAsSymbol.innerText.prepended('@')
        val memberNode_ = memberNode(nameAsSymbol, fieldName, code(node), Defines.Any)
        val memberAst   = Ast(memberNode_)
        val getterAst   = Option.when(node.hasGetter)(astForGetterMethod(node, fieldName))
        val setterAst   = Option.when(node.hasSetter)(astForSetterMethod(node, fieldName))
        Seq(memberAst) ++ getterAst.toList ++ setterAst.toList
      case _ =>
        logger.warn(s"Unsupported field declaration: ${nameNode.text}, skipping")
        Seq()
  }

  // creates a `def <name>() { return <fieldName> }` METHOD, for <fieldName> = @<name>.
  private def astForGetterMethod(node: FieldsDeclaration, fieldName: String): Ast = {
    val name     = fieldName.drop(1)
    val fullName = computeMethodFullName(name)
    val method = methodNode(
      node = node,
      name = name,
      fullName = fullName,
      code = s"def $name (...)",
      signature = None,
      fileName = relativeFileName,
      astParentType = scope.surroundingAstLabel,
      astParentFullName = scope.surroundingScopeFullName
    )
    scope.pushNewScope(MethodScope(fullName, procParamGen.fresh))
    val block_ = blockNode(node)
    scope.pushNewScope(BlockScope(block_))
    // TODO: Should it be `return this.@abc`?
    val returnAst_ = {
      val returnNode_         = returnNode(node, s"return $fieldName")
      val fieldNameIdentifier = identifierNode(node, fieldName, fieldName, Defines.Any)
      returnAst(returnNode_, Seq(Ast(fieldNameIdentifier)))
    }

    val methodBody = blockAst(block_, List(returnAst_))
    scope.popScope()
    scope.popScope()
    methodAst(method, Seq(), methodBody, methodReturnNode(node, Defines.Any))
  }

  // creates a `def <name>=(x) { <fieldName> = x }` METHOD, for <fieldName> = @<name>
  private def astForSetterMethod(node: FieldsDeclaration, fieldName: String): Ast = {
    val name     = fieldName.drop(1) + "="
    val fullName = computeMethodFullName(name)
    val method = methodNode(
      node = node,
      name = name,
      fullName = fullName,
      code = s"def $name (...)",
      signature = None,
      fileName = relativeFileName,
      astParentType = scope.surroundingAstLabel,
      astParentFullName = scope.surroundingScopeFullName
    )
    scope.pushNewScope(MethodScope(fullName, procParamGen.fresh))
    val parameter = parameterInNode(node, "x", "x", 1, false, EvaluationStrategies.BY_REFERENCE)
    val methodBody = {
      val block_ = blockNode(node)
      scope.pushNewScope(BlockScope(block_))
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
      scope.popScope()
      blockAst(blockNode(node), List(assignmentAst))
    }
    scope.popScope()
    methodAst(method, Seq(Ast(parameter)), methodBody, methodReturnNode(node, Defines.Any))
  }

}
