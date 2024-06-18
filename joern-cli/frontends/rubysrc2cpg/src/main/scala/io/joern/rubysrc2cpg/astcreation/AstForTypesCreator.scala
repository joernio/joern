package io.joern.rubysrc2cpg.astcreation

import io.joern.rubysrc2cpg.astcreation.RubyIntermediateAst.*
import io.joern.rubysrc2cpg.datastructures.{BlockScope, MethodScope, ModuleScope, TypeScope}
import io.joern.rubysrc2cpg.passes.Defines
import io.joern.x2cpg.utils.NodeBuilders.newModifierNode
import io.joern.x2cpg.{Ast, ValidationMode, Defines as XDefines}
import io.shiftleft.codepropertygraph.generated.nodes.{
  NewCall,
  NewFieldIdentifier,
  NewIdentifier,
  NewMethod,
  NewTypeDecl,
  NewTypeRef
}
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, EvaluationStrategies, ModifierTypes, Operators}

import scala.collection.immutable.List
import scala.collection.mutable

trait AstForTypesCreator(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  protected def astForClassDeclaration(node: RubyNode & TypeDeclaration): Seq[Ast] = {
    node.name match
      case name: SimpleIdentifier => astForSimpleNamedClassDeclaration(node, name)
      case name =>
        logger.warn(s"Qualified class names are not supported yet: ${name.text} ($relativeFileName), skipping")
        astForUnknown(node) :: Nil
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
  ): Seq[Ast] = {
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
    /*
      In Ruby, there are semantic differences between the ordinary class and singleton class (think "meta" class in
      Python). Similar to how Java allows both static and dynamic methods/fields/etc. within the same type declaration,
      Ruby allows `self` methods and @@ fields to be defined alongside ordinary methods and @ fields. However, both
      classes are more dynamic and have separate behaviours in Ruby and we model it as such.

      To signify the singleton type, we add the <class> tag.
     */
    val singletonTypeDecl = typeDecl.copy
      .name(s"$className<class>")
      .fullName(s"$classFullName<class>")
      .inheritsFromTypeFullName(inheritsFrom.map(x => s"$x<class>"))

    val (classModifiers, singletonModifiers) = node match {
      case _: ModuleDeclaration =>
        scope.pushNewScope(ModuleScope(classFullName))
        (
          ModifierTypes.VIRTUAL :: Nil map newModifierNode map Ast.apply,
          ModifierTypes.VIRTUAL :: ModifierTypes.FINAL :: Nil map newModifierNode map Ast.apply
        )
      case _: TypeDeclaration =>
        scope.pushNewScope(TypeScope(classFullName, List.empty))
        (
          ModifierTypes.VIRTUAL :: Nil map newModifierNode map Ast.apply,
          ModifierTypes.VIRTUAL :: Nil map newModifierNode map Ast.apply
        )
    }

    val classBody =
      node.body.asInstanceOf[StatementList] // for now (bodyStatement is a superset of stmtList)

    def handleDefaultConstructor(bodyAsts: Seq[Ast]): Seq[Ast] = bodyAsts match {
      case bodyAsts if scope.shouldGenerateDefaultConstructor && this.parseLevel == AstParseLevel.FULL_AST =>
        val bodyStart  = classBody.span.spanStart()
        val initBody   = StatementList(List())(bodyStart)
        val methodDecl = astForMethodDeclaration(MethodDeclaration(Defines.Initialize, List(), initBody)(bodyStart))
        methodDecl ++ bodyAsts
      case bodyAsts => bodyAsts
    }

    // TODO: Test the <body> method and give fields a home within them
    val PART_OF_BODY      = 0
    val PART_OF_SINGLETON = 1
    val PART_OF_CLASS     = 2

    val singletonBodyAsts = mutable.Buffer.empty[Ast]
    val classBodyAsts     = mutable.Buffer.empty[Ast]
    classBody.statements
      .map {
        case n: MethodDeclaration if n.methodName == Defines.InitializeClass =>
          n.copy(methodName = Defines.Initialize)(n.span) -> PART_OF_SINGLETON
        case n: (SingletonMethodDeclaration | MethodDeclaration | FieldsDeclaration | TypeDeclaration) =>
          n -> PART_OF_CLASS
        case n => n -> PART_OF_BODY
      }
      .groupBy(_._2)
      .map { case (x, xs) => x -> xs.map(_._1) }
      .foreach {
        case (PART_OF_SINGLETON, xs) =>
          singletonBodyAsts.appendAll(handleDefaultConstructor(xs.flatMap(astsForStatement)))
        case (PART_OF_CLASS, xs) => classBodyAsts.appendAll(handleDefaultConstructor(xs.flatMap(astsForStatement)))
        case (_, xs) =>
          val fakeBodyAst = astsForStatement(
            MethodDeclaration(Defines.TypeDeclBody, Nil, StatementList(xs)(node.span))(
              node.span.spanStart(s"${node.name.text}<body>")
            )
          )
          classBodyAsts.prependAll(fakeBodyAst)
      }

    val fields = node match {
      case classDecl: ClassDeclaration   => classDecl.fields
      case moduleDecl: ModuleDeclaration => moduleDecl.fields
      case _                             => Seq.empty
    }
    val (fieldTypeMemberNodes, fieldSingletonMemberNodes) = fields
      .map { x =>
        val name = code(x)
        x.isInstanceOf[InstanceFieldIdentifier] -> Ast(memberNode(x, name, name, Defines.Any))
      }
      .partition(_._1)

    scope.popScope()
    val prefixAst = createTypeRefPointer(typeDecl)
    val typeDeclAst = Ast(typeDecl)
      .withChildren(classModifiers)
      .withChildren(fieldTypeMemberNodes.map(_._2))
      .withChildren(classBodyAsts.toSeq)
    val singletonTypeDeclAst =
      Ast(singletonTypeDecl)
        .withChildren(singletonModifiers)
        .withChildren(fieldSingletonMemberNodes.map(_._2))
        .withChildren(singletonBodyAsts.toSeq)

    prefixAst :: typeDeclAst :: singletonTypeDeclAst :: Nil filterNot (_.root.isEmpty)
  }

  private def createTypeRefPointer(typeDecl: NewTypeDecl): Ast = {
    if (scope.isSurroundedByProgramScope) {
      // We aim to preserve whether it's a `class` or `module` in the `code` property
      val typeRefCode = s"${typeDecl.code.strip().takeWhile(_ != ' ')} ${typeDecl.name} (...)"
      val typeRefNode = Ast(
        NewTypeRef()
          .code(typeRefCode)
          .typeFullName(s"${typeDecl.fullName}<class>") // Everything will be dispatched on the singleton
          .lineNumber(typeDecl.lineNumber)
          .columnNumber(typeDecl.columnNumber)
      )

      val typeRefIdent = {
        val self = NewIdentifier().name(Defines.Self).code(Defines.Self).typeFullName(Defines.Any)
        val fi = NewFieldIdentifier()
          .code(typeDecl.name)
          .canonicalName(typeDecl.name)
          .lineNumber(typeDecl.lineNumber)
          .columnNumber(typeDecl.columnNumber)
        val fieldAccess = NewCall()
          .name(Operators.fieldAccess)
          .code(s"${Defines.Self}.${typeDecl.name}")
          .methodFullName(Operators.fieldAccess)
          .dispatchType(DispatchTypes.STATIC_DISPATCH)
          .typeFullName(Defines.Any)
        callAst(fieldAccess, Seq(Ast(self), Ast(fi)))
      }
      astForAssignment(typeRefIdent, typeRefNode, typeDecl.lineNumber, typeDecl.columnNumber)
    } else {
      Ast()
    }
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
