package io.joern.php2cpg.astcreation

import io.joern.php2cpg.astcreation.AstCreator.{NameConstants, TypeConstants}
import io.joern.php2cpg.parser.Domain.*
import io.joern.php2cpg.parser.Domain.PhpModifiers.containsAccessModifier
import io.joern.php2cpg.utils.MethodScope
import io.joern.x2cpg.utils.AstPropertiesUtil.RootProperties
import io.joern.x2cpg.{Ast, Defines, ValidationMode}
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.{
  EdgeTypes,
  EvaluationStrategies,
  ModifierTypes,
  PropertyDefaults,
  PropertyNames
}
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal

trait AstForFunctionsCreator(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  protected def astForClosureExpr(closureExpr: PhpClosureExpr): Ast = {
    val methodName = scope.getScopedClosureName
    val methodRef  = methodRefNode(closureExpr, methodName, methodName, Defines.Any)

    val localsForUses = closureExpr.uses.flatMap { closureUse =>
      closureUse.variable match {
        case PhpVariable(PhpNameExpr(name, _), _) =>
          val typeFullName = scope
            .lookupVariable(name)
            .flatMap(_.properties.get(PropertyNames.TypeFullName).map(_.toString))
            .getOrElse(Defines.Any)
          val byRefPrefix = if (closureUse.byRef) "&" else ""

          Some(localNode(closureExpr, name, s"$byRefPrefix$$$name", typeFullName))

        case other =>
          logger.warn(s"Found incorrect closure use variable '$other' in $relativeFileName")
          None
      }
    }

    // Add closure bindings to diffgraph
    localsForUses.foreach { local =>
      val closureBindingId = s"$methodName:${local.name}" // Filename already included in methodName

      local.closureBindingId(closureBindingId)

      val closureBindingNode = NewClosureBinding()
        .closureBindingId(closureBindingId)
        .evaluationStrategy(EvaluationStrategies.BY_SHARING)

      scope.lookupVariable(local.name) match {
        case Some(refLocal) =>
          diffGraph.addEdge(closureBindingNode, refLocal, EdgeTypes.REF)
        case _ =>
          logger.warn(s"Unable to find local variable for closure ref: ${local.name}")
      }

      scope.addToScope(local.name, local)
      diffGraph.addNode(closureBindingNode)
      diffGraph.addEdge(methodRef, closureBindingNode, EdgeTypes.CAPTURE)
    }

    // Create method for closure
    val name = PhpNameExpr(methodName, closureExpr.attributes)
    // TODO Check for static modifier
    val modifiers = ModifierTypes.LAMBDA :: (if (closureExpr.isStatic) ModifierTypes.STATIC :: Nil else Nil)
    val methodDecl = PhpMethodDecl(
      name,
      closureExpr.params,
      modifiers,
      closureExpr.returnType,
      closureExpr.stmts,
      closureExpr.returnByRef,
      namespacedName = None,
      isClassMethod = closureExpr.isStatic,
      closureExpr.attributes,
      List.empty[PhpAttributeGroup]
    )

    val usesCode = localsForUses match {
      case Nil    => ""
      case locals => s" use(${locals.map(_.code).mkString(", ")})"
    }

    val methodAst =
      astForMethodDecl(methodDecl, localsForUses.map(Ast(_)), Option(methodName), usesCode = Option(usesCode))

    // Add method to scope to be attached to typeDecl later
    scope.addAnonymousMethod(methodAst)

    Ast(methodRef)
  }

  protected def astForMethodDecl(
    decl: PhpMethodDecl,
    bodyPrefixAsts: List[Ast] = Nil,
    fullNameOverride: Option[String] = None,
    isConstructor: Boolean = false,
    usesCode: Option[String] = None
  ): Ast = {
    val isStatic = decl.modifiers.contains(ModifierTypes.STATIC)
    val thisParam = if (decl.isClassMethod && !isStatic) {
      Option(thisParamAstForMethod(decl))
    } else {
      None
    }

    val methodName = decl.name.name
    val fullName   = fullNameOverride.getOrElse(composeMethodFullName(methodName))

    val parameters = thisParam.toList ++ decl.params.zipWithIndex.map { case (param, idx) =>
      astForParam(param, idx + 1)
    }

    val constructorModifier   = Option.when(isConstructor)(ModifierTypes.CONSTRUCTOR)
    val defaultAccessModifier = Option.unless(containsAccessModifier(decl.modifiers))(ModifierTypes.PUBLIC)

    val allModifiers      = constructorModifier ++: defaultAccessModifier ++: decl.modifiers
    val modifiers         = allModifiers.map(modifierNode(decl, _))
    val excludedModifiers = Set(ModifierTypes.MODULE, ModifierTypes.LAMBDA)
    val modifierString = decl.modifiers.filterNot(excludedModifiers.contains) match {
      case Nil  => ""
      case mods => s"${mods.mkString(" ")} "
    }
    val methodCode =
      s"${modifierString}function $methodName(${parameters.map(_.rootCodeOrEmpty).mkString(",")})${usesCode.getOrElse("")}"

    val methodRef =
      if methodName == NamespaceTraversal.globalNamespaceName then None
      else Option(methodRefNode(decl, s"$methodCode", fullName, Defines.Any))
    val method = methodNode(decl, methodName, methodCode, fullName, None, relativeFileName)

    scope.surroundingScopeFullName.map(method.astParentFullName(_))
    scope.surroundingAstLabel.map(method.astParentType(_))

    val methodBodyNode = blockNode(decl)

    scope.pushNewScope(MethodScope(method, methodBodyNode, method.fullName, methodRef))
    scope.useFunctionDecl(methodName, fullName)

    val returnType = decl.returnType.map(_.name).getOrElse(Defines.Any)

    val fieldInitAsts = scope.getFieldInits.map { fieldInit =>
      astForMemberAssignment(fieldInit.originNode, fieldInit.memberNode, fieldInit.value, isField = true)
    }

    val methodBodyStmts = bodyPrefixAsts ++ fieldInitAsts ++ decl.stmts.flatMap(astsForStmt)
    val methodReturn    = methodReturnNode(decl, returnType)

    val attributeAsts = decl.attributeGroups.flatMap(astForAttributeGroup)
    val methodBody    = blockAst(methodBodyNode, methodBodyStmts)

    scope.popScope()
    val methodAst = methodAstWithAnnotations(method, parameters, methodBody, methodReturn, modifiers, attributeAsts)

    val methodRefAst = methodRef.map(Ast(_)).getOrElse(Ast())

    methodAst
  }

  private def thisParamAstForMethod(originNode: PhpNode): Ast = {
    val typeFullName = scope.getEnclosingTypeDeclTypeFullName.getOrElse(Defines.Any)

    val thisNode = parameterInNode(
      originNode,
      name = NameConstants.This,
      code = NameConstants.This,
      index = 0,
      isVariadic = false,
      evaluationStrategy = EvaluationStrategies.BY_SHARING,
      typeFullName = typeFullName
    ).dynamicTypeHintFullName(typeFullName :: Nil)
    // TODO Add dynamicTypeHintFullName to parameterInNode param list

    scope.addToScope(NameConstants.This, thisNode)

    Ast(thisNode)
  }

  protected def thisIdentifier(originNode: PhpNode): NewIdentifier = {
    val typ = scope.getEnclosingTypeDeclTypeName
    identifierNode(originNode, NameConstants.This, s"$$${NameConstants.This}", typ.getOrElse(Defines.Any), typ.toList)
  }

  protected def astForConstructor(constructorDecl: PhpMethodDecl): Ast = {
    astForMethodDecl(constructorDecl, isConstructor = true)
  }

  protected def defaultConstructorAst(originNode: PhpNode): Ast = {
    val fullName = composeMethodFullName(ConstructorMethodName)

    val modifiers =
      List(ModifierTypes.VIRTUAL, ModifierTypes.PUBLIC, ModifierTypes.CONSTRUCTOR).map(modifierNode(originNode, _))

    val thisParam = thisParamAstForMethod(originNode)

    val initAsts = scope.getFieldInits.map { fieldInit =>
      astForMemberAssignment(fieldInit.originNode, fieldInit.memberNode, fieldInit.value, isField = true)
    }

    val method    = methodNode(originNode, ConstructorMethodName, fullName, fullName, None, relativeFileName)
    val methodRef = methodRefNode(originNode, fullName, fullName, Defines.Any)

    val methodBodyBlock = blockNode(originNode)

    scope.surroundingScopeFullName.map(method.astParentFullName(_))
    scope.surroundingAstLabel.map(method.astParentType(_))

    scope.pushNewScope(MethodScope(method, methodBodyBlock, method.fullName, Option(methodRef)))

    val methodBody = blockAst(methodBodyBlock, initAsts)

    val methodReturn = methodReturnNode(originNode, Defines.Any)

    val methodAst = methodAstWithAnnotations(method, thisParam :: Nil, methodBody, methodReturn, modifiers)

    scope.popScope()

    methodAst
  }

  protected def astForAttributeGroup(attrGrp: PhpAttributeGroup): Seq[Ast] = {
    attrGrp.attrs.map(astForAttribute)
  }

  private def astForAttribute(attribute: PhpAttribute): Ast = {
    val name     = attribute.name
    val fullName = composeMethodFullName(name.name)
    val _annotationNode =
      annotationNode(attribute, code = name.name, attribute.name.name, fullName)
    val argsAst = attribute.args.map(astForCallArg)
    annotationAst(_annotationNode, argsAst)
  }

  private def astForParam(param: PhpParam, index: Int): Ast = {
    val evaluationStrategy =
      if (param.byRef)
        EvaluationStrategies.BY_REFERENCE
      else
        EvaluationStrategies.BY_VALUE

    val typeFullName = param.paramType.map(_.name).getOrElse(Defines.Any)

    val byRefCodePrefix = if (param.byRef) "&" else ""
    val code            = s"$byRefCodePrefix$$${param.name}"
    val paramNode = parameterInNode(param, param.name, code, index, param.isVariadic, evaluationStrategy, typeFullName)
    val attributeAsts = param.attributeGroups.flatMap(astForAttributeGroup)

    scope.addToScope(param.name, paramNode)

    Ast(paramNode).withChildren(attributeAsts)
  }

  protected def astForStaticAndConstInits(node: PhpNode): Option[Ast] = {
    scope.getConstAndStaticInits match {
      case Nil => None

      case inits =>
        val fullName = composeMethodFullName(Defines.StaticInitMethodName)
        val methodNode_ = methodNode(
          node,
          Defines.StaticInitMethodName,
          fullName,
          PropertyDefaults.Signature,
          Option(relativeFileName).getOrElse(PropertyDefaults.Filename)
        )

        val methodRef = methodRefNode(node, fullName, fullName, Defines.Any)

        val methodBlock = NewBlock()

        scope.pushNewScope(MethodScope(methodNode_, methodBlock, fullName, Option(methodRef)))

        val assignmentAsts = inits.map { init =>
          astForMemberAssignment(init.originNode, init.memberNode, init.value, isField = false)
        }

        val body = blockAst(methodBlock, assignmentAsts)

        val ast =
          staticInitMethodAst(node, methodNode_, body, TypeConstants.Void)

        for {
          method  <- ast.root.collect { case method: NewMethod => method }
          content <- fileContent
        } {
          method.offset(0)
          method.offsetEnd(content.length)
        }
        scope.popScope()
        Option(ast)
    }

  }

}
