package io.joern.php2cpg.astcreation

import io.joern.php2cpg.astcreation.AstCreator.{NameConstants, TypeConstants}
import io.joern.php2cpg.parser.Domain.*
import io.joern.php2cpg.parser.Domain.PhpModifiers.containsAccessModifier
import io.joern.x2cpg.Defines.UnresolvedSignature
import io.joern.x2cpg.datastructures.AstParseLevel.{FULL_AST, SIGNATURES}
import io.joern.x2cpg.utils.AstPropertiesUtil.RootProperties
import io.joern.x2cpg.{Ast, Defines, ValidationMode}
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, EvaluationStrategies, ModifierTypes, PropertyNames}

trait AstForFunctionsCreator(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  protected def astForClosureExpr(closureExpr: PhpClosureExpr): Ast = {
    val methodName = scope.getScopedClosureName
    val methodRef  = methodRefNode(closureExpr, methodName, methodName, Defines.Any)

    val localsForUses = closureExpr.uses.flatMap { closureUse =>
      closureUse.variable match {
        case PhpVariable(PhpNameExpr(name, _), _) =>
          val typeFullName = scope
            .lookupVariable(name)
            .flatMap(_.properties.get(PropertyNames.TYPE_FULL_NAME).map(_.toString))
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
      val closureBindingId = s"$relativeFileName:$methodName:${local.name}"
      local.closureBindingId(closureBindingId)
      scope.addToScope(local.name, local)

      val closureBindingNode = NewClosureBinding()
        .closureBindingId(closureBindingId)
        .closureOriginalName(local.name)
        .evaluationStrategy(EvaluationStrategies.BY_SHARING)

      // The ref edge to the captured local is added in the ClosureRefPass
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
    val methodAst = astForMethodDecl(methodDecl, localsForUses.map(Ast(_)), Option(methodName))

    val usesCode = localsForUses match {
      case Nil    => ""
      case locals => s" use(${locals.map(_.code).mkString(", ")})"
    }
    methodAst.root.collect { case method: NewMethod => method }.foreach { methodNode =>
      methodNode.code(methodNode.code ++ usesCode)
    }

    // Add method to scope to be attached to typeDecl later
    scope.addAnonymousMethod(methodAst)

    Ast(methodRef)
  }

  protected def astForMethodDecl(
    decl: PhpMethodDecl,
    bodyPrefixAsts: List[Ast] = Nil,
    fullNameOverride: Option[String] = None,
    isConstructor: Boolean = false
  ): Ast = {
    val isStatic = decl.modifiers.contains(ModifierTypes.STATIC)
    val thisParam = if (decl.isClassMethod && !isStatic) {
      Option(thisParamAstForMethod(decl))
    } else {
      None
    }

    val methodName = decl.name.name
    val fullName   = fullNameOverride.getOrElse(composeMethodFullName(methodName, isStatic))

    val signature = s"$UnresolvedSignature(${decl.params.size})"

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
    val methodCode = s"${modifierString}function $methodName(${parameters.map(_.rootCodeOrEmpty).mkString(",")})"

    val method     = methodNode(decl, methodName, methodCode, fullName, Some(signature), relativeFileName)
    val isTopLevel = scope.isTopLevel

    scope.pushNewScope(method)

    val returnType = decl.returnType.map(_.name).getOrElse(Defines.Any)

    val methodBodyStmts = bodyPrefixAsts ++ decl.stmts.flatMap(astsForStmt)
    val methodReturn    = methodReturnNode(decl, returnType)

    val attributeAsts = decl.attributeGroups.flatMap(astForAttributeGroup)
    val methodBody = parseLevel match {
      case SIGNATURES if !isTopLevel => blockAst(blockNode(decl), Nil)
      case _                         => blockAst(blockNode(decl), methodBodyStmts)
    }

    scope.popScope()
    methodAstWithAnnotations(method, parameters, methodBody, methodReturn, modifiers, attributeAsts)
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
    val fieldInits = scope.getFieldInits
    astForMethodDecl(constructorDecl, fieldInits, isConstructor = true)
  }

  protected def defaultConstructorAst(originNode: PhpNode): Ast = {
    val fullName = composeMethodFullName(ConstructorMethodName, isStatic = false)

    val signature = s"$UnresolvedSignature(0)"

    val modifiers =
      List(ModifierTypes.VIRTUAL, ModifierTypes.PUBLIC, ModifierTypes.CONSTRUCTOR).map(modifierNode(originNode, _))

    val thisParam = thisParamAstForMethod(originNode)

    val method = methodNode(originNode, ConstructorMethodName, fullName, fullName, Some(signature), relativeFileName)

    val methodBody = blockAst(blockNode(originNode), scope.getFieldInits)

    val methodReturn = methodReturnNode(originNode, Defines.Any)

    methodAstWithAnnotations(method, thisParam :: Nil, methodBody, methodReturn, modifiers)
  }

  protected def astForAttributeGroup(attrGrp: PhpAttributeGroup): Seq[Ast] = {
    attrGrp.attrs.map(astForAttribute)
  }

  private def astForAttribute(attribute: PhpAttribute): Ast = {
    val name     = attribute.name
    val fullName = composeMethodFullName(name.name, true)
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
        val signature = s"${TypeConstants.Void}()"
        val fullName  = composeMethodFullName(Defines.StaticInitMethodName, isStatic = true)
        val ast =
          staticInitMethodAst(
            node,
            inits,
            fullName,
            Option(signature),
            TypeConstants.Void,
            fileName = Some(relativeFileName)
          )

        for {
          method  <- ast.root.collect { case method: NewMethod => method }
          content <- fileContent
        } {
          method.offset(0)
          method.offsetEnd(content.length)
        }
        Option(ast)
    }

  }

}
