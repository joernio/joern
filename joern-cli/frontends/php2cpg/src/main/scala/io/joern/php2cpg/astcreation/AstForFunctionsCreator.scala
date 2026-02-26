package io.joern.php2cpg.astcreation

import io.joern.php2cpg.astcreation.AstCreator.{NameConstants, TypeConstants}
import io.joern.php2cpg.parser.Domain
import io.joern.php2cpg.parser.Domain.*
import io.joern.php2cpg.parser.Domain.PhpModifiers.containsAccessModifier
import io.joern.php2cpg.utils.{BlockScope, MethodScope}
import io.joern.x2cpg.Defines.UnresolvedSignature
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
    val methodRef =
      methodRefNode(closureExpr, methodName, methodName, methodName).dynamicTypeHintFullName(methodName :: Nil)

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

      val closureBindingNode = createClosureBinding(closureBindingId)

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
    // Static closures exist, but this only affects captures. They are always called dynamically, so treat them
    // as class methods.
    val isClassMethod = true
    val methodDecl = PhpMethodDecl(
      name,
      closureExpr.params,
      modifiers,
      closureExpr.returnType,
      closureExpr.stmts,
      closureExpr.returnByRef,
      namespacedName = None,
      isClassMethod = isClassMethod,
      closureExpr.attributes,
      List.empty[PhpAttributeGroup]
    )

    val usesCode = localsForUses match {
      case Nil    => ""
      case locals => s" use(${locals.map(_.code).mkString(", ")})"
    }

    val methodAst =
      astForMethodDecl(
        methodDecl,
        localsForUses.map(Ast(_)),
        Option(methodName),
        usesCode = Option(usesCode),
        isArrowClosure = closureExpr.isArrowFunc,
        isAnonymousMethod = true,
        closureMethodRef = Option(methodRef)
      )

    // Add method to scope to be attached to typeDecl later
    scope.registerAstForInsertion(methodAst)

    Ast(methodRef)
  }

  protected def astForMethodDecl(
    decl: PhpMethodDecl,
    bodyPrefixAsts: List[Ast] = Nil,
    fullNameOverride: Option[String] = None,
    isConstructor: Boolean = false,
    usesCode: Option[String] = None,
    isArrowClosure: Boolean = false,
    isAnonymousMethod: Boolean = false,
    closureMethodRef: Option[NewMethodRef] = None
  ): Ast = {
    val isStatic = decl.modifiers.contains(ModifierTypes.STATIC)
    val thisParam =
      if (!isAnonymousMethod && decl.isClassMethod && !isStatic) Option(thisParamAstForMethod(decl)) else None

    val methodName = decl.name.name
    val fullName   = fullNameOverride.getOrElse(composeMethodFullName(methodName))

    val parameters = thisParam.toList ++ decl.params.zipWithIndex.map { case (param, idx) =>
      astForParam(param, idx + 1)
    }

    val constructorModifier   = Option.when(isConstructor)(ModifierTypes.CONSTRUCTOR)
    val virtualModifier       = Option.unless(isStatic || isConstructor)(ModifierTypes.VIRTUAL)
    val defaultAccessModifier = Option.unless(containsAccessModifier(decl.modifiers))(ModifierTypes.PUBLIC)

    val allModifiers      = virtualModifier ++: constructorModifier ++: defaultAccessModifier ++: decl.modifiers
    val modifiers         = allModifiers.map(modifierNode(decl, _))
    val excludedModifiers = Set(ModifierTypes.MODULE, ModifierTypes.LAMBDA)
    val modifierString = decl.modifiers.filterNot(excludedModifiers.contains) match {
      case Nil  => ""
      case mods => s"${mods.mkString(" ")} "
    }
    val methodCode =
      s"${modifierString}function $methodName(${parameters.filterNot(_.rootName.contains(NameConstants.This)).map(_.rootCodeOrEmpty).mkString(",")})${usesCode
          .getOrElse("")}"

    val methodRef =
      if (methodName == NamespaceTraversal.globalNamespaceName) None
      else if (isAnonymousMethod) closureMethodRef
      else Option(methodRefNode(decl, s"$methodCode", fullName, Defines.Any))
    val method = methodNode(decl, methodName, methodCode, fullName, None, relativeFileName)

    scope.surroundingScopeFullName.map(method.astParentFullName(_))
    scope.surroundingAstLabel.map(method.astParentType(_))

    val methodBodyNode = blockNode(decl)

    scope.pushNewScope(
      MethodScope(method, methodBodyNode, method.fullName, decl.params.map(_.name), methodRef, isArrowClosure)
    )
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

    if (decl.isClassMethod && !isStatic && !isConstructor) {
      val paramCopies = parameters.map(paramAst => (paramAst, paramAst.root)).collect {
        case (paramAst, Some(root: AstNodeNew)) => paramAst.subTreeCopy(root)
      }
      createAndPushTypeDeclForMethod(decl, method, isAnonymousMethod, paramCopies)
    }

    methodAst
  }

  /** Following PHP convention, callable object invocations are lowered to an explicit `__invoke` call in the CPG, e.g.
    * `$x(...)` is lowered to `$x.__invoke(...)`. To facilitate dataflow tracking through these methods (which could
    * either be lambda/anonymous functions, or regular functions assigned to objects via the first-class function
    * syntax), a type decl corresponding to the originally called method is created. A binding from the `__invoke`
    * method on this synthetic type to the originally called method is added to link to the correct method which should
    * actually be called.
    */
  private def createAndPushTypeDeclForMethod(
    decl: PhpMethodDecl,
    method: NewMethod,
    isAnonymousDecl: Boolean,
    parameters: List[Ast]
  ): Unit = {
    val methodTypeDecl = typeDeclNode(decl, method.name, method.fullName, method.filename, code = method.name)

    val binding = NewBinding().name(NameConstants.Invoke).signature("")
    val methodTypeDeclAst = Ast(methodTypeDecl)
      .withBindsEdge(methodTypeDecl, binding)
      .withRefEdge(binding, method)

    scope.registerAstForInsertion(methodTypeDeclAst)
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

  protected def defaultConstructorAst(originNode: PhpNode, fullNameOverride: Option[String] = None): Ast = {
    val attributes = originNode.attributes
    val defaultConstructorDecl = PhpMethodDecl(
      name = PhpNameExpr(Domain.ConstructorMethodName, attributes),
      params = Nil,
      modifiers = List(ModifierTypes.PUBLIC, ModifierTypes.CONSTRUCTOR),
      returnType = None,
      stmts = Nil,
      returnByRef = false,
      namespacedName = None,
      isClassMethod = true,
      attributes = attributes,
      attributeGroups = Nil
    )

    val initAsts = scope.getFieldInits.map { fieldInit =>
      astForMemberAssignment(fieldInit.originNode, fieldInit.memberNode, fieldInit.value, isField = true)
    }

    astForMethodDecl(defaultConstructorDecl, initAsts, isConstructor = true, fullNameOverride = fullNameOverride)
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

        val methodBlock = NewBlock().typeFullName(Defines.Any)

        scope.pushNewScope(MethodScope(methodNode_, methodBlock, fullName, methodRefNode = Option(methodRef)))

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
