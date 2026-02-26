package io.joern.swiftsrc2cpg.astcreation

import io.joern.swiftsrc2cpg.parser.SwiftNodeSyntax.*
import io.joern.x2cpg.AstNodeBuilder.dependencyNode
import io.joern.x2cpg.datastructures.Stack.*
import io.joern.x2cpg.datastructures.VariableScopeManager
import io.joern.x2cpg.frontendspecific.swiftsrc2cpg.Defines
import io.joern.x2cpg.{Ast, ValidationMode}
import io.shiftleft.codepropertygraph.generated.*
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal

import scala.annotation.unused

trait AstForDeclSyntaxCreator(implicit withSchemaValidation: ValidationMode) {
  this: AstCreator =>

  protected type TypeDeclLike = ClassDeclSyntax | ProtocolDeclSyntax | StructDeclSyntax | EnumDeclSyntax |
    ActorDeclSyntax | TypeAliasDeclSyntax | AssociatedTypeDeclSyntax

  protected type FunctionDeclLike = FunctionDeclSyntax | AccessorDeclSyntax | InitializerDeclSyntax |
    DeinitializerDeclSyntax | ClosureExprSyntax | SubscriptDeclSyntax

  private def astForAssociatedTypeDeclSyntax(node: AssociatedTypeDeclSyntax): Ast = {
    // TODO:
    // - handle genericWhereClause
    val attributes = astForDeclAttributes(node)
    val modifiers  = modifiersForDecl(node)
    val aliasName  = node.initializer.map(i => nameFromTypeSyntaxAst(i.value))

    val TypeInfo(typeName, typeFullName) = typeNameInfoForDeclSyntax(node)

    val typeDeclNode_ = typeDeclNode(node, typeName, typeFullName, parserResult.filename, code(node), alias = aliasName)

    attributes.foreach { ast =>
      Ast.storeInDiffGraph(ast, diffGraph)
      ast.root.foreach(r => diffGraph.addEdge(typeDeclNode_, r, EdgeTypes.AST))
    }

    modifiers.foreach { mod =>
      diffGraph.addEdge(typeDeclNode_, mod, EdgeTypes.AST)
    }

    createDeclConstructor(node, typeDeclNode_, List.empty)
    Ast(typeDeclNode_)
  }

  private def isConstructor(node: SwiftNode): Boolean = node match {
    case _: InitializerDeclSyntax => true
    case _                        => false
  }

  private def declMembers(decl: TypeDeclLike, withConstructor: Boolean = true): Seq[DeclSyntax] = {
    val memberBlock = decl match {
      case c: ClassDeclSyntax          => Option(c.memberBlock)
      case p: ProtocolDeclSyntax       => Option(p.memberBlock)
      case s: StructDeclSyntax         => Option(s.memberBlock)
      case e: EnumDeclSyntax           => Option(e.memberBlock)
      case a: ActorDeclSyntax          => Option(a.memberBlock)
      case _: TypeAliasDeclSyntax      => None
      case _: AssociatedTypeDeclSyntax => None
    }
    val allMembers = memberBlock.map(_.members.children.map(_.decl)).getOrElse(Seq.empty)
    if (withConstructor) {
      allMembers
    } else {
      allMembers.filterNot(isConstructor)
    }
  }

  private def isInitializedMember(node: DeclSyntax): Boolean = node match {
    case v: VariableDeclSyntax =>
      v.bindings.children.exists(c =>
        c.initializer.isDefined && !c.accessorBlock.exists(_.accessors.isInstanceOf[CodeBlockItemListSyntax])
      )
    case e: EnumCaseDeclSyntax => e.elements.children.exists(c => c.rawValue.isDefined)
    case _                     => false
  }

  protected def typeNameForDeclSyntax(node: DeclSyntax): String = {
    val name = node match {
      case d: ActorDeclSyntax          => code(d.name)
      case d: AssociatedTypeDeclSyntax => code(d.name)
      case d: ClassDeclSyntax          => code(d.name)
      case d: EnumDeclSyntax           => code(d.name)
      case d: ExtensionDeclSyntax      => code(d.extendedType)
      case d: FunctionDeclSyntax       => d.signature.returnClause.fold(Defines.Any)(c => code(c.`type`))
      case d: InitializerDeclSyntax    => d.signature.returnClause.fold(Defines.Any)(c => code(c.`type`))
      case d: MacroDeclSyntax          => d.signature.returnClause.fold(Defines.Any)(c => code(c.`type`))
      case d: MacroExpansionDeclSyntax => code(d.macroName)
      case d: ProtocolDeclSyntax       => code(d.name)
      case d: StructDeclSyntax         => code(d.name)
      case d: SubscriptDeclSyntax      => code(d.returnClause.`type`)
      case d: TypeAliasDeclSyntax      => code(d.name)
      case _                           => Defines.Any
    }
    AstCreatorHelper.cleanType(name)
  }

  private def isStaticMember(node: DeclSyntax): Boolean = node match {
    case d: AccessorDeclSyntax          => d.modifier.map(code).exists(_.contains("static"))
    case d: ActorDeclSyntax             => d.modifiers.children.map(c => code(c.name)).contains("static")
    case d: AssociatedTypeDeclSyntax    => d.modifiers.children.map(c => code(c.name)).contains("static")
    case d: ClassDeclSyntax             => d.modifiers.children.map(c => code(c.name)).contains("static")
    case d: DeinitializerDeclSyntax     => d.modifiers.children.map(c => code(c.name)).contains("static")
    case d: EditorPlaceholderDeclSyntax => d.modifiers.children.map(c => code(c.name)).contains("static")
    case d: EnumCaseDeclSyntax          => d.modifiers.children.map(c => code(c.name)).contains("static")
    case d: EnumDeclSyntax              => d.modifiers.children.map(c => code(c.name)).contains("static")
    case d: ExtensionDeclSyntax         => d.modifiers.children.map(c => code(c.name)).contains("static")
    case d: FunctionDeclSyntax          => d.modifiers.children.map(c => code(c.name)).contains("static")
    case _: IfConfigDeclSyntax          => false
    case d: ImportDeclSyntax            => d.modifiers.children.map(c => code(c.name)).contains("static")
    case d: InitializerDeclSyntax       => d.modifiers.children.map(c => code(c.name)).contains("static")
    case d: MacroDeclSyntax             => d.modifiers.children.map(c => code(c.name)).contains("static")
    case d: MacroExpansionDeclSyntax    => d.modifiers.children.map(c => code(c.name)).contains("static")
    case d: MissingDeclSyntax           => d.modifiers.children.map(c => code(c.name)).contains("static")
    case _: OperatorDeclSyntax          => false
    case _: PoundSourceLocationSyntax   => false
    case d: PrecedenceGroupDeclSyntax   => d.modifiers.children.map(c => code(c.name)).contains("static")
    case d: ProtocolDeclSyntax          => d.modifiers.children.map(c => code(c.name)).contains("static")
    case d: StructDeclSyntax            => d.modifiers.children.map(c => code(c.name)).contains("static")
    case d: SubscriptDeclSyntax         => d.modifiers.children.map(c => code(c.name)).contains("static")
    case d: TypeAliasDeclSyntax         => d.modifiers.children.map(c => code(c.name)).contains("static")
    case d: VariableDeclSyntax          => d.modifiers.children.map(c => code(c.name)).contains("static")
  }

  private def createFakeConstructor(
    node: TypeDeclLike,
    typeDeclNode: NewTypeDecl,
    methodBlockContent: List[DeclSyntax]
  ): Unit = {
    val constructorName = Defines.ConstructorMethodName
    val signature       = s"()->${typeDeclNode.fullName}"
    val methodFullName  = s"${typeDeclNode.fullName}.$constructorName:$signature"

    val methodNode_ =
      methodNode(node, constructorName, constructorName, methodFullName, Some(signature), parserResult.filename)
    val modifiers = Seq(NewModifier().modifierType(ModifierTypes.CONSTRUCTOR))

    val methodReturnNode_ = methodReturnNode(node, typeDeclNode.fullName)

    val blockNode = NewBlock()
    methodAstParentStack.push(methodNode_)
    scope.pushNewMethodScope(methodFullName, constructorName, blockNode, typeRefIdStack.headOption)
    localAstParentStack.push(blockNode)
    val methodBlockContentAsts = methodBlockContent.map(m => astForDeclMember(m, typeDeclNode))
    val parameterNode =
      parameterInNode(node, "self", "self", 0, false, EvaluationStrategies.BY_SHARING, typeDeclNode.fullName)
    scope.addVariable("self", parameterNode, typeDeclNode.fullName, VariableScopeManager.ScopeType.MethodScope)
    localAstParentStack.pop()
    methodAstParentStack.pop()
    scope.popScope()
    val mAst = methodAstWithAnnotations(
      methodNode_,
      Seq(Ast(parameterNode)),
      blockAst(blockNode, methodBlockContentAsts),
      methodReturnNode_,
      modifiers
    )

    val functionBindingAst = createFunctionBinding(methodNode_)
    Ast.storeInDiffGraph(mAst.merge(functionBindingAst), diffGraph)
    diffGraph.addEdge(typeDeclNode, methodNode_, EdgeTypes.AST)
  }

  private def declSyntaxFromIfConfigClauseSyntax(node: IfConfigClauseSyntax): Seq[DeclSyntax] = {
    node.elements match {
      case Some(value: CodeBlockItemListSyntax) =>
        value.children.collect { case elem if elem.item.isInstanceOf[DeclSyntax] => elem.item.asInstanceOf[DeclSyntax] }
      case Some(value: MemberBlockItemListSyntax) => value.children.map(_.decl)
      case _                                      => Seq.empty
    }
  }

  private def declSyntaxFromIfConfigDeclSyntax(node: IfConfigDeclSyntax): Seq[DeclSyntax] = {
    val children              = node.clauses.children
    val ifIfConfigClauses     = children.filter(c => code(c.poundKeyword) == "#if")
    val elseIfIfConfigClauses = children.filter(c => code(c.poundKeyword) == "#elseif")
    val elseIfConfigClauses   = children.filter(c => code(c.poundKeyword) == "#else")
    ifIfConfigClauses match {
      case Nil => Seq.empty
      case ifIfConfigClause :: Nil if ifConfigDeclConditionIsSatisfied(ifIfConfigClause) =>
        declSyntaxFromIfConfigClauseSyntax(ifIfConfigClause)
      case _ :: Nil =>
        val firstElseIfSatisfied = elseIfIfConfigClauses.find(ifConfigDeclConditionIsSatisfied)
        firstElseIfSatisfied match {
          case Some(elseIfIfConfigClause) => declSyntaxFromIfConfigClauseSyntax(elseIfIfConfigClause)
          case None =>
            elseIfConfigClauses match {
              case Nil                       => Seq.empty
              case elseIfConfigClause :: Nil => declSyntaxFromIfConfigClauseSyntax(elseIfConfigClause)
              case _                         => Seq.empty
            }
        }
      case _ => Seq.empty
    }
  }

  private def astForDeclMember(node: DeclSyntax, typeDeclNode: NewTypeDecl): Ast = {
    node match {
      case d: FunctionDeclLike =>
        val ast = astForFunctionLike(d, List.empty, None)
        Ast.storeInDiffGraph(ast, diffGraph)
        ast.root.foreach(r => diffGraph.addEdge(typeDeclNode, r, EdgeTypes.AST))
        Ast()
      case ifConf: IfConfigDeclSyntax =>
        val declElements = declSyntaxFromIfConfigDeclSyntax(ifConf)
        declElements.foldLeft(Ast()) { (ast, decl) => ast.merge(astForDeclMember(decl, typeDeclNode)) }
      case _: (ActorDeclSyntax | AssociatedTypeDeclSyntax | ClassDeclSyntax | EnumDeclSyntax | ExtensionDeclSyntax |
            ImportDeclSyntax | ProtocolDeclSyntax | StructDeclSyntax | MacroDeclSyntax | MacroExpansionDeclSyntax |
            OperatorDeclSyntax | PoundSourceLocationSyntax | PrecedenceGroupDeclSyntax | SubscriptDeclSyntax |
            TypeAliasDeclSyntax) =>
        val ast = astForNode(node)
        Ast.storeInDiffGraph(ast, diffGraph)
        ast.root.foreach(r => diffGraph.addEdge(typeDeclNode, r, EdgeTypes.AST))
        Ast()
      case d: EnumCaseDeclSyntax =>
        val ast = astForNode(d)
        d.elements.children.foreach { c =>
          val cCode          = code(c.name)
          val tpeFromTypeMap = fullnameProvider.typeFullname(c)
          val typeFullName   = tpeFromTypeMap.getOrElse(typeNameForDeclSyntax(d))
          val memberNode_    = memberNode(c, cCode, cCode, typeFullName)
          registerType(typeFullName)
          scope.addVariable(cCode, memberNode_, typeFullName, VariableScopeManager.ScopeType.TypeDeclScope)
          diffGraph.addEdge(typeDeclNode, memberNode_, EdgeTypes.AST)
        }
        ast
      case d: VariableDeclSyntax =>
        d.bindings.children.foreach { c =>
          val cCode          = code(c.pattern)
          val tpeFromTypeMap = fullnameProvider.typeFullname(c)
          val typeFullName = tpeFromTypeMap.getOrElse(
            c.typeAnnotation.map(t => AstCreatorHelper.cleanType(code(t.`type`))).getOrElse(Defines.Any)
          )
          registerType(typeFullName)
          val memberNode_ = memberNode(c, cCode, cCode, typeFullName)
          scope.addVariable(cCode, memberNode_, typeFullName, VariableScopeManager.ScopeType.TypeDeclScope)
          diffGraph.addEdge(typeDeclNode, memberNode_, EdgeTypes.AST)
        }
        astForVariableDeclSyntax(d, true)
      case other => notHandledYet(other)
    }
  }

  private def findDeclConstructor(decl: TypeDeclLike): Option[DeclSyntax] =
    declMembers(decl).find(isConstructor)

  private def createDeclConstructor(
    node: TypeDeclLike,
    typeDeclNode: NewTypeDecl,
    constructorContent: List[DeclSyntax]
  ): Unit =
    findDeclConstructor(node) match {
      case Some(constructor: InitializerDeclSyntax) =>
        val ast = astForFunctionLike(constructor, methodBlockContent = constructorContent, Some(typeDeclNode))
        Ast.storeInDiffGraph(ast, diffGraph)
        ast.root.foreach(r => diffGraph.addEdge(typeDeclNode, r, EdgeTypes.AST))
      case _ =>
        createFakeConstructor(node, typeDeclNode, methodBlockContent = constructorContent)
    }

  private def isClassMethodOrUninitializedMember(node: DeclSyntax): Boolean = {
    node match {
      case _: AccessorDeclSyntax                => true
      case _: InitializerDeclSyntax             => true
      case _: DeinitializerDeclSyntax           => true
      case _: FunctionDeclSyntax                => true
      case other if !isInitializedMember(other) => true
      case _                                    => false
    }
  }

  private def astForDeclAttributes(node: TypeDeclLike | VariableDeclSyntax): Seq[Ast] = {
    node match {
      case c: ClassDeclSyntax          => c.attributes.children.map(astForNode)
      case p: ProtocolDeclSyntax       => p.attributes.children.map(astForNode)
      case v: VariableDeclSyntax       => v.attributes.children.map(astForNode)
      case s: StructDeclSyntax         => s.attributes.children.map(astForNode)
      case e: EnumDeclSyntax           => e.attributes.children.map(astForNode)
      case a: ActorDeclSyntax          => a.attributes.children.map(astForNode)
      case t: TypeAliasDeclSyntax      => t.attributes.children.map(astForNode)
      case a: AssociatedTypeDeclSyntax => a.attributes.children.map(astForNode)
    }
  }

  private def createStaticConstructor(node: SwiftNode, inits: List[DeclSyntax], typeDeclNode: NewTypeDecl): Unit = {
    val constructorName = io.joern.x2cpg.Defines.StaticInitMethodName
    val signature       = s"()->${typeDeclNode.fullName}"
    val methodFullName  = s"${typeDeclNode.fullName}.$constructorName:$signature"

    val methodNode_ =
      methodNode(node, constructorName, constructorName, methodFullName, Some(signature), parserResult.filename)
    val modifiers =
      Seq(NewModifier().modifierType(ModifierTypes.CONSTRUCTOR), NewModifier().modifierType(ModifierTypes.STATIC))

    val blockNode = NewBlock()
    methodAstParentStack.push(methodNode_)
    scope.pushNewMethodScope(methodFullName, constructorName, blockNode, typeRefIdStack.headOption, true)
    localAstParentStack.push(blockNode)

    val initAsts = inits.map(m => astForDeclMember(m, typeDeclNode))

    scope.popScope()
    methodAstParentStack.pop()
    localAstParentStack.pop()

    val bodyAst           = blockAst(blockNode, initAsts)
    val methodReturnNode_ = methodReturnNode(node, typeDeclNode.fullName)
    val mAst              = methodAst(methodNode_, Nil, bodyAst, methodReturnNode_, modifiers)

    val functionBindingAst = createFunctionBinding(methodNode_)
    Ast.storeInDiffGraph(mAst.merge(functionBindingAst), diffGraph)
    diffGraph.addEdge(typeDeclNode, methodNode_, EdgeTypes.AST)
  }

  private def astForTypeDeclSyntax(node: TypeDeclLike): Ast = {
    // TODO:
    // - handle genericParameterClause
    // - handle genericWhereClause
    val attributes = astForDeclAttributes(node)
    val modifiers  = modifiersForDecl(node)
    val inherits   = inheritsFrom(node)

    val TypeInfo(typeName, typeFullName) = typeNameInfoForDeclSyntax(node)

    val typeDeclNode_ =
      typeDeclNode(node, typeName, typeFullName, parserResult.filename, code(node), inherits = inherits)

    attributes.foreach { ast =>
      Ast.storeInDiffGraph(ast, diffGraph)
      ast.root.foreach(r => diffGraph.addEdge(typeDeclNode_, r, EdgeTypes.AST))
    }

    modifiers.foreach { mod =>
      diffGraph.addEdge(typeDeclNode_, mod, EdgeTypes.AST)
    }

    val typeRefNode_ = typeRefNode(node, code(node), typeFullName)

    methodAstParentStack.push(typeDeclNode_)
    typeRefIdStack.push(typeRefNode_)
    scope.pushNewTypeDeclScope(typeName, typeFullName)
    scope.pushNewMethodScope(typeFullName, typeName, typeDeclNode_, None)

    val allClassMembers = declMembers(node, withConstructor = false).toList

    // adding all other members and retrieving their initialization calls
    val memberInits = allClassMembers.filter(m => !isStaticMember(m) && isInitializedMember(m))
    createDeclConstructor(node, typeDeclNode_, memberInits)

    // adding all static members and retrieving their initialization calls
    val staticMemberInits = allClassMembers.filter { member =>
      isStaticMember(member) && !isClassMethodOrUninitializedMember(member)
    }

    if (staticMemberInits.nonEmpty) {
      createStaticConstructor(node, staticMemberInits, typeDeclNode_)
    }

    // adding all class methods / functions and uninitialized members
    allClassMembers
      .filter(member => isClassMethodOrUninitializedMember(member))
      .foreach(m => astForDeclMember(m, typeDeclNode_))

    methodAstParentStack.pop()
    typeRefIdStack.pop()
    scope.popScope()
    scope.popScope()

    Ast.storeInDiffGraph(Ast(typeDeclNode_), diffGraph)
    diffGraph.addEdge(methodAstParentStack.head, typeDeclNode_, EdgeTypes.AST)

    Ast(typeRefNode_)
  }

  private def astForDeinitializerDeclSyntax(node: DeinitializerDeclSyntax): Ast = {
    astForNode(node)
  }

  private def astForEditorPlaceholderDeclSyntax(node: EditorPlaceholderDeclSyntax): Ast = {
    // EditorPlaceholderDeclSyntax node is not generated by the parser anymore.
    // Placeholders are represented by a MissingDeclSyntax.
    Ast()
  }

  private def astForEnumCaseDeclSyntax(node: EnumCaseDeclSyntax): Ast = {
    val bindingAsts = node.elements.children.map { binding =>
      val name       = code(binding.name)
      val nLocalNode = localNode(binding, name, name, Defines.Any).order(0)
      scope.addVariable(name, nLocalNode, Defines.Any, VariableScopeManager.ScopeType.BlockScope)
      diffGraph.addEdge(localAstParentStack.head, nLocalNode, EdgeTypes.AST)

      val initAsts = binding.rawValue.map(astForNode).toList
      if (initAsts.isEmpty) {
        Ast()
      } else {
        val attributeAsts = node.attributes.children.map(astForNode)
        val modifiers     = modifiersForDecl(node)

        val patternAst = astForNode(binding.name)
        modifiers.foreach { mod =>
          patternAst.root.foreach { r => diffGraph.addEdge(r, mod, EdgeTypes.AST) }
        }
        attributeAsts.foreach { attrAst =>
          Ast.storeInDiffGraph(attrAst, diffGraph)
          patternAst.root.foreach { r => attrAst.root.foreach { attr => diffGraph.addEdge(r, attr, EdgeTypes.AST) } }
        }
        createAssignmentCallAst(binding, patternAst, initAsts.head, code(binding).stripSuffix(","))
      }
    }

    bindingAsts match {
      case Nil         => Ast()
      case head :: Nil => head
      case _ =>
        val block = blockNode(node, code(node), Defines.Any)
        blockAst(block, bindingAsts.toList)
    }
  }

  private def inheritsFrom(node: TypeDeclLike | ExtensionDeclSyntax): Seq[String] = {
    val inheritFullNames = fullnameProvider.inheritsFor(node) match {
      case fullNames if fullNames.nonEmpty => fullNames
      case _ =>
        val clause = node match {
          case c: ClassDeclSyntax          => c.inheritanceClause
          case p: ProtocolDeclSyntax       => p.inheritanceClause
          case s: StructDeclSyntax         => s.inheritanceClause
          case e: EnumDeclSyntax           => e.inheritanceClause
          case a: ActorDeclSyntax          => a.inheritanceClause
          case a: AssociatedTypeDeclSyntax => a.inheritanceClause
          case e: ExtensionDeclSyntax      => e.inheritanceClause
          case _: TypeAliasDeclSyntax      => None
        }
        clause match {
          case Some(value) =>
            value.inheritedTypes.children.map(c => AstCreatorHelper.cleanType(code(c.`type`))).distinct.sorted
          case None => Seq.empty
        }
    }
    inheritFullNames.foreach(registerType)
    inheritFullNames
  }

  private def astForExtensionDeclSyntax(node: ExtensionDeclSyntax): Ast = {
    val TypeInfo(typeName, typeFullName) = typeNameInfoForDeclSyntax(node)
    val (extendedTypeName, extendedTypeFullName) = fullnameProvider.typeFullname(node) match {
      case Some(tpe) =>
        if (tpe.contains('.')) {
          val parts = tpe.split('.')
          (parts.last, tpe)
        } else {
          (typeName, tpe)
        }
      case None => (typeName, typeFullName)
    }

    val typeRefNode_ = typeRefNode(node, code(node), extendedTypeFullName)

    val inherits = inheritsFrom(node)
    if (inherits.nonEmpty) {
      global.addExtensionInherits(extendedTypeFullName, inherits)
    }

    typeRefIdStack.push(typeRefNode_)
    scope.pushNewTypeDeclScope(extendedTypeName, extendedTypeFullName)

    scope.restoreMembersForExtension(extendedTypeFullName)

    val memberBlock = node.memberBlock

    val members = memberBlock.members.children.map(_.decl).collect { case v: VariableDeclSyntax => v }.toList
    members.foreach { decl =>
      decl.bindings.children.foreach { binding =>
        val name           = code(binding.pattern)
        val cCode          = code(binding)
        val tpeFromTypeMap = fullnameProvider.typeFullname(binding)
        val typeFullName = tpeFromTypeMap.getOrElse(
          binding.typeAnnotation.map(t => AstCreatorHelper.cleanType(code(t.`type`))).getOrElse(Defines.Any)
        )
        registerType(typeFullName)
        global.addExtensionMember(extendedTypeFullName, name, cCode, typeFullName)

        binding.accessorBlock.map(_.accessors).collect {
          case accessorList: AccessorDeclListSyntax =>
            accessorList.children.foreach(astForAccessorInExtension(_, name, typeFullName))
          case block: CodeBlockItemListSyntax =>
            astForAccessorBlockInExtension(block, name, typeFullName, binding)
        }
      }
    }

    val functionDeclLikes = memberBlock.members.children.map(_.decl).collect { case f: FunctionDeclLike => f }.toList
    val functionDeclLikesAsts = functionDeclLikes.map(astForFunctionInExtension)

    typeRefIdStack.pop()
    scope.popScope()

    functionDeclLikesAsts.foreach { ast =>
      Ast.storeInDiffGraph(ast, diffGraph)
      ast.root.foreach(r => diffGraph.addEdge(methodAstParentStack.head, r, EdgeTypes.AST))
    }

    Ast(typeRefNode_)
  }

  private def modifiersForDecl(node: TypeDeclLike | EnumCaseDeclSyntax): Seq[NewModifier] = {
    val modifierList = node match {
      case c: ClassDeclSyntax          => c.modifiers.children
      case p: ProtocolDeclSyntax       => p.modifiers.children
      case s: StructDeclSyntax         => s.modifiers.children
      case e: EnumDeclSyntax           => e.modifiers.children
      case ec: EnumCaseDeclSyntax      => ec.modifiers.children
      case a: ActorDeclSyntax          => a.modifiers.children
      case t: TypeAliasDeclSyntax      => t.modifiers.children
      case a: AssociatedTypeDeclSyntax => a.modifiers.children
    }
    val modifiers = modifierList.flatMap(c => astForNode(c).root.map(_.asInstanceOf[NewModifier]))
    val allModifier = if (modifiers.isEmpty) {
      Seq(NewModifier().modifierType(ModifierTypes.PRIVATE))
    } else {
      modifiers
    }
    allModifier.zipWithIndex.map { case (m, index) =>
      m.order(index)
    }
  }

  private def modifiersForFunctionLike(node: FunctionDeclLike): Seq[NewModifier] = {
    val virtualModifier = Seq(NewModifier().modifierType(ModifierTypes.VIRTUAL))
    val modifiers = node match {
      case f: FunctionDeclSyntax =>
        f.modifiers.children.flatMap(c => astForNode(c).root.map(_.asInstanceOf[NewModifier]))
      case a: AccessorDeclSyntax => a.modifier.toSeq.flatMap(c => astForNode(c).root.map(_.asInstanceOf[NewModifier]))
      case i: InitializerDeclSyntax =>
        i.modifiers.children.flatMap(c => astForNode(c).root.map(_.asInstanceOf[NewModifier]))
      case d: DeinitializerDeclSyntax =>
        d.modifiers.children.flatMap(c => astForNode(c).root.map(_.asInstanceOf[NewModifier]))
      case s: SubscriptDeclSyntax =>
        s.modifiers.children.flatMap(c => astForNode(c).root.map(_.asInstanceOf[NewModifier]))
      case _: ClosureExprSyntax =>
        Seq(NewModifier().modifierType(ModifierTypes.LAMBDA))
    }
    val constructorModifier = if (isConstructor(node)) {
      Seq(NewModifier().modifierType(ModifierTypes.CONSTRUCTOR))
    } else { Seq.empty }
    (constructorModifier ++ virtualModifier ++ modifiers).zipWithIndex.map { case (m, index) =>
      m.order(index)
    }
  }

  case class AstAndMethod(ast: Ast, method: NewMethod, methodBlock: Ast)

  private def paramTypeString(node: FunctionParameterSyntax | ClosureParameterSyntax): String = {
    node match {
      case f: FunctionParameterSyntax =>
        val tpe   = AstCreatorHelper.cleanType(code(f.`type`))
        val label = code(f.firstName)
        s"$label:$tpe"
      case c: ClosureParameterSyntax =>
        val tpe   = c.`type`.fold(Defines.Any)(t => AstCreatorHelper.cleanType(code(t)))
        val label = code(c.firstName)
        s"$label:$tpe"
    }
  }

  protected def paramSignature(
    node: FunctionParameterClauseSyntax | ClosureShorthandParameterListSyntax | ClosureParameterClauseSyntax |
      AccessorParametersSyntax
  ): String = {
    node match {
      case f: FunctionParameterClauseSyntax =>
        f.parameters.children.map(paramTypeString).mkString("(", ",", ")")
      case c: ClosureParameterClauseSyntax =>
        c.parameters.children.map(paramTypeString).mkString("(", ",", ")")
      case c: ClosureShorthandParameterListSyntax =>
        c.children.map(_ => Defines.Any).mkString("(", ",", ")")
      case a: AccessorParametersSyntax => ""
    }
  }

  private def attributeAstsAndModifierForFunctionLike(node: FunctionDeclLike): (Seq[Ast], Seq[NewModifier]) = {
    val attributes = node match {
      case f: FunctionDeclSyntax      => f.attributes.children.map(astForNode)
      case a: AccessorDeclSyntax      => a.attributes.children.map(astForNode)
      case i: InitializerDeclSyntax   => i.attributes.children.map(astForNode)
      case d: DeinitializerDeclSyntax => d.attributes.children.map(astForNode)
      case s: SubscriptDeclSyntax     => s.attributes.children.map(astForNode)
      case c: ClosureExprSyntax =>
        val x = c.signature.map(s => s.attributes.children.map(astForNode))
        x.getOrElse(Seq.empty)
    }
    val modifiers = modifiersForFunctionLike(node)
    (attributes, modifiers)
  }

  protected def astForFunctionLike(
    node: FunctionDeclLike,
    methodBlockContent: List[DeclSyntax],
    typeDecl: Option[NewTypeDecl]
  ): Ast = {
    val (attributes, modifiers)                                       = attributeAstsAndModifierForFunctionLike(node)
    val filename                                                      = parserResult.filename
    val methodInfo                                                    = methodInfoForFunctionDeclLike(node)
    val MethodInfo(methodName, methodFullName, signature, returnType) = methodInfo
    val methodFullNameAndSignature                                    = methodInfo.fullNameAndSignature
    val isStatic = modifiers.exists(_.modifierType == ModifierTypes.STATIC)

    val shouldCreateFunctionReference = typeRefIdStack.isEmpty ||
      methodAstParentStack.headOption.exists(_.isInstanceOf[NewMethod]) ||
      node.isInstanceOf[ClosureExprSyntax] ||
      node.isInstanceOf[AccessorDeclSyntax]
    val methodRefNode_ = if (!shouldCreateFunctionReference) { None }
    else { Option(methodRefNode(node, methodName, methodFullNameAndSignature, methodFullNameAndSignature)) }
    val capturingRefNode = methodRefNode_.orElse(typeRefIdStack.headOption)

    val methodNode_ = methodNode(node, methodName, code(node), methodFullNameAndSignature, Option(signature), filename)
    val block       = blockNode(node, PropertyDefaults.Code, Defines.Any)

    methodAstParentStack.push(methodNode_)
    scope.pushNewMethodScope(methodFullNameAndSignature, methodName, block, capturingRefNode, isStatic)
    localAstParentStack.push(block)

    val (parameterAsts, bodyStmtAsts) = paramAndBodyAstsForFunctionLike(node)
    val methodReturnNode_             = methodReturnNode(node, returnType)

    val methodBlockContentAsts = methodBlockContent.map(m => astForDeclMember(m, typeDecl.get))
    val blockAst_              = blockAst(block, methodBlockContentAsts ++ bodyStmtAsts)
    val astForMethod = methodAstWithAnnotations(
      methodNode_,
      parameterAsts,
      blockAst_,
      methodReturnNode_,
      modifiers = modifiers,
      annotations = attributes
    )

    scope.popScope()
    localAstParentStack.pop()
    methodAstParentStack.pop()

    methodRefNode_ match {
      case Some(ref) =>
        createFunctionTypeAndTypeDecl(node, methodNode_)
        Ast.storeInDiffGraph(astForMethod, diffGraph)
        diffGraph.addEdge(methodAstParentStack.head, methodNode_, EdgeTypes.AST)
        Ast(ref)
      case None =>
        val functionBindingAst = createFunctionBinding(methodNode_)
        astForMethod.merge(functionBindingAst)
    }
  }

  private def paramAndBodyAstsForFunctionLike(node: FunctionDeclLike): (Seq[Ast], Seq[Ast]) = {
    val selfTpe = fullNameOfEnclosingTypeDecl()

    val parameterAsts = node match {
      case f: FunctionDeclSyntax =>
        val selfAst =
          if (!isStaticMember(f) && !fullNameOfEnclosingTypeDecl().endsWith(NamespaceTraversal.globalNamespaceName)) {
            val parameterNode =
              parameterInNode(node, "self", "self", 0, false, EvaluationStrategies.BY_SHARING, selfTpe)
            scope.addVariable("self", parameterNode, selfTpe, VariableScopeManager.ScopeType.MethodScope)
            Seq(Ast(parameterNode))
          } else Seq.empty
        selfAst ++ f.signature.parameterClause.parameters.children.map(astForNode)
      case a: AccessorDeclSyntax =>
        val parameterNode =
          parameterInNode(node, "self", "self", 0, false, EvaluationStrategies.BY_SHARING, selfTpe)
        scope.addVariable("self", parameterNode, selfTpe, VariableScopeManager.ScopeType.MethodScope)
        Ast(parameterNode) +: a.parameters.toSeq.map(astForNode)
      case i: InitializerDeclSyntax =>
        val parameterNode =
          parameterInNode(node, "self", "self", 0, false, EvaluationStrategies.BY_SHARING, selfTpe)
        scope.addVariable("self", parameterNode, selfTpe, VariableScopeManager.ScopeType.MethodScope)
        Ast(parameterNode) +: i.signature.parameterClause.parameters.children.map(astForNode)
      case _: DeinitializerDeclSyntax =>
        val parameterNode =
          parameterInNode(node, "self", "self", 0, false, EvaluationStrategies.BY_SHARING, selfTpe)
        scope.addVariable("self", parameterNode, selfTpe, VariableScopeManager.ScopeType.MethodScope)
        Seq(Ast(parameterNode))
      case s: SubscriptDeclSyntax =>
        val parameterNode =
          parameterInNode(node, "self", "self", 0, false, EvaluationStrategies.BY_SHARING, selfTpe)
        scope.addVariable("self", parameterNode, selfTpe, VariableScopeManager.ScopeType.MethodScope)
        Ast(parameterNode) +: s.parameterClause.parameters.children.map(astForNode)
      case c: ClosureExprSyntax =>
        c.signature.flatMap(_.parameterClause) match {
          case Some(p: ClosureShorthandParameterListSyntax) => p.children.map(astForNode)
          case Some(p: ClosureParameterClauseSyntax)        => p.parameters.children.map(astForNode)
          case None                                         => Seq.empty
        }
    }

    val body: Option[CodeBlockSyntax | AccessorDeclListSyntax | CodeBlockItemListSyntax] = node match {
      case f: FunctionDeclSyntax      => f.body
      case a: AccessorDeclSyntax      => a.body
      case i: InitializerDeclSyntax   => i.body
      case d: DeinitializerDeclSyntax => d.body
      case s: SubscriptDeclSyntax =>
        s.accessorBlock.map(_.accessors match {
          case l: AccessorDeclListSyntax  => l
          case l: CodeBlockItemListSyntax => l
        })
      case c: ClosureExprSyntax => Option(c.statements)
    }

    val bodyStmtAsts = body match {
      case Some(bodyNode: AccessorDeclListSyntax) =>
        // TODO: handle accessors in subscript functions
        bodyNode.children.toList.map(astForNode)
      case Some(bodyNode: CodeBlockSyntax) =>
        bodyNode.statements.children.toList match {
          case Nil => List.empty[Ast]
          case head :: Nil if head.item.isInstanceOf[ClosureExprSyntax] =>
            val retCode = code(head)
            List(returnAst(returnNode(head, retCode), List(astForNode(head.item))))
          case children =>
            astsForBlockElements(children)
        }
      case Some(bodyNode: CodeBlockItemListSyntax) =>
        bodyNode.children.toList match {
          case Nil => List.empty[Ast]
          case head :: Nil if !head.item.isInstanceOf[ReturnStmtSyntax] =>
            val retCode = code(head)
            List(returnAst(returnNode(head, retCode), List(astForNode(head.item))))
          case children =>
            astsForBlockElements(children)
        }
      case None =>
        List.empty[Ast]
    }

    (parameterAsts, bodyStmtAsts)
  }

  private def astForFunctionInExtension(node: FunctionDeclLike): Ast = {
    val (attributes, modifiers)                                       = attributeAstsAndModifierForFunctionLike(node)
    val filename                                                      = parserResult.filename
    val methodInfo                                                    = methodInfoForFunctionDeclLike(node)
    val MethodInfo(methodName, methodFullName, signature, returnType) = methodInfo
    val methodFullNameAndSignature                                    = methodInfo.fullNameAndSignature
    val methodFullNameAndSignatureExt                                 = methodInfo.fullNameAndSignatureExt
    val isStatic = modifiers.exists(_.modifierType == ModifierTypes.STATIC)

    global.addExtensionMethodFullName(methodFullNameAndSignature, methodFullNameAndSignatureExt)

    val capturingRefNode = typeRefIdStack.headOption
    val methodNode_ =
      methodNode(node, methodName, code(node), methodFullNameAndSignatureExt, Option(signature), filename)
    val block = blockNode(node, PropertyDefaults.Code, Defines.Any)

    methodAstParentStack.push(methodNode_)
    scope.pushNewMethodScope(methodFullNameAndSignatureExt, methodName, block, capturingRefNode, isStatic)
    localAstParentStack.push(block)

    val (parameterAsts, bodyStmtAsts) = paramAndBodyAstsForFunctionLike(node)

    scope.popScope()
    localAstParentStack.pop()
    methodAstParentStack.pop()

    methodAstWithAnnotations(
      methodNode_,
      parameterAsts,
      blockAst(block, bodyStmtAsts.toList),
      methodReturnNode(node, returnType),
      modifiers = modifiers,
      annotations = attributes
    )
  }

  private def astForFunctionDeclSyntax(node: FunctionDeclSyntax): Ast = {
    astForFunctionLike(node, List.empty, None)
  }

  protected def ifConfigDeclConditionIsSatisfied(node: IfConfigClauseSyntax): Boolean = {
    node.condition.isEmpty
    || definedSymbols.get(code(node.condition.get)).exists(_.toLowerCase == "true")
    || definedSymbols.get(code(node.condition.get)).contains("1")
  }

  protected def astForIfConfigDeclSyntax(node: IfConfigDeclSyntax): Ast = {
    val children              = node.clauses.children
    val ifIfConfigClauses     = children.filter(c => code(c.poundKeyword) == "#if")
    val elseIfIfConfigClauses = children.filter(c => code(c.poundKeyword) == "#elseif")
    val elseIfConfigClauses   = children.filter(c => code(c.poundKeyword) == "#else")
    ifIfConfigClauses match {
      case Nil => notHandledYet(node)
      case ifIfConfigClause :: Nil if ifConfigDeclConditionIsSatisfied(ifIfConfigClause) =>
        ifIfConfigClause.elements.fold(Ast())(astForNode)
      case _ :: Nil =>
        val firstElseIfSatisfied = elseIfIfConfigClauses.find(ifConfigDeclConditionIsSatisfied)
        firstElseIfSatisfied match {
          case Some(elseIfIfConfigClause) =>
            elseIfIfConfigClause.elements.fold(Ast())(astForNode)
          case None =>
            elseIfConfigClauses match {
              case Nil                       => Ast()
              case elseIfConfigClause :: Nil => elseIfConfigClause.elements.fold(Ast())(astForNode)
              case _                         => notHandledYet(node)
            }
        }
      case _ => notHandledYet(node)
    }
  }

  private def astForImportDeclSyntax(node: ImportDeclSyntax): Ast = {
    val importPath = node.path.children.map(c => code(c.name))
    val (name, groupName) = importPath match {
      case Nil         => (None, None)
      case elem :: Nil => (Option(elem), Option(elem))
      case _           => (importPath.lastOption, Option(importPath.slice(0, importPath.size - 1).mkString(".")))
    }
    if (name.isEmpty && groupName.isEmpty) {
      Ast()
    } else {
      val _dependencyNode = dependencyNode(name.get, groupName.get, "import")
      val importNode      = newImportNode(code(node), groupName.get, name.get, node)
      diffGraph.addNode(_dependencyNode)
      diffGraph.addEdge(importNode, _dependencyNode, EdgeTypes.IMPORTS)
      Ast(importNode)
    }
  }

  private def astForInitializerDeclSyntax(node: InitializerDeclSyntax): Ast = {
    astForNode(node)
  }

  private def astForMacroDeclSyntax(node: MacroDeclSyntax): Ast = notHandledYet(node)

  private def astForMacroExpansionDeclSyntax(node: MacroExpansionDeclSyntax): Ast = {
    val nodeCode = code(node.macroName)
    val fullName = fullnameProvider.declFullname(node).getOrElse(nodeCode)
    val tpe      = fullnameProvider.typeFullname(node).getOrElse(Defines.Any)
    registerType(tpe)

    val trailingClosureAsts            = node.trailingClosure.toList.map(astForNode)
    val additionalTrailingClosuresAsts = node.additionalTrailingClosures.children.map(c => astForNode(c.closure))

    val argAsts = astForNode(node.arguments) +: (trailingClosureAsts ++ additionalTrailingClosuresAsts)
    val callNode =
      NewCall()
        .name(nodeCode)
        .dispatchType(DispatchTypes.INLINED)
        .methodFullName(fullName)
        .code(code(node))
        .typeFullName(tpe)
        .lineNumber(line(node))
        .columnNumber(column(node))
    callAst(callNode, argAsts)
  }

  private def astForOperatorDeclSyntax(@unused node: OperatorDeclSyntax): Ast = Ast()

  private def astForPoundSourceLocationSyntax(node: PoundSourceLocationSyntax): Ast = notHandledYet(node)

  private def astForPrecedenceGroupDeclSyntax(@unused node: PrecedenceGroupDeclSyntax): Ast = Ast()

  private def astForSubscriptDeclSyntax(node: SubscriptDeclSyntax): Ast = {
    astForFunctionLike(node, List.empty, None)
  }

  private def nameFromTypeSyntaxAst(node: TypeSyntax): String = {
    astForTypeSyntax(node).root match {
      case Some(id: NewIdentifier) =>
        id.typeFullName
      case Some(typeDecl: NewTypeDecl) =>
        typeDecl.fullName
      case _ =>
        AstCreatorHelper.cleanType(code(node))
    }
  }

  private def astForTypeAliasDeclSyntax(node: TypeAliasDeclSyntax): Ast = {
    // TODO:
    // - handle genericParameterClause
    // - handle genericWhereClause
    val attributes = astForDeclAttributes(node)
    val modifiers  = modifiersForDecl(node)
    val aliasName  = nameFromTypeSyntaxAst(node.initializer.value)

    val TypeInfo(typeName, typeFullName) = typeNameInfoForDeclSyntax(node)

    val typeDeclNode_ =
      typeDeclNode(node, typeName, typeFullName, parserResult.filename, code(node), alias = Option(aliasName))

    attributes.foreach { ast =>
      Ast.storeInDiffGraph(ast, diffGraph)
      ast.root.foreach(r => diffGraph.addEdge(typeDeclNode_, r, EdgeTypes.AST))
    }

    modifiers.foreach { mod =>
      diffGraph.addEdge(typeDeclNode_, mod, EdgeTypes.AST)
    }

    createDeclConstructor(node, typeDeclNode_, List.empty)
    Ast(typeDeclNode_)
  }

  private def astForAccessor(node: AccessorDeclSyntax, variableName: String, tpe: String): Unit = {
    val attributes = node.attributes.children.map(astForNode)
    val modifiers  = modifiersForFunctionLike(node)

    val filename          = parserResult.filename
    val parameters        = node.parameters.toSeq
    val accessorSpecifier = code(node.accessorSpecifier)

    val methodInfo = methodInfoForAccessorDecl(node, variableName, tpe)
    val MethodInfo(methodName, methodFullName, signature, returnType) = methodInfo
    val methodFullNameAndSignature                                    = methodInfo.fullNameAndSignature
    val isStatic = modifiers.exists(_.modifierType == ModifierTypes.STATIC)

    global.addMemberPropertyFullName(methodFullName, methodFullNameAndSignature)

    val methodNode_ = methodNode(node, methodName, code(node), methodFullNameAndSignature, Option(signature), filename)
    val block       = blockNode(node, PropertyDefaults.Code, Defines.Any)

    val capturingRefNode = typeRefIdStack.headOption
    methodAstParentStack.push(methodNode_)
    scope.pushNewMethodScope(methodFullNameAndSignature, methodName, block, capturingRefNode)
    localAstParentStack.push(block)

    val selfTpe = fullNameOfEnclosingTypeDecl()
    val selfParameterNode =
      parameterInNode(node, "self", "self", 0, false, EvaluationStrategies.BY_SHARING, selfTpe)
    scope.addVariable("self", selfParameterNode, selfTpe, VariableScopeManager.ScopeType.MethodScope)
    val selfParameterNodeAst = Ast(selfParameterNode)

    val parameterAsts = if (parameters.isEmpty && accessorSpecifier == "set") {
      val name          = "newValue" // Swift default parameter name for set accessors
      val parameterNode = parameterInNode(node, name, name, 1, false, EvaluationStrategies.BY_VALUE, Some(tpe))
      scope.addVariable(name, parameterNode, parameterNode.typeFullName, VariableScopeManager.ScopeType.MethodScope)
      Seq(Ast(parameterNode))
    } else {
      parameters.map(astForNode)
    }

    val bodyStmtAsts = node.body
      .map { bodyNode =>
        bodyNode.statements.children.toList match {
          case Nil => List.empty[Ast]
          case head :: Nil if head.item.isInstanceOf[ClosureExprSyntax] =>
            val retCode = code(head)
            List(returnAst(returnNode(head, retCode), List(astForNode(head.item))))
          case children =>
            astsForBlockElements(children)
        }
      }
      .getOrElse(List.empty[Ast])

    val methodReturnNode_ = methodReturnNode(node, returnType)

    val blockAst_ = blockAst(block, bodyStmtAsts)
    val astForMethod =
      methodAstWithAnnotations(
        methodNode_,
        selfParameterNodeAst +: parameterAsts,
        blockAst_,
        methodReturnNode_,
        modifiers = modifiers,
        annotations = attributes
      )

    scope.popScope()
    localAstParentStack.pop()
    methodAstParentStack.pop()

    val functionBindingAst = createFunctionBinding(methodNode_)
    Ast.storeInDiffGraph(astForMethod.merge(functionBindingAst), diffGraph)
    diffGraph.addEdge(methodAstParentStack.head, methodNode_, EdgeTypes.AST)
  }

  private def astForAccessorBlock(
    node: CodeBlockItemListSyntax,
    variableName: String,
    tpe: String,
    binding: PatternBindingSyntax
  ): Unit = {
    val modifiers = Seq(NewModifier().modifierType(ModifierTypes.VIRTUAL))

    val filename = parserResult.filename

    val methodInfo = methodInfoForAccessorDecl(binding, variableName, tpe)
    val MethodInfo(methodName, methodFullName, signature, returnType) = methodInfo
    val methodFullNameAndSignature                                    = methodInfo.fullNameAndSignature
    global.addMemberPropertyFullName(methodFullName, methodFullNameAndSignature)

    val methodNode_ = methodNode(node, methodName, code(node), methodFullNameAndSignature, Option(signature), filename)
    val block       = blockNode(node, PropertyDefaults.Code, Defines.Any)

    val capturingRefNode = typeRefIdStack.headOption
    methodAstParentStack.push(methodNode_)
    scope.pushNewMethodScope(methodFullNameAndSignature, methodName, block, capturingRefNode)
    localAstParentStack.push(block)

    val selfTpe = fullNameOfEnclosingTypeDecl()
    val selfParameterNode =
      parameterInNode(node, "self", "self", 0, false, EvaluationStrategies.BY_SHARING, selfTpe)
    scope.addVariable("self", selfParameterNode, selfTpe, VariableScopeManager.ScopeType.MethodScope)
    val selfParameterNodeAst = Ast(selfParameterNode)

    val bodyStmtAsts = node.children.toList match {
      case Nil => List.empty[Ast]
      case head :: Nil if head.item.isInstanceOf[ClosureExprSyntax] =>
        val retCode = code(head)
        List(returnAst(returnNode(head, retCode), List(astForNode(head.item))))
      case children =>
        astsForBlockElements(children)
    }

    val methodReturnNode_ = methodReturnNode(node, returnType)

    val blockAst_ = blockAst(block, bodyStmtAsts)
    val astForMethod =
      methodAstWithAnnotations(
        methodNode_,
        Seq(selfParameterNodeAst),
        blockAst_,
        methodReturnNode_,
        modifiers = modifiers
      )

    scope.popScope()
    localAstParentStack.pop()
    methodAstParentStack.pop()

    val functionBindingAst = createFunctionBinding(methodNode_)
    Ast.storeInDiffGraph(astForMethod.merge(functionBindingAst), diffGraph)
    diffGraph.addEdge(methodAstParentStack.head, methodNode_, EdgeTypes.AST)
  }

  private def astForAccessorBlockInExtension(
    node: CodeBlockItemListSyntax,
    variableName: String,
    tpe: String,
    binding: PatternBindingSyntax
  ): Unit = {
    val modifiers = Seq(NewModifier().modifierType(ModifierTypes.VIRTUAL))

    val filename = parserResult.filename

    val methodInfo = methodInfoForAccessorDecl(binding, variableName, tpe)
    val MethodInfo(methodName, methodFullName, signature, returnType) = methodInfo
    val methodFullNameAndSignature                                    = methodInfo.fullNameAndSignature
    val methodFullNameAndSignatureExt                                 = methodInfo.fullNameAndSignatureExt
    global.addMemberPropertyFullName(methodFullName, methodFullNameAndSignatureExt)

    val methodNode_ =
      methodNode(node, methodName, code(node), methodFullNameAndSignatureExt, Option(signature), filename)
    val block = blockNode(node, PropertyDefaults.Code, Defines.Any)

    val capturingRefNode = typeRefIdStack.headOption
    methodAstParentStack.push(methodNode_)
    scope.pushNewMethodScope(methodFullNameAndSignatureExt, methodName, block, capturingRefNode)
    localAstParentStack.push(block)

    val selfTpe = fullNameOfEnclosingTypeDecl()
    val selfParameterNode =
      parameterInNode(node, "self", "self", 0, false, EvaluationStrategies.BY_SHARING, selfTpe)
    scope.addVariable("self", selfParameterNode, selfTpe, VariableScopeManager.ScopeType.MethodScope)
    val selfParameterNodeAst = Ast(selfParameterNode)

    val bodyStmtAsts = node.children.toList match {
      case Nil => List.empty[Ast]
      case head :: Nil if head.item.isInstanceOf[ClosureExprSyntax] =>
        val retCode = code(head)
        List(returnAst(returnNode(head, retCode), List(astForNode(head.item))))
      case children =>
        astsForBlockElements(children)
    }

    val methodReturnNode_ = methodReturnNode(node, returnType)

    val blockAst_ = blockAst(block, bodyStmtAsts)
    val astForMethod =
      methodAstWithAnnotations(
        methodNode_,
        Seq(selfParameterNodeAst),
        blockAst_,
        methodReturnNode_,
        modifiers = modifiers
      )

    scope.popScope()
    localAstParentStack.pop()
    methodAstParentStack.pop()

    Ast.storeInDiffGraph(astForMethod, diffGraph)
    diffGraph.addEdge(methodAstParentStack.head, methodNode_, EdgeTypes.AST)
  }

  private def astForAccessorInExtension(node: AccessorDeclSyntax, variableName: String, tpe: String): Unit = {
    val attributes = node.attributes.children.map(astForNode)
    val modifiers  = modifiersForFunctionLike(node)

    val filename          = parserResult.filename
    val parameters        = node.parameters.toSeq
    val accessorSpecifier = code(node.accessorSpecifier)

    val methodInfo = methodInfoForAccessorDecl(node, variableName, tpe)
    val MethodInfo(methodName, methodFullName, signature, returnType) = methodInfo
    val methodFullNameAndSignature                                    = methodInfo.fullNameAndSignature
    val methodFullNameAndSignatureExt                                 = methodInfo.fullNameAndSignatureExt

    global.addMemberPropertyFullName(methodFullName, methodFullNameAndSignatureExt)

    val capturingRefNode = typeRefIdStack.headOption
    val methodNode_ =
      methodNode(node, methodName, code(node), methodFullNameAndSignatureExt, Option(signature), filename)
    val block = blockNode(node, PropertyDefaults.Code, Defines.Any)

    methodAstParentStack.push(methodNode_)
    scope.pushNewMethodScope(methodFullNameAndSignatureExt, methodName, block, capturingRefNode)
    localAstParentStack.push(block)

    val selfTpe = fullNameOfEnclosingTypeDecl()
    val selfParameterNode =
      parameterInNode(node, "self", "self", 0, false, EvaluationStrategies.BY_SHARING, selfTpe)
    scope.addVariable("self", selfParameterNode, selfTpe, VariableScopeManager.ScopeType.MethodScope)
    val selfParameterNodeAst = Ast(selfParameterNode)

    val parameterAsts = if (parameters.isEmpty && accessorSpecifier == "set") {
      val name          = "newValue" // Swift default parameter name for set accessors
      val parameterNode = parameterInNode(node, name, name, 1, false, EvaluationStrategies.BY_VALUE, Some(tpe))
      scope.addVariable(name, parameterNode, parameterNode.typeFullName, VariableScopeManager.ScopeType.MethodScope)
      Seq(Ast(parameterNode))
    } else {
      parameters.map(astForNode)
    }

    val bodyStmtAsts = node.body
      .map { bodyNode =>
        bodyNode.statements.children.toList match {
          case Nil => List.empty[Ast]
          case head :: Nil if head.item.isInstanceOf[ClosureExprSyntax] =>
            val retCode = code(head)
            List(returnAst(returnNode(head, retCode), List(astForNode(head.item))))
          case children =>
            astsForBlockElements(children)
        }
      }
      .getOrElse(List.empty[Ast])

    val methodReturnNode_ = methodReturnNode(node, returnType)

    val blockAst_ = blockAst(block, bodyStmtAsts)
    val astForMethod =
      methodAstWithAnnotations(
        methodNode_,
        selfParameterNodeAst +: parameterAsts,
        blockAst_,
        methodReturnNode_,
        modifiers = modifiers,
        annotations = attributes
      )

    scope.popScope()
    localAstParentStack.pop()
    methodAstParentStack.pop()

    Ast.storeInDiffGraph(astForMethod, diffGraph)
    diffGraph.addEdge(methodAstParentStack.head, methodNode_, EdgeTypes.AST)
  }

  private def astForVariableDeclSyntax(variableDecl: VariableDeclSyntax, isTypeDeclMember: Boolean = false): Ast = {
    val kind = code(variableDecl.bindingSpecifier)
    val scopeType = if (kind == "let") { VariableScopeManager.ScopeType.BlockScope }
    else { VariableScopeManager.ScopeType.MethodScope }

    val bindingAsts = variableDecl.bindings.children.flatMap { binding =>
      val namesWithNode = binding.pattern match {
        case expr: ExpressionPatternSyntax =>
          notHandledYet(expr)
          Seq((code(expr), expr))
        case ident: IdentifierPatternSyntax =>
          Seq((code(ident.identifier), ident))
        case isType: IsTypePatternSyntax =>
          notHandledYet(isType)
          Seq((code(isType), isType))
        case missing: MissingPatternSyntax =>
          Seq((code(missing.placeholder), missing))
        case tuple: TuplePatternSyntax =>
          tuple.elements.children.map(c => (code(c.pattern), c))
        case valueBinding: ValueBindingPatternSyntax =>
          Seq((code(valueBinding.pattern), valueBinding))
        case w: WildcardPatternSyntax =>
          Seq((scopeLocalUniqueName("wildcard"), w))
      }

      namesWithNode.map { case (name, node) =>
        val cleanedName    = AstCreatorHelper.cleanName(name)
        val tpeFromTypeMap = fullnameProvider.typeFullname(node)
        val tpeFromAst     = binding.typeAnnotation.map(t => AstCreatorHelper.cleanType(code(t.`type`)))
        val typeFullName   = tpeFromTypeMap.orElse(tpeFromAst).getOrElse(Defines.Any)
        registerType(typeFullName)

        if (!isTypeDeclMember) {
          val nLocalNode = localNode(binding, cleanedName, cleanedName, typeFullName).order(0)
          scope.addVariable(cleanedName, nLocalNode, typeFullName, scopeType)
          diffGraph.addEdge(localAstParentStack.head, nLocalNode, EdgeTypes.AST)
        }

        binding.accessorBlock.map(_.accessors).collect {
          case accessorList: AccessorDeclListSyntax =>
            accessorList.children.foreach(astForAccessor(_, name, typeFullName))
          case block: CodeBlockItemListSyntax =>
            astForAccessorBlock(block, name, typeFullName, binding)
        }

        val initAsts = binding.initializer.map(astForNode).toSeq
        if (initAsts.isEmpty) {
          Ast()
        } else {
          val patternAst = if (!isTypeDeclMember) {
            val attributeAsts = variableDecl.attributes.children.map(astForNode)
            val modifiers =
              variableDecl.modifiers.children.flatMap(c => astForNode(c).root.map(_.asInstanceOf[NewModifier]))

            val patternIdentifier = identifierNode(binding.pattern, cleanedName).typeFullName(typeFullName)
            scope.addVariableReference(cleanedName, patternIdentifier, typeFullName, EvaluationStrategies.BY_REFERENCE)
            modifiers.foreach { mod =>
              diffGraph.addEdge(patternIdentifier, mod, EdgeTypes.AST)
            }
            attributeAsts.foreach { attrAst =>
              Ast.storeInDiffGraph(attrAst, diffGraph)
              attrAst.root.foreach { attr => diffGraph.addEdge(patternIdentifier, attr, EdgeTypes.AST) }
            }
            Ast(patternIdentifier)
          } else {
            val tpe = fullNameOfEnclosingTypeDecl()
            val selfNode = if (scope.isInStaticMethodScope) {
              typeRefNode(node, "Self", tpe)
            } else {
              val selfIdNode = identifierNode(node, "self", "self", tpe)
              scope.addVariableReference("self", selfIdNode, selfIdNode.typeFullName, EvaluationStrategies.BY_REFERENCE)
              selfIdNode
            }
            fieldAccessAst(node, node, Ast(selfNode), s"${selfNode.code}.$name", name, typeFullName)
          }

          val initCode = binding.initializer.fold("")(i => s" ${code(i).strip()}")
          val typeCode = binding.typeAnnotation.fold("")(t => code(t).strip())

          val rhsAst = initAsts match {
            case Nil         => Ast()
            case head :: Nil => head
            case others =>
              val block = blockNode(node, code(node), Defines.Any)
              blockAst(block, others.toList)
          }

          createAssignmentCallAst(binding, patternAst, rhsAst, s"$kind $cleanedName$typeCode$initCode".strip())
        }
      }
    }

    bindingAsts match {
      case Nil         => Ast()
      case head :: Nil => head
      case others =>
        val block = blockNode(variableDecl, code(variableDecl), Defines.Any)
        blockAst(block, others.toList)
    }
  }

  private def astForMissingDeclSyntax(@unused node: MissingDeclSyntax): Ast = Ast()

  protected def astForDeclSyntax(declSyntax: DeclSyntax): Ast = declSyntax match {
    case node: ActorDeclSyntax             => astForTypeDeclSyntax(node)
    case node: AssociatedTypeDeclSyntax    => astForAssociatedTypeDeclSyntax(node)
    case node: ClassDeclSyntax             => astForTypeDeclSyntax(node)
    case node: DeinitializerDeclSyntax     => astForDeinitializerDeclSyntax(node)
    case node: EditorPlaceholderDeclSyntax => astForEditorPlaceholderDeclSyntax(node)
    case node: EnumCaseDeclSyntax          => astForEnumCaseDeclSyntax(node)
    case node: EnumDeclSyntax              => astForTypeDeclSyntax(node)
    case node: ExtensionDeclSyntax         => astForExtensionDeclSyntax(node)
    case node: FunctionDeclSyntax          => astForFunctionDeclSyntax(node)
    case node: IfConfigDeclSyntax          => astForIfConfigDeclSyntax(node)
    case node: ImportDeclSyntax            => astForImportDeclSyntax(node)
    case node: InitializerDeclSyntax       => astForInitializerDeclSyntax(node)
    case node: MacroDeclSyntax             => astForMacroDeclSyntax(node)
    case node: MacroExpansionDeclSyntax    => astForMacroExpansionDeclSyntax(node)
    case node: MissingDeclSyntax           => astForMissingDeclSyntax(node)
    case node: OperatorDeclSyntax          => astForOperatorDeclSyntax(node)
    case node: PoundSourceLocationSyntax   => astForPoundSourceLocationSyntax(node)
    case node: PrecedenceGroupDeclSyntax   => astForPrecedenceGroupDeclSyntax(node)
    case node: ProtocolDeclSyntax          => astForTypeDeclSyntax(node)
    case node: StructDeclSyntax            => astForTypeDeclSyntax(node)
    case node: SubscriptDeclSyntax         => astForSubscriptDeclSyntax(node)
    case node: TypeAliasDeclSyntax         => astForTypeAliasDeclSyntax(node)
    case node: VariableDeclSyntax          => astForVariableDeclSyntax(node)
    case other                             => notHandledYet(other)
  }

}
