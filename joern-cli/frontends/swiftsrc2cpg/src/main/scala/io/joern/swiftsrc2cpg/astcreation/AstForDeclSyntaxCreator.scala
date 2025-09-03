package io.joern.swiftsrc2cpg.astcreation

import io.joern.swiftsrc2cpg.parser.SwiftNodeSyntax.*
import io.joern.x2cpg.AstNodeBuilder.dependencyNode
import io.joern.x2cpg.datastructures.Stack.*
import io.joern.x2cpg.datastructures.VariableScopeManager
import io.joern.x2cpg.frontendspecific.swiftsrc2cpg.Defines
import io.joern.x2cpg.{Ast, ValidationMode}
import io.shiftleft.codepropertygraph.generated.*
import io.shiftleft.codepropertygraph.generated.nodes.*

import scala.annotation.unused

trait AstForDeclSyntaxCreator(implicit withSchemaValidation: ValidationMode) {
  this: AstCreator =>

  protected type TypeDeclLike = ClassDeclSyntax | ExtensionDeclSyntax | ProtocolDeclSyntax | StructDeclSyntax |
    EnumDeclSyntax | ActorDeclSyntax | TypeAliasDeclSyntax | AssociatedTypeDeclSyntax

  protected type FunctionDeclLike = FunctionDeclSyntax | AccessorDeclSyntax | InitializerDeclSyntax |
    DeinitializerDeclSyntax | ClosureExprSyntax | SubscriptDeclSyntax

  private def astForAccessorDeclSyntax(node: AccessorDeclSyntax): Ast = {
    astForNode(node)
  }

  private def astForActorDeclSyntax(node: ActorDeclSyntax): Ast = {
    astForTypeDeclSyntax(node)
  }

  private def astForAssociatedTypeDeclSyntax(node: AssociatedTypeDeclSyntax): Ast = {
    // TODO:
    // - handle genericWhereClause
    val attributes = astForDeclAttributes(node)
    val modifiers  = modifiersForDecl(node)
    val aliasName  = node.initializer.map(i => nameFromTypeSyntaxAst(i.value))

    val (astParentType, astParentFullName) = astParentInfo()
    val TypeInfo(typeName, typeFullName)   = typeNameInfoForDeclSyntax(node)

    val typeDeclNode_ = typeDeclNode(
      node,
      typeName,
      typeFullName,
      parserResult.filename,
      code(node),
      astParentType,
      astParentFullName,
      alias = aliasName
    )
    seenAliasTypes.add(typeDeclNode_)

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
      case e: ExtensionDeclSyntax      => Option(e.memberBlock)
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
    case v: VariableDeclSyntax => v.bindings.children.exists(c => c.initializer.isDefined || c.accessorBlock.isDefined)
    case e: EnumCaseDeclSyntax => e.elements.children.exists(c => c.rawValue.isDefined)
    case _                     => false
  }

  protected def typeNameForDeclSyntax(node: DeclSyntax): String = {
    val name = node match {
      case d: ActorDeclSyntax          => code(d.name)
      case d: AssociatedTypeDeclSyntax => code(d.name)
      case d: ClassDeclSyntax          => code(d.name)
      case d: EnumDeclSyntax           => code(d.name)
      case d: ExtensionDeclSyntax      => s"${code(d.extendedType)}<extension>"
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
    methodBlockContent: List[Ast] = List.empty
  ): Unit = {
    val constructorName = Defines.ConstructorMethodName
    val signature       = s"()->${typeDeclNode.fullName}"
    val methodFullName  = s"${typeDeclNode.fullName}.$constructorName:$signature"

    val methodNode_ =
      methodNode(node, constructorName, constructorName, methodFullName, Some(signature), parserResult.filename)
    val modifiers = Seq(NewModifier().modifierType(ModifierTypes.CONSTRUCTOR))

    methodAstParentStack.push(methodNode_)
    val methodReturnNode_ = methodReturnNode(node, typeDeclNode.fullName)

    methodAstParentStack.pop()

    val mAst = if (methodBlockContent.isEmpty) {
      methodStubAst(methodNode_, Seq.empty, methodReturnNode_, modifiers)
    } else {
      val bodyAst = blockAst(NewBlock(), methodBlockContent)
      methodAstWithAnnotations(methodNode_, Seq.empty, bodyAst, methodReturnNode_, modifiers)
    }

    val typeDeclAst = createFunctionTypeAndTypeDecl(methodNode_)
    Ast.storeInDiffGraph(mAst.merge(typeDeclAst), diffGraph)
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
        val ast = astForFunctionLike(d)
        ast.root.collect {
          case function: NewMethod =>
            val tpeFromTypeMap = fullnameProvider.typeFullname(d)
            val typeFullName   = tpeFromTypeMap.getOrElse(typeNameForDeclSyntax(d))
            val memberNode_    = memberNode(d, function.name, code(d), typeFullName, Seq(function.fullName))
            diffGraph.addEdge(typeDeclNode, memberNode_, EdgeTypes.AST)
          case methodRef: NewMethodRef =>
            val tpeFromTypeMap = fullnameProvider.typeFullname(d)
            val typeFullName   = tpeFromTypeMap.getOrElse(typeNameForDeclSyntax(d))
            val memberNode_    = memberNode(d, methodRef.code, code(d), typeFullName, Seq(methodRef.methodFullName))
            diffGraph.addEdge(typeDeclNode, memberNode_, EdgeTypes.AST)
        }
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
          diffGraph.addEdge(typeDeclNode, memberNode_, EdgeTypes.AST)
        }
        ast
      case d: VariableDeclSyntax =>
        val ast = astForNode(d)
        d.bindings.children.foreach { c =>
          val cCode          = code(c.pattern)
          val tpeFromTypeMap = fullnameProvider.typeFullname(c)
          val typeFullName   = tpeFromTypeMap.getOrElse(typeNameForDeclSyntax(d))
          val memberNode_    = memberNode(c, cCode, cCode, typeFullName)
          diffGraph.addEdge(typeDeclNode, memberNode_, EdgeTypes.AST)
        }
        ast
      case other => notHandledYet(other)
    }
  }

  private def findDeclConstructor(decl: TypeDeclLike): Option[DeclSyntax] =
    declMembers(decl).find(isConstructor)

  private def createDeclConstructor(
    node: TypeDeclLike,
    typeDeclNode: NewTypeDecl,
    constructorContent: List[Ast],
    constructorBlock: Ast = Ast()
  ): Unit =
    findDeclConstructor(node) match {
      case Some(constructor: InitializerDeclSyntax) =>
        val ast = astForFunctionLike(constructor, methodBlockContent = constructorContent)
        Ast.storeInDiffGraph(ast, diffGraph)
        ast.root.foreach(r => diffGraph.addEdge(typeDeclNode, r, EdgeTypes.AST))
      case _ if constructorBlock.root.isDefined =>
        constructorBlock.root.foreach { r =>
          constructorContent.foreach { c =>
            Ast.storeInDiffGraph(c, diffGraph)
            c.root.foreach(diffGraph.addEdge(r, _, EdgeTypes.AST))
          }
        }
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
      case t: ExtensionDeclSyntax      => t.attributes.children.map(astForNode)
      case a: AssociatedTypeDeclSyntax => a.attributes.children.map(astForNode)
    }
  }

  private def astForTypeDeclSyntax(node: TypeDeclLike): Ast = {
    // TODO:
    // - handle genericParameterClause
    // - handle genericWhereClause
    val attributes = astForDeclAttributes(node)
    val modifiers  = modifiersForDecl(node)
    val inherits   = inheritsFrom(node)

    val TypeInfo(typeName, typeFullName)   = typeNameInfoForDeclSyntax(node)
    val (astParentType, astParentFullName) = astParentInfo()

    val typeDeclNode_ = typeDeclNode(
      node,
      typeName,
      typeFullName,
      parserResult.filename,
      code(node),
      astParentType,
      astParentFullName,
      inherits = inherits
    )
    seenAliasTypes.add(typeDeclNode_)

    attributes.foreach { ast =>
      Ast.storeInDiffGraph(ast, diffGraph)
      ast.root.foreach(r => diffGraph.addEdge(typeDeclNode_, r, EdgeTypes.AST))
    }

    modifiers.foreach { mod =>
      diffGraph.addEdge(typeDeclNode_, mod, EdgeTypes.AST)
    }

    val typeRefNode_ = typeRefNode(node, code(node), typeFullName)
    methodAstParentStack.find(_.isInstanceOf[NewMethod]).foreach { node =>
      diffGraph.addEdge(node, typeRefNode_, EdgeTypes.AST)
    }

    methodAstParentStack.push(typeDeclNode_)
    dynamicInstanceTypeStack.push(typeFullName)
    typeRefIdStack.push(typeRefNode_)
    scope.pushNewMethodScope(typeFullName, typeName, typeDeclNode_, None)

    val allClassMembers = declMembers(node, withConstructor = false).toList

    // adding all other members and retrieving their initialization calls
    val memberInitCalls = allClassMembers
      .filter(m => !isStaticMember(m) && isInitializedMember(m))
      .map(m => astForDeclMember(m, typeDeclNode_))

    createDeclConstructor(node, typeDeclNode_, memberInitCalls)

    // adding all class methods / functions and uninitialized, non-static members
    allClassMembers
      .filter(member => isClassMethodOrUninitializedMember(member) && !isStaticMember(member))
      .foreach(m => astForDeclMember(m, typeDeclNode_))

    // adding all static members and retrieving their initialization calls
    val staticMemberInitCalls =
      allClassMembers.filter(isStaticMember).map(m => astForDeclMember(m, typeDeclNode_))

    methodAstParentStack.pop()
    dynamicInstanceTypeStack.pop()
    typeRefIdStack.pop()
    scope.popScope()

    if (staticMemberInitCalls.nonEmpty) {
      val init = staticInitMethodAstAndBlock(
        node,
        staticMemberInitCalls,
        s"$typeFullName.${io.joern.x2cpg.Defines.StaticInitMethodName}",
        None,
        Defines.Any
      )
      Ast.storeInDiffGraph(init.ast, diffGraph)
      diffGraph.addEdge(typeDeclNode_, init.method, EdgeTypes.AST)
    }

    Ast(typeDeclNode_)
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
    val attributeAsts = node.attributes.children.map(astForNode)
    val modifiers     = modifiersForDecl(node)

    val bindingAsts = node.elements.children.map { binding =>
      val name       = code(binding.name)
      val nLocalNode = localNode(binding, name, name, Defines.Any).order(0)
      scope.addVariable(name, nLocalNode, Defines.Any, VariableScopeManager.ScopeType.BlockScope)
      diffGraph.addEdge(localAstParentStack.head, nLocalNode, EdgeTypes.AST)

      val initAsts = binding.rawValue.map(astForNode).toList
      if (initAsts.isEmpty) {
        Ast()
      } else {
        val patternAst = astForNode(binding.name)
        modifiers.foreach { mod =>
          patternAst.root.foreach { r => diffGraph.addEdge(r, mod, EdgeTypes.AST) }
        }
        attributeAsts.foreach { attrAst =>
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

  private def inheritsFrom(node: TypeDeclLike): Seq[String] = {
    val clause = node match {
      case c: ClassDeclSyntax          => c.inheritanceClause
      case e: ExtensionDeclSyntax      => e.inheritanceClause
      case p: ProtocolDeclSyntax       => p.inheritanceClause
      case s: StructDeclSyntax         => s.inheritanceClause
      case e: EnumDeclSyntax           => e.inheritanceClause
      case a: ActorDeclSyntax          => a.inheritanceClause
      case a: AssociatedTypeDeclSyntax => a.inheritanceClause
      case _: TypeAliasDeclSyntax      => None
    }
    val inherits = clause match {
      case Some(value) =>
        value.inheritedTypes.children.map(c => AstCreatorHelper.cleanType(code(c.`type`))).distinct.sorted
      case None => Seq.empty
    }
    inherits.foreach(registerType)
    inherits
  }

  private def astForExtensionDeclSyntax(node: ExtensionDeclSyntax): Ast = {
    // TODO:
    // - handle genericParameterClause
    // - handle genericWhereClause
    val attributes = node.attributes.children.map(astForNode)
    val modifiers  = modifiersForDecl(node)
    val inherits   = inheritsFrom(node)

    val TypeInfo(typeName, typeFullName)   = typeNameInfoForDeclSyntax(node)
    val (astParentType, astParentFullName) = astParentInfo()

    val typeDeclNode_ = typeDeclNode(
      node,
      typeName,
      typeFullName,
      parserResult.filename,
      code(node),
      astParentType,
      astParentFullName,
      inherits = inherits
    )
    seenAliasTypes.add(typeDeclNode_)

    attributes.foreach { ast =>
      Ast.storeInDiffGraph(ast, diffGraph)
      ast.root.foreach(r => diffGraph.addEdge(typeDeclNode_, r, EdgeTypes.AST))
    }

    modifiers.foreach { mod =>
      diffGraph.addEdge(typeDeclNode_, mod, EdgeTypes.AST)
    }

    val typeRefNode_ = typeRefNode(node, code(node), typeFullName)
    methodAstParentStack.find(_.isInstanceOf[NewMethod]).foreach { node =>
      diffGraph.addEdge(node, typeRefNode_, EdgeTypes.AST)
    }

    methodAstParentStack.push(typeDeclNode_)
    dynamicInstanceTypeStack.push(typeFullName)
    typeRefIdStack.push(typeRefNode_)
    scope.pushNewMethodScope(typeFullName, typeName, typeDeclNode_, None)

    val allClassMembers = declMembers(node, withConstructor = false).toList

    // adding all other members and retrieving their initialization calls
    val memberInitCalls = allClassMembers
      .filter(m => !isStaticMember(m) && isInitializedMember(m))
      .map(m => astForDeclMember(m, typeDeclNode_))

    createDeclConstructor(node, typeDeclNode_, memberInitCalls)

    // adding all class methods / functions and uninitialized, non-static members
    allClassMembers
      .filter(member => isClassMethodOrUninitializedMember(member) && !isStaticMember(member))
      .foreach(m => astForDeclMember(m, typeDeclNode_))

    // adding all static members and retrieving their initialization calls
    val staticMemberInitCalls =
      allClassMembers.filter(isStaticMember).map(m => astForDeclMember(m, typeDeclNode_))

    methodAstParentStack.pop()
    dynamicInstanceTypeStack.pop()
    typeRefIdStack.pop()
    scope.popScope()

    if (staticMemberInitCalls.nonEmpty) {
      val init = staticInitMethodAstAndBlock(
        node,
        staticMemberInitCalls,
        s"$typeFullName.${io.joern.x2cpg.Defines.StaticInitMethodName}",
        None,
        Defines.Any
      )
      Ast.storeInDiffGraph(init.ast, diffGraph)
      diffGraph.addEdge(typeDeclNode_, init.method, EdgeTypes.AST)
    }

    Ast(typeDeclNode_)
  }

  private def modifiersForDecl(node: TypeDeclLike | EnumCaseDeclSyntax): Seq[NewModifier] = {
    val modifierList = node match {
      case c: ClassDeclSyntax          => c.modifiers.children
      case e: ExtensionDeclSyntax      => e.modifiers.children
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
      case _: ClosureExprSyntax => Seq.empty
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

  protected def astForFunctionLike(node: FunctionDeclLike, methodBlockContent: List[Ast] = List.empty): Ast = {
    // TODO: handle genericParameterClause
    // TODO: handle genericWhereClause

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
    val filename  = parserResult.filename

    val methodInfo                                                    = methodInfoForFunctionDeclLike(node)
    val MethodInfo(methodName, methodFullName, signature, returnType) = methodInfo
    val methodFullNameAndSignature                                    = methodInfo.fullNameAndSignature

    val shouldCreateFunctionReference =
      typeRefIdStack.isEmpty || node.isInstanceOf[ClosureExprSyntax] || node.isInstanceOf[AccessorDeclSyntax]
    val methodRefNode_ = if (!shouldCreateFunctionReference) { None }
    else { Option(methodRefNode(node, methodName, methodFullNameAndSignature, methodFullNameAndSignature)) }
    val capturingRefNode = methodRefNode_.orElse(typeRefIdStack.headOption)

    val methodNode_ = methodNode(node, methodName, code(node), methodFullNameAndSignature, Option(signature), filename)
    val block       = blockNode(node, PropertyDefaults.Code, Defines.Any)

    val parentFullName = astParentInfo()._2

    methodAstParentStack.push(methodNode_)
    scope.pushNewMethodScope(methodFullName, methodName, block, capturingRefNode)
    localAstParentStack.push(block)

    val parameterAsts = node match {
      case f: FunctionDeclSyntax =>
        f.signature.parameterClause.parameters.children.map(astForNode)
      case a: AccessorDeclSyntax =>
        a.parameters.toSeq.map(astForNode)
      case i: InitializerDeclSyntax =>
        val parameterNode =
          parameterInNode(node, "self", "self", 0, false, EvaluationStrategies.BY_SHARING, parentFullName)
        scope.addVariable("self", parameterNode, parentFullName, VariableScopeManager.ScopeType.MethodScope)
        Ast(parameterNode) +: i.signature.parameterClause.parameters.children.map(astForNode)
      case _: DeinitializerDeclSyntax =>
        val parameterNode =
          parameterInNode(node, "self", "self", 0, false, EvaluationStrategies.BY_SHARING, parentFullName)
        scope.addVariable("self", parameterNode, parentFullName, VariableScopeManager.ScopeType.MethodScope)
        Seq(Ast(parameterNode))
      case s: SubscriptDeclSyntax =>
        s.parameterClause.parameters.children.map(astForNode)
      case c: ClosureExprSyntax =>
        c.signature.flatMap(_.parameterClause) match
          case Some(p: ClosureShorthandParameterListSyntax) => p.children.map(astForNode)
          case Some(p: ClosureParameterClauseSyntax)        => p.parameters.children.map(astForNode)
          case None                                         => Seq.empty
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

    val methodReturnNode_ = methodReturnNode(node, returnType)

    val blockAst_ = blockAst(block, methodBlockContent ++ bodyStmtAsts)
    val astForMethod =
      methodAstWithAnnotations(
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
        val typeDeclAst = createFunctionTypeAndTypeDecl(methodNode_)
        astForMethod.merge(typeDeclAst)
    }
  }

  private def astForFunctionDeclSyntax(node: FunctionDeclSyntax): Ast = {
    astForFunctionLike(node)
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
    astForFunctionLike(node)
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

    val TypeInfo(typeName, typeFullName)   = typeNameInfoForDeclSyntax(node)
    val (astParentType, astParentFullName) = astParentInfo()

    val typeDeclNode_ = typeDeclNode(
      node,
      typeName,
      typeFullName,
      parserResult.filename,
      code(node),
      astParentType,
      astParentFullName,
      alias = Option(aliasName)
    )
    seenAliasTypes.add(typeDeclNode_)

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

    val methodNode_ = methodNode(node, methodName, code(node), methodFullNameAndSignature, Option(signature), filename)
    val block       = blockNode(node, PropertyDefaults.Code, Defines.Any)

    val capturingRefNode = typeRefIdStack.headOption
    methodAstParentStack.push(methodNode_)
    scope.pushNewMethodScope(methodFullName, methodName, block, capturingRefNode)
    localAstParentStack.push(block)

    val parameterAsts = if (parameters.isEmpty && accessorSpecifier == "set") {
      val name          = "newValue" // Swift default parameter name for set accessors
      val parameterNode = parameterInNode(node, name, name, 1, false, EvaluationStrategies.BY_VALUE, Some(tpe))
      scope.addVariable(name, parameterNode, Defines.Any, VariableScopeManager.ScopeType.MethodScope)
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

    val syntheticBodyStmtAst = if (bodyStmtAsts.isEmpty) {
      accessorSpecifier match {
        case "set" =>
          val thisNode      = identifierNode(node, "this")
          val fieldAccess   = fieldAccessAst(node, node, Ast(thisNode), s"this.$variableName", variableName, tpe)
          val sourceName    = parameterAsts.head.root.collect { case p: NewMethodParameterIn => p.name }.get
          val barIdentifier = identifierNode(node, sourceName).typeFullName(tpe)
          scope.addVariableReference(sourceName, barIdentifier, tpe, EvaluationStrategies.BY_REFERENCE)
          List(createAssignmentCallAst(node, fieldAccess, Ast(barIdentifier), s"this.$variableName = $sourceName"))
        case "get" =>
          val thisNode    = identifierNode(node, "this")
          val fieldAccess = fieldAccessAst(node, node, Ast(thisNode), s"this.$variableName", variableName, tpe)
          List(returnAst(returnNode(node, variableName), List(fieldAccess)))
        case _ => List.empty[Ast]
      }
    } else {
      bodyStmtAsts
    }

    val methodReturnNode_ = methodReturnNode(node, returnType)

    val blockAst_ = blockAst(block, syntheticBodyStmtAst)
    val astForMethod =
      methodAstWithAnnotations(
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

    val typeDeclAst = createFunctionTypeAndTypeDecl(methodNode_)
    Ast.storeInDiffGraph(astForMethod.merge(typeDeclAst), diffGraph)
    diffGraph.addEdge(methodAstParentStack.head, methodNode_, EdgeTypes.AST)
  }

  private def astForVariableDeclSyntax(node: VariableDeclSyntax): Ast = {
    val attributeAsts = node.attributes.children.map(astForNode)
    val modifiers     = node.modifiers.children.flatMap(c => astForNode(c).root.map(_.asInstanceOf[NewModifier]))
    val kind          = code(node.bindingSpecifier)
    val scopeType = if (kind == "let") { VariableScopeManager.ScopeType.BlockScope }
    else { VariableScopeManager.ScopeType.MethodScope }

    val bindingAsts = node.bindings.children.flatMap { binding =>
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
        val nLocalNode = localNode(binding, cleanedName, cleanedName, typeFullName).order(0)
        scope.addVariable(cleanedName, nLocalNode, typeFullName, scopeType)
        diffGraph.addEdge(localAstParentStack.head, nLocalNode, EdgeTypes.AST)

        val accessorBlocks = binding.accessorBlock.map(_.accessors).collect {
          case accessorList: AccessorDeclListSyntax =>
            accessorList.children.foreach(astForAccessor(_, name, typeFullName))
            None
          case other =>
            Some(astForNode(other))
        }

        val initAsts = (binding.initializer.map(astForNode) ++ accessorBlocks.flatten).toSeq
        if (initAsts.isEmpty) {
          Ast()
        } else {
          val patternIdentifier = identifierNode(binding.pattern, cleanedName).typeFullName(typeFullName)
          scope.addVariableReference(cleanedName, patternIdentifier, typeFullName, EvaluationStrategies.BY_REFERENCE)
          val patternAst = Ast(patternIdentifier)
          modifiers.foreach { mod =>
            diffGraph.addEdge(patternIdentifier, mod, EdgeTypes.AST)
          }
          attributeAsts.foreach { attrAst =>
            attrAst.root.foreach { attr => diffGraph.addEdge(patternIdentifier, attr, EdgeTypes.AST) }
          }
          val initCode          = binding.initializer.fold("")(i => s" ${code(i).strip()}")
          val accessorBlockCode = binding.accessorBlock.fold("")(a => s" ${code(a).strip()}")
          val typeCode          = binding.typeAnnotation.fold("")(t => code(t).strip())

          val rhsAst = initAsts match {
            case Nil         => Ast()
            case head :: Nil => head
            case others =>
              val block = blockNode(node, code(node), Defines.Any)
              blockAst(block, others.toList)
          }

          val assignmentAst = createAssignmentCallAst(
            binding,
            patternAst,
            rhsAst,
            s"$kind $cleanedName$typeCode$initCode$accessorBlockCode".strip()
          )
          assignmentAst
        }
      }
    }

    bindingAsts match {
      case Nil         => Ast()
      case head :: Nil => head
      case others =>
        val block = blockNode(node, code(node), Defines.Any)
        blockAst(block, others.toList)
    }
  }

  private def astForMissingDeclSyntax(@unused node: MissingDeclSyntax): Ast = Ast()

  protected def astForDeclSyntax(declSyntax: DeclSyntax): Ast = declSyntax match {
    case node: AccessorDeclSyntax          => astForAccessorDeclSyntax(node)
    case node: ActorDeclSyntax             => astForActorDeclSyntax(node)
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
  }

}
