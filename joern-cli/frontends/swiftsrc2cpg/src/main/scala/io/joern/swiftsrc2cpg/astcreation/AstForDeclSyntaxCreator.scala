package io.joern.swiftsrc2cpg.astcreation

import io.joern.swiftsrc2cpg.datastructures.BlockScope
import io.joern.swiftsrc2cpg.datastructures.MethodScope
import io.joern.swiftsrc2cpg.parser.SwiftNodeSyntax.*
import io.joern.swiftsrc2cpg.passes.Defines
import io.joern.x2cpg.Ast
import io.joern.x2cpg.utils.NodeBuilders.*
import io.joern.x2cpg.ValidationMode
import io.joern.x2cpg.datastructures.Stack.*
import io.shiftleft.codepropertygraph.generated.nodes.NewModifier
import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.ModifierTypes
import io.shiftleft.codepropertygraph.generated.nodes.NewBlock
import io.shiftleft.codepropertygraph.generated.nodes.NewCall
import io.shiftleft.codepropertygraph.generated.nodes.NewMethod
import io.shiftleft.codepropertygraph.generated.nodes.NewTypeDecl
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.codepropertygraph.generated.nodes.File.PropertyDefaults
import io.shiftleft.codepropertygraph.generated.nodes.NewIdentifier

import scala.annotation.unused

trait AstForDeclSyntaxCreator(implicit withSchemaValidation: ValidationMode) {
  this: AstCreator =>

  protected type TypeDeclLike = ClassDeclSyntax | ExtensionDeclSyntax | ProtocolDeclSyntax | StructDeclSyntax |
    EnumDeclSyntax | ActorDeclSyntax | TypeAliasDeclSyntax | AssociatedTypeDeclSyntax

  protected type FunctionDeclLike = FunctionDeclSyntax | AccessorDeclSyntax | InitializerDeclSyntax |
    DeinitializerDeclSyntax | ClosureExprSyntax | SubscriptDeclSyntax

  private def astForAccessorDeclSyntax(node: AccessorDeclSyntax): Ast = {
    astForNodeWithFunctionReference(node)
  }

  private def astForActorDeclSyntax(node: ActorDeclSyntax): Ast = {
    astForTypeDeclSyntax(node)
  }

  private def astForAssociatedTypeDeclSyntax(node: AssociatedTypeDeclSyntax): Ast = {
    // TODO:
    // - handle genericWhereClause
    val attributes = astForDeclAttributes(node)
    val modifiers  = modifiersForDecl(node)
    val aliasName  = node.initializer.map(i => handleTypeAliasInitializer(i.value))

    val name                               = typeNameForDeclSyntax(node)
    val (astParentType, astParentFullName) = astParentInfo()
    val (typeName, typeFullName)           = calcTypeNameAndFullName(name)
    registerType(typeFullName)

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

    diffGraph.addEdge(methodAstParentStack.head, typeDeclNode_, EdgeTypes.AST)

    createDeclConstructor(node, typeDeclNode_, List.empty)

    val typeRefNode_ = typeRefNode(node, code(node), typeFullName)
    Ast(typeRefNode_)
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

  private def typeNameForDeclSyntax(node: DeclSyntax): String = node match {
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
  ): AstAndMethod = {
    val constructorName = io.joern.x2cpg.Defines.ConstructorMethodName
    val methodFullName  = s"${typeDeclNode.fullName}:$constructorName"
    val methodNode_ = methodNode(node, constructorName, constructorName, methodFullName, None, parserResult.filename)
    val modifiers   = Seq(NewModifier().modifierType(ModifierTypes.CONSTRUCTOR))

    methodAstParentStack.push(methodNode_)
    val methodReturnNode =
      newMethodReturnNode(
        typeDeclNode.fullName,
        dynamicTypeHintFullName = None,
        line = line(node),
        column = column(node)
      )

    methodAstParentStack.pop()

    val functionTypeAndTypeDeclAst =
      createFunctionTypeAndTypeDeclAst(node, methodNode_, constructorName, methodFullName)

    val (mAst, bAst) = if (methodBlockContent.isEmpty) {
      (methodStubAst(methodNode_, Seq.empty, methodReturnNode, modifiers), Ast())
    } else {
      setArgumentIndices(methodBlockContent)
      val bodyAst = blockAst(NewBlock(), methodBlockContent)
      (methodAstWithAnnotations(methodNode_, Seq.empty, bodyAst, methodReturnNode, modifiers), bodyAst)
    }

    Ast.storeInDiffGraph(mAst, diffGraph)
    Ast.storeInDiffGraph(functionTypeAndTypeDeclAst, diffGraph)
    diffGraph.addEdge(methodAstParentStack.head, methodNode_, EdgeTypes.AST)

    AstAndMethod(Ast(), methodNode_, bAst)
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
    val typeFullName = typeNameForDeclSyntax(node)
    node match {
      case d: FunctionDeclLike =>
        val function    = astForFunctionLike(d).method
        val bindingNode = newBindingNode("", "", "")
        diffGraph.addEdge(typeDeclNode, bindingNode, EdgeTypes.BINDS)
        diffGraph.addEdge(bindingNode, function, EdgeTypes.REF)
        val memberNode_ = memberNode(d, function.name, code(d), typeFullName, Seq(function.fullName))
        diffGraph.addEdge(typeDeclNode, memberNode_, EdgeTypes.AST)
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
          val cCode       = code(c.name)
          val memberNode_ = memberNode(c, cCode, cCode, typeFullName)
          diffGraph.addEdge(typeDeclNode, memberNode_, EdgeTypes.AST)
        }
        ast
      case d: VariableDeclSyntax =>
        val ast = astForNode(d)
        d.bindings.children.foreach { c =>
          val cCode       = code(c.pattern)
          val memberNode_ = memberNode(c, cCode, cCode, typeFullName)
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
  ): Option[AstAndMethod] =
    findDeclConstructor(node) match {
      case Some(constructor: InitializerDeclSyntax) =>
        val result = astForFunctionLike(constructor, methodBlockContent = constructorContent)
        diffGraph.addEdge(result.method, NewModifier().modifierType(ModifierTypes.CONSTRUCTOR), EdgeTypes.AST)
        Option(result)
      case _ if constructorBlock.root.isDefined =>
        constructorBlock.root.foreach { r =>
          constructorContent.foreach { c =>
            Ast.storeInDiffGraph(c, diffGraph)
            c.root.foreach(diffGraph.addEdge(r, _, EdgeTypes.AST))
          }
        }
        None
      case _ =>
        Option(createFakeConstructor(node, typeDeclNode, methodBlockContent = constructorContent))
    }

  private def isClassMethodOrUninitializedMember(node: DeclSyntax): Boolean =
    node.isInstanceOf[AccessorDeclSyntax] ||
      node.isInstanceOf[InitializerDeclSyntax] ||
      node.isInstanceOf[DeinitializerDeclSyntax] ||
      node.isInstanceOf[FunctionDeclSyntax] ||
      !isInitializedMember(node)

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

    val name                     = typeNameForDeclSyntax(node)
    val (typeName, typeFullName) = calcTypeNameAndFullName(name)
    registerType(typeFullName)

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

    diffGraph.addEdge(methodAstParentStack.head, typeDeclNode_, EdgeTypes.AST)

    val typeRefNode_ = typeRefNode(node, code(node), typeFullName)

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
        staticMemberInitCalls,
        s"$typeFullName:${io.joern.x2cpg.Defines.StaticInitMethodName}",
        None,
        Defines.Any
      )
      Ast.storeInDiffGraph(init.ast, diffGraph)
      diffGraph.addEdge(typeDeclNode_, init.method, EdgeTypes.AST)
    }

    Ast(typeRefNode_)
  }

  private def astForDeinitializerDeclSyntax(node: DeinitializerDeclSyntax): Ast = {
    astForNodeWithFunctionReference(node)
  }

  private def astForEditorPlaceholderDeclSyntax(node: EditorPlaceholderDeclSyntax): Ast = notHandledYet(node)

  private def astForEnumCaseDeclSyntax(node: EnumCaseDeclSyntax): Ast = {
    val attributeAsts = node.attributes.children.map(astForNode)
    val modifiers     = modifiersForDecl(node)
    val scopeType     = BlockScope

    val bindingAsts = node.elements.children.map { binding =>
      val name       = code(binding.name)
      val nLocalNode = localNode(binding, name, name, Defines.Any).order(0)
      scope.addVariable(name, nLocalNode, scopeType)
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
        createAssignmentCallAst(
          patternAst,
          initAsts.head,
          code(binding).stripSuffix(","),
          line = line(binding),
          column = column(binding)
        )
      }
    }

    bindingAsts match {
      case Nil         => Ast()
      case head :: Nil => head
      case _ =>
        val block = blockNode(node, code(node), Defines.Any)
        setArgumentIndices(bindingAsts)
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
      case Some(value) => value.inheritedTypes.children.map(c => code(c.`type`)).distinct.sorted
      case None        => Seq.empty
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

    val name = typeNameForDeclSyntax(node)

    val (typeName, typeFullName) = calcTypeNameAndFullName(name)
    registerType(typeFullName)

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

    diffGraph.addEdge(methodAstParentStack.head, typeDeclNode_, EdgeTypes.AST)

    val typeRefNode_ = typeRefNode(node, code(node), typeFullName)

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
        staticMemberInitCalls,
        s"$typeFullName:${io.joern.x2cpg.Defines.StaticInitMethodName}",
        None,
        Defines.Any
      )
      Ast.storeInDiffGraph(init.ast, diffGraph)
      diffGraph.addEdge(typeDeclNode_, init.method, EdgeTypes.AST)
    }

    Ast(typeRefNode_)
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
    val virtualModifier = NewModifier().modifierType(ModifierTypes.VIRTUAL)
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
    (virtualModifier +: modifiers).zipWithIndex.map { case (m, index) =>
      m.order(index)
    }
  }

  case class AstAndMethod(ast: Ast, method: NewMethod, methodBlock: Ast)

  private def paramSignature(
    node: FunctionParameterClauseSyntax | ClosureShorthandParameterListSyntax | ClosureParameterClauseSyntax
  ): String = {
    node match {
      case f: FunctionParameterClauseSyntax =>
        f.parameters.children.map(c => code(c.`type`)).mkString("(", ",", ")")
      case c: ClosureParameterClauseSyntax =>
        c.parameters.children.map(c => c.`type`.fold(Defines.Any)(code)).mkString("(", ",", ")")
      case c: ClosureShorthandParameterListSyntax =>
        c.children.map(_ => Defines.Any).mkString("(", ",", ")")
    }
  }

  protected def astForFunctionLike(
    node: FunctionDeclLike,
    shouldCreateFunctionReference: Boolean = false,
    shouldCreateAssignmentCall: Boolean = false,
    methodBlockContent: List[Ast] = List.empty
  ): AstAndMethod = {
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

    val modifiers                    = modifiersForFunctionLike(node)
    val (methodName, methodFullName) = calcMethodNameAndFullName(node)
    val filename                     = parserResult.filename
    val (signature, returnType) = node match {
      case f: FunctionDeclSyntax =>
        val returnType = f.signature.returnClause.fold(Defines.Any)(c => code(c.`type`))
        (s"$returnType${paramSignature(f.signature.parameterClause)}", returnType)
      case a: AccessorDeclSyntax =>
        val returnType = Defines.Any
        (s"$returnType${a.parameters.fold("()")(code)}", returnType)
      case i: InitializerDeclSyntax =>
        val (_, returnType) = astParentInfo()
        (s"$returnType${paramSignature(i.signature.parameterClause)}", returnType)
      case _: DeinitializerDeclSyntax =>
        val returnType = Defines.Any
        (s"$returnType()", returnType)
      case s: SubscriptDeclSyntax =>
        val returnType = code(s.returnClause.`type`)
        (s"$returnType${paramSignature(s.parameterClause)}", returnType)
      case c: ClosureExprSyntax =>
        val returnType      = c.signature.flatMap(_.returnClause).fold(Defines.Any)(r => code(r.`type`))
        val paramClauseCode = c.signature.flatMap(_.parameterClause).fold("()")(paramSignature)
        (s"$returnType$paramClauseCode", returnType)
    }
    registerType(returnType)
    val methodFullNameAndSignature = s"$methodFullName:$signature"
    functionNodeToNameAndFullName(node) = (methodName, methodFullNameAndSignature)

    val methodRefNode_ = if (!shouldCreateFunctionReference) {
      None
    } else {
      Option(methodRefNode(node, methodName, methodFullNameAndSignature, methodFullNameAndSignature))
    }

    val callAst = if (shouldCreateAssignmentCall && shouldCreateFunctionReference) {
      val idNode  = identifierNode(node, methodName)
      val idLocal = localNode(node, methodName, methodName, methodFullNameAndSignature).order(0)
      diffGraph.addEdge(localAstParentStack.head, idLocal, EdgeTypes.AST)
      scope.addVariable(methodName, idLocal, BlockScope)
      scope.addVariableReference(methodName, idNode)
      val assignmentCode = s"func $methodName = ${code(node)}"
      val assignment     = createAssignmentCallAst(idNode, methodRefNode_.get, assignmentCode, line(node), column(node))
      assignment
    } else {
      Ast()
    }

    val capturingRefNode =
      if (shouldCreateFunctionReference) {
        methodRefNode_
      } else {
        typeRefIdStack.headOption
      }

    val codeString  = code(node)
    val methodNode_ = methodNode(node, methodName, codeString, methodFullNameAndSignature, Option(signature), filename)
    val block       = blockNode(node, PropertyDefaults.Code, Defines.Any)
    methodAstParentStack.push(methodNode_)
    scope.pushNewMethodScope(methodFullName, methodName, block, capturingRefNode)
    localAstParentStack.push(block)

    val parameterAsts = node match {
      case f: FunctionDeclSyntax =>
        f.signature.parameterClause.parameters.children.map(astForNode)
      case a: AccessorDeclSyntax =>
        a.parameters.toSeq.map(astForNode)
      case i: InitializerDeclSyntax =>
        i.signature.parameterClause.parameters.children.map(astForNode)
      case _: DeinitializerDeclSyntax =>
        Seq.empty
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
        bodyNode.children.toList match {
          case Nil      => List.empty[Ast]
          case children => children.map(astForNodeWithFunctionReferenceAndCall)
        }
      case Some(bodyNode: CodeBlockSyntax) =>
        bodyNode.statements.children.toList match {
          case Nil => List.empty[Ast]
          case head :: Nil if head.item.isInstanceOf[ClosureExprSyntax] =>
            val retCode = code(head)
            List(returnAst(returnNode(head, retCode), List(astForNodeWithFunctionReference(head.item))))
          case children =>
            astsForBlockElements(children)
        }
      case Some(bodyNode: CodeBlockItemListSyntax) =>
        bodyNode.children.toList match {
          case Nil => List.empty[Ast]
          case head :: Nil if !head.item.isInstanceOf[ReturnStmtSyntax] =>
            val retCode = code(head)
            List(returnAst(returnNode(head, retCode), List(astForNodeWithFunctionReference(head.item))))
          case children =>
            astsForBlockElements(children)
        }
      case None =>
        List.empty[Ast]
    }

    val methodReturnNode =
      newMethodReturnNode(returnType, dynamicTypeHintFullName = None, line = line(node), column = column(node))

    val bodyAsts = methodBlockContent ++ bodyStmtAsts
    setArgumentIndices(bodyAsts)
    val blockAst_ = blockAst(block, bodyAsts)
    val astForMethod =
      methodAstWithAnnotations(
        methodNode_,
        parameterAsts,
        blockAst_,
        methodReturnNode,
        modifiers = modifiers,
        annotations = attributes
      )

    scope.popScope()
    localAstParentStack.pop()
    methodAstParentStack.pop()

    val typeDeclAst = createFunctionTypeAndTypeDeclAst(node, methodNode_, methodName, methodFullNameAndSignature)
    Ast.storeInDiffGraph(astForMethod, diffGraph)
    Ast.storeInDiffGraph(typeDeclAst, diffGraph)
    diffGraph.addEdge(methodAstParentStack.head, methodNode_, EdgeTypes.AST)

    methodRefNode_ match {
      case Some(ref) if callAst.nodes.isEmpty => AstAndMethod(Ast(ref), methodNode_, blockAst_)
      case _                                  => AstAndMethod(callAst, methodNode_, blockAst_)
    }
  }

  private def astForFunctionDeclSyntax(node: FunctionDeclSyntax): Ast = {
    astForFunctionLike(node).ast
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
      val _dependencyNode = newDependencyNode(name.get, groupName.get, "import")
      val importNode      = newImportNode(code(node), groupName.get, name.get, node)
      diffGraph.addNode(_dependencyNode)
      diffGraph.addEdge(importNode, _dependencyNode, EdgeTypes.IMPORTS)
      Ast(importNode)
    }
  }

  private def astForInitializerDeclSyntax(node: InitializerDeclSyntax): Ast = {
    astForNodeWithFunctionReference(node)
  }

  private def astForMacroDeclSyntax(node: MacroDeclSyntax): Ast = notHandledYet(node)

  private def astForMacroExpansionDeclSyntax(node: MacroExpansionDeclSyntax): Ast = {
    val name = code(node.macroName)
    val argAsts = astForNode(node.arguments) +:
      node.trailingClosure.toList.map(astForNodeWithFunctionReference) :+
      astForNode(node.additionalTrailingClosures)
    val callNode =
      NewCall()
        .name(name)
        .dispatchType(DispatchTypes.INLINED)
        .methodFullName(name)
        .code(code(node))
        .typeFullName(Defines.Any)
        .lineNumber(line(node))
        .columnNumber(column(node))
    callAst(callNode, argAsts)
  }

  private def astForOperatorDeclSyntax(@unused node: OperatorDeclSyntax): Ast = Ast()

  private def astForPoundSourceLocationSyntax(node: PoundSourceLocationSyntax): Ast = notHandledYet(node)

  private def astForPrecedenceGroupDeclSyntax(@unused node: PrecedenceGroupDeclSyntax): Ast = Ast()

  private def astForSubscriptDeclSyntax(node: SubscriptDeclSyntax): Ast = notHandledYet(node)

  protected def handleTypeAliasInitializer(node: TypeSyntax): String = {
    astForTypeSyntax(node).root match
      case Some(id: NewIdentifier)     => id.name
      case Some(typeDecl: NewTypeDecl) => typeDecl.fullName
      case _                           => code(node)
  }

  private def astForTypeAliasDeclSyntax(node: TypeAliasDeclSyntax): Ast = {
    // TODO:
    // - handle genericParameterClause
    // - handle genericWhereClause
    val attributes = astForDeclAttributes(node)
    val modifiers  = modifiersForDecl(node)
    val aliasName  = handleTypeAliasInitializer(node.initializer.value)

    val name                     = typeNameForDeclSyntax(node)
    val (typeName, typeFullName) = calcTypeNameAndFullName(name)
    registerType(typeFullName)

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

    diffGraph.addEdge(methodAstParentStack.head, typeDeclNode_, EdgeTypes.AST)

    createDeclConstructor(node, typeDeclNode_, List.empty)

    val typeRefNode_ = typeRefNode(node, code(node), typeFullName)
    Ast(typeRefNode_)
  }

  private def astForVariableDeclSyntax(node: VariableDeclSyntax): Ast = {
    val attributeAsts = node.attributes.children.map(astForNode)
    val modifiers     = node.modifiers.children.flatMap(c => astForNode(c).root.map(_.asInstanceOf[NewModifier]))
    val kind          = code(node.bindingSpecifier)
    val scopeType = if (kind == "let") {
      BlockScope
    } else {
      MethodScope
    }

    val bindingAsts = node.bindings.children.map { binding =>
      val names = binding.pattern match {
        case expr: ExpressionPatternSyntax =>
          notHandledYet(expr)
          Seq(code(expr))
        case ident: IdentifierPatternSyntax =>
          Seq(code(ident.identifier))
        case isType: IsTypePatternSyntax =>
          notHandledYet(isType)
          Seq(code(isType))
        case missing: MissingPatternSyntax =>
          Seq(code(missing.placeholder))
        case tuple: TuplePatternSyntax =>
          tuple.elements.children.map(c => code(c.pattern))
        case valueBinding: ValueBindingPatternSyntax =>
          Seq(code(valueBinding.pattern))
        case _: WildcardPatternSyntax =>
          Seq(generateUnusedVariableName(usedVariableNames, "wildcard"))
      }

      names.map { name =>
        val typeFullName = binding.typeAnnotation.fold(Defines.Any)(t => code(t.`type`))
        registerType(typeFullName)
        val nLocalNode = localNode(binding, name, name, typeFullName).order(0)
        scope.addVariable(name, nLocalNode, scopeType)
        diffGraph.addEdge(localAstParentStack.head, nLocalNode, EdgeTypes.AST)

        val initAsts = binding.initializer.map(astForNode) ++ binding.accessorBlock.map(astForNode)
        if (initAsts.isEmpty) {
          Ast()
        } else {
          val patternIdentifier = identifierNode(binding.pattern, name).typeFullName(typeFullName)
          scope.addVariableReference(name, patternIdentifier)
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
          createAssignmentCallAst(
            patternAst,
            initAsts.head,
            s"$kind $name$typeCode$initCode$accessorBlockCode".strip(),
            line = line(binding),
            column = column(binding)
          )
        }
      }
    }

    bindingAsts.flatten match {
      case Nil         => Ast()
      case head :: Nil => head
      case others =>
        val block = blockNode(node, code(node), Defines.Any)
        setArgumentIndices(others)
        blockAst(block, others.toList)
    }
  }

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
    case _: MissingDeclSyntax              => Ast()
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
