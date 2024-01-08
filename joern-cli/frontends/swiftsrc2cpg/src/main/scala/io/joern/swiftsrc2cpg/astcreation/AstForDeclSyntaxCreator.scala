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
import io.shiftleft.codepropertygraph.generated.nodes.NewIdentifier
import io.shiftleft.codepropertygraph.generated.nodes.NewMethod
import io.shiftleft.codepropertygraph.generated.nodes.NewTypeDecl

import scala.jdk.CollectionConverters.*

trait AstForDeclSyntaxCreator(implicit withSchemaValidation: ValidationMode) {
  this: AstCreator =>

  private def astForAccessorDeclSyntax(node: AccessorDeclSyntax): Ast             = notHandledYet(node)
  private def astForActorDeclSyntax(node: ActorDeclSyntax): Ast                   = notHandledYet(node)
  private def astForAssociatedTypeDeclSyntax(node: AssociatedTypeDeclSyntax): Ast = notHandledYet(node)

  private def isConstructor(node: SwiftNode): Boolean = node match {
    case _: InitializerDeclSyntax => true
    case _                        => false
  }

  private def declMembers(
    decl: ClassDeclSyntax | ExtensionDeclSyntax | ProtocolDeclSyntax | StructDeclSyntax | EnumDeclSyntax,
    withConstructor: Boolean = true
  ): Seq[DeclSyntax] = {
    val memberBlock = decl match {
      case c: ClassDeclSyntax     => c.memberBlock
      case e: ExtensionDeclSyntax => e.memberBlock
      case p: ProtocolDeclSyntax  => p.memberBlock
      case s: StructDeclSyntax    => s.memberBlock
      case e: EnumDeclSyntax      => e.memberBlock
    }
    val allMembers = memberBlock.members.children.map(_.decl)
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
    case d: ExtensionDeclSyntax      => code(d.extendedType)
    case d: FunctionDeclSyntax       => d.signature.returnClause.map(c => code(c.`type`)).getOrElse(Defines.Any)
    case d: InitializerDeclSyntax    => d.signature.returnClause.map(c => code(c.`type`)).getOrElse(Defines.Any)
    case d: MacroDeclSyntax          => d.signature.returnClause.map(c => code(c.`type`)).getOrElse(Defines.Any)
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
    node: ClassDeclSyntax | ExtensionDeclSyntax | ProtocolDeclSyntax | StructDeclSyntax | EnumDeclSyntax,
    methodBlockContent: List[Ast] = List.empty
  ): AstAndMethod = {
    val constructorName              = io.joern.x2cpg.Defines.ConstructorMethodName
    val (methodName, methodFullName) = calcTypeNameAndFullName(constructorName)
    val methodNode_ = methodNode(node, methodName, constructorName, methodFullName, None, parserResult.filename)

    val modifiers = Seq(NewModifier().modifierType(ModifierTypes.CONSTRUCTOR))

    methodAstParentStack.push(methodNode_)

    val name       = typeNameForDeclSyntax(node)
    val returnType = calcTypeNameAndFullName(name)._2
    val methodReturnNode =
      newMethodReturnNode(returnType, dynamicTypeHintFullName = None, line = line(node), column = column(node))

    methodAstParentStack.pop()

    val functionTypeAndTypeDeclAst = createFunctionTypeAndTypeDeclAst(node, methodNode_, methodName, methodFullName)

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

  private def astForDeclMember(node: DeclSyntax, typeDeclNode: NewTypeDecl): Ast = {
    val typeFullName = typeNameForDeclSyntax(node)
    node match {
      case d: (AccessorDeclSyntax | InitializerDeclSyntax | DeinitializerDeclSyntax | FunctionDeclSyntax) =>
        val function    = astForFunctionLike(d).method
        val bindingNode = newBindingNode("", "", "")
        diffGraph.addEdge(typeDeclNode, bindingNode, EdgeTypes.BINDS)
        diffGraph.addEdge(bindingNode, function, EdgeTypes.REF)
        val memberNode_ = memberNode(d, function.name, code(d), typeFullName, Seq(function.fullName))
        diffGraph.addEdge(typeDeclNode, memberNode_, EdgeTypes.AST)
        Ast()
      case _: (ActorDeclSyntax | AssociatedTypeDeclSyntax | ClassDeclSyntax | EnumDeclSyntax | ExtensionDeclSyntax |
            ImportDeclSyntax | ProtocolDeclSyntax | StructDeclSyntax | MacroDeclSyntax | MacroExpansionDeclSyntax |
            OperatorDeclSyntax | PoundSourceLocationSyntax | PrecedenceGroupDeclSyntax | SubscriptDeclSyntax |
            TypeAliasDeclSyntax | IfConfigDeclSyntax) =>
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

  private def findDeclConstructor(
    decl: ClassDeclSyntax | ExtensionDeclSyntax | ProtocolDeclSyntax | StructDeclSyntax | EnumDeclSyntax
  ): Option[DeclSyntax] =
    declMembers(decl).find(isConstructor)

  private def createDeclConstructor(
    node: ClassDeclSyntax | ExtensionDeclSyntax | ProtocolDeclSyntax | StructDeclSyntax | EnumDeclSyntax,
    constructorContent: List[Ast],
    constructorBlock: Ast = Ast()
  ): Option[AstAndMethod] =
    findDeclConstructor(node) match {
      case Some(constructor: InitializerDeclSyntax) =>
        val result = astForFunctionLike(constructor, methodBlockContent = constructorContent)
        diffGraph.addEdge(result.method, NewModifier().modifierType(ModifierTypes.CONSTRUCTOR), EdgeTypes.AST)
        Some(result)
      case _ if constructorBlock.root.isDefined =>
        constructorBlock.root.foreach { r =>
          constructorContent.foreach { c =>
            Ast.storeInDiffGraph(c, diffGraph)
            c.root.foreach(diffGraph.addEdge(r, _, EdgeTypes.AST))
          }
        }
        None
      case _ =>
        Some(createFakeConstructor(node, methodBlockContent = constructorContent))
    }

  private def isClassMethodOrUninitializedMember(node: DeclSyntax): Boolean =
    node.isInstanceOf[AccessorDeclSyntax] ||
      node.isInstanceOf[InitializerDeclSyntax] ||
      node.isInstanceOf[DeinitializerDeclSyntax] ||
      node.isInstanceOf[FunctionDeclSyntax] ||
      !isInitializedMember(node)

  private def astForDeclAttributes(
    node: ClassDeclSyntax | ProtocolDeclSyntax | VariableDeclSyntax | StructDeclSyntax | EnumDeclSyntax
  ): Seq[Ast] = {
    node match {
      case c: ClassDeclSyntax    => c.attributes.children.map(astForNode)
      case p: ProtocolDeclSyntax => p.attributes.children.map(astForNode)
      case v: VariableDeclSyntax => v.attributes.children.map(astForNode)
      case s: StructDeclSyntax   => s.attributes.children.map(astForNode)
      case e: EnumDeclSyntax     => e.attributes.children.map(astForNode)
    }
  }

  private def astForTypeDeclSyntax(
    node: ClassDeclSyntax | ProtocolDeclSyntax | StructDeclSyntax | EnumDeclSyntax
  ): Ast = {
    // TODO:
    // - handle genericParameterClause
    // - handle genericWhereClause
    val attributes = astForDeclAttributes(node)
    val modifiers  = modifiersForDecl(node)
    val inherits   = inheritsFrom(node)

    val name                     = typeNameForDeclSyntax(node)
    val (typeName, typeFullName) = calcTypeNameAndFullName(name)
    val existingTypeDecl         = global.seenTypeDecls.keys().asScala.find(_.name == typeName)

    if (existingTypeDecl.isEmpty) {
      registerType(typeFullName)

      val astParentType     = methodAstParentStack.head.label
      val astParentFullName = methodAstParentStack.head.properties("FULL_NAME").toString

      val typeDeclNode_ = typeDeclNode(
        node,
        typeName,
        typeFullName,
        parserResult.filename,
        s"class $typeName",
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

      val typeRefNode_ = typeRefNode(node, s"class $typeName", typeFullName)

      methodAstParentStack.push(typeDeclNode_)
      dynamicInstanceTypeStack.push(typeFullName)
      typeRefIdStack.push(typeRefNode_)

      scope.pushNewMethodScope(typeFullName, typeName, typeDeclNode_, None)

      val allClassMembers = declMembers(node, withConstructor = false).toList

      // adding all other members and retrieving their initialization calls
      val memberInitCalls = allClassMembers
        .filter(m => !isStaticMember(m) && isInitializedMember(m))
        .map(m => astForDeclMember(m, typeDeclNode_))

      val constructor = createDeclConstructor(node, memberInitCalls)

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
        if (constructor.isDefined) {
          global.seenTypeDecls.put(
            typeDeclNode_,
            global.ConstructorBlocks(constructor.get.methodBlock, init.methodBlock)
          )
        }
      } else {
        if (constructor.isDefined) {
          global.seenTypeDecls.put(typeDeclNode_, global.ConstructorBlocks(constructor.get.methodBlock, Ast()))
        }
      }
      Ast(typeRefNode_)
    } else {
      val typeDeclNode_          = existingTypeDecl.get
      val constructorBlock       = global.seenTypeDecls.get(typeDeclNode_).constructorBlock
      val staticConstructorBlock = global.seenTypeDecls.get(typeDeclNode_).staticConstructorBlock

      addInheritsFrom(typeDeclNode_, inherits)
      methodAstParentStack.push(typeDeclNode_)
      dynamicInstanceTypeStack.push(typeFullName)

      scope.pushNewMethodScope(typeFullName, typeName, typeDeclNode_, None)

      val allClassMembers = declMembers(node, withConstructor = false).toList

      // adding all other members and retrieving their initialization calls
      val memberInitCalls = allClassMembers
        .filter(m => !isStaticMember(m) && isInitializedMember(m))
        .map(m => astForDeclMember(m, typeDeclNode_))

      createDeclConstructor(node, memberInitCalls, constructorBlock)

      // adding all class methods / functions and uninitialized, non-static members
      allClassMembers
        .filter(member => isClassMethodOrUninitializedMember(member) && !isStaticMember(member))
        .foreach(m => astForDeclMember(m, typeDeclNode_))

      // adding all static members and retrieving their initialization calls
      val staticMemberInitCalls =
        allClassMembers.filter(isStaticMember).map(m => astForDeclMember(m, typeDeclNode_))

      methodAstParentStack.pop()
      dynamicInstanceTypeStack.pop()
      scope.popScope()

      if (staticMemberInitCalls.nonEmpty) {
        if (staticConstructorBlock.nodes.isEmpty) {
          val init = staticInitMethodAstAndBlock(
            staticMemberInitCalls,
            s"$typeFullName:${io.joern.x2cpg.Defines.StaticInitMethodName}",
            None,
            Defines.Any
          )
          Ast.storeInDiffGraph(init.ast, diffGraph)
          diffGraph.addEdge(typeDeclNode_, init.method, EdgeTypes.AST)
        } else {
          val block = staticConstructorBlock.root.get
          staticMemberInitCalls.foreach { s =>
            Ast.storeInDiffGraph(s, diffGraph)
            s.root.foreach(diffGraph.addEdge(block, _, EdgeTypes.AST))
          }
        }
      }

      Ast()
    }
  }

  private def astForDeinitializerDeclSyntax(node: DeinitializerDeclSyntax): Ast         = notHandledYet(node)
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

  private def inheritsFrom(
    node: ClassDeclSyntax | ExtensionDeclSyntax | ProtocolDeclSyntax | StructDeclSyntax | EnumDeclSyntax
  ): Seq[String] = {
    val clause = node match {
      case c: ClassDeclSyntax     => c.inheritanceClause
      case e: ExtensionDeclSyntax => e.inheritanceClause
      case p: ProtocolDeclSyntax  => p.inheritanceClause
      case s: StructDeclSyntax    => s.inheritanceClause
      case e: EnumDeclSyntax      => e.inheritanceClause
    }
    val inheritsFrom = clause match {
      case Some(value) => value.inheritedTypes.children.map(c => code(c.`type`))
      case None        => Seq.empty
    }
    inheritsFrom.distinct.sorted
  }

  private def addInheritsFrom(typeDecl: NewTypeDecl, inherits: Seq[String]): Unit = {
    typeDecl.inheritsFromTypeFullName((typeDecl.inheritsFromTypeFullName ++ inherits).distinct.sorted)
  }

  private def astForExtensionDeclSyntax(node: ExtensionDeclSyntax): Ast = {
    // TODO:
    // - handle genericParameterClause
    // - handle genericWhereClause
    val attributes = node.attributes.children.map(astForNode)
    val modifiers  = modifiersForDecl(node)
    val inherits   = inheritsFrom(node)

    val name                     = typeNameForDeclSyntax(node)
    val (typeName, typeFullName) = calcTypeNameAndFullName(name)
    val existingTypeDecl         = global.seenTypeDecls.keys().asScala.find(_.name == typeName)

    if (existingTypeDecl.isEmpty) {
      registerType(typeFullName)

      val astParentType     = methodAstParentStack.head.label
      val astParentFullName = methodAstParentStack.head.properties("FULL_NAME").toString

      val typeDeclNode_ = typeDeclNode(
        node,
        typeName,
        typeFullName,
        parserResult.filename,
        s"class $typeName",
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

      val typeRefNode_ = typeRefNode(node, s"class $typeName", typeFullName)

      methodAstParentStack.push(typeDeclNode_)
      dynamicInstanceTypeStack.push(typeFullName)
      typeRefIdStack.push(typeRefNode_)

      scope.pushNewMethodScope(typeFullName, typeName, typeDeclNode_, None)

      val allClassMembers = declMembers(node, withConstructor = false).toList

      // adding all other members and retrieving their initialization calls
      val memberInitCalls = allClassMembers
        .filter(m => !isStaticMember(m) && isInitializedMember(m))
        .map(m => astForDeclMember(m, typeDeclNode_))

      val constructor = createDeclConstructor(node, memberInitCalls)

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
        if (constructor.isDefined) {
          global.seenTypeDecls.put(
            typeDeclNode_,
            global.ConstructorBlocks(constructor.get.methodBlock, init.methodBlock)
          )
        } else {
          global.seenTypeDecls.put(typeDeclNode_, global.ConstructorBlocks(Ast(), init.methodBlock))
        }
      } else {
        if (constructor.isDefined) {
          global.seenTypeDecls.put(typeDeclNode_, global.ConstructorBlocks(constructor.get.methodBlock, Ast()))
        } else {
          global.seenTypeDecls.put(typeDeclNode_, global.ConstructorBlocks(Ast(), Ast()))
        }
      }
      Ast(typeRefNode_)
    } else {
      val typeDeclNode_          = existingTypeDecl.get
      val constructorBlock       = global.seenTypeDecls.get(typeDeclNode_).constructorBlock
      val staticConstructorBlock = global.seenTypeDecls.get(typeDeclNode_).staticConstructorBlock

      addInheritsFrom(typeDeclNode_, inherits)
      methodAstParentStack.push(typeDeclNode_)
      dynamicInstanceTypeStack.push(typeFullName)

      scope.pushNewMethodScope(typeFullName, typeName, typeDeclNode_, None)

      val allClassMembers = declMembers(node, withConstructor = false).toList

      // adding all other members and retrieving their initialization calls
      val memberInitCalls = allClassMembers
        .filter(m => !isStaticMember(m) && isInitializedMember(m))
        .map(m => astForDeclMember(m, typeDeclNode_))

      createDeclConstructor(node, memberInitCalls, constructorBlock)

      // adding all class methods / functions and uninitialized, non-static members
      allClassMembers
        .filter(member => isClassMethodOrUninitializedMember(member) && !isStaticMember(member))
        .foreach(m => astForDeclMember(m, typeDeclNode_))

      // adding all static members and retrieving their initialization calls
      val staticMemberInitCalls =
        allClassMembers.filter(isStaticMember).map(m => astForDeclMember(m, typeDeclNode_))

      methodAstParentStack.pop()
      dynamicInstanceTypeStack.pop()
      scope.popScope()

      if (staticMemberInitCalls.nonEmpty) {
        if (staticConstructorBlock.nodes.isEmpty) {
          val init = staticInitMethodAstAndBlock(
            staticMemberInitCalls,
            s"$typeFullName:${io.joern.x2cpg.Defines.StaticInitMethodName}",
            None,
            Defines.Any
          )
          Ast.storeInDiffGraph(init.ast, diffGraph)
          diffGraph.addEdge(typeDeclNode_, init.method, EdgeTypes.AST)
        } else {
          val block = staticConstructorBlock.root.get
          staticMemberInitCalls.foreach { s =>
            Ast.storeInDiffGraph(s, diffGraph)
            s.root.foreach(diffGraph.addEdge(block, _, EdgeTypes.AST))
          }
        }
      }

      Ast()
    }
  }

  private def modifiersForDecl(
    node: ClassDeclSyntax | ExtensionDeclSyntax | ProtocolDeclSyntax | StructDeclSyntax | EnumDeclSyntax |
      EnumCaseDeclSyntax
  ): Seq[NewModifier] = {
    val modifierList = node match {
      case c: ClassDeclSyntax     => c.modifiers.children
      case e: ExtensionDeclSyntax => e.modifiers.children
      case p: ProtocolDeclSyntax  => p.modifiers.children
      case s: StructDeclSyntax    => s.modifiers.children
      case e: EnumDeclSyntax      => e.modifiers.children
      case ec: EnumCaseDeclSyntax => ec.modifiers.children
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

  private def modifiersForFunctionLike(
    node: FunctionDeclSyntax | AccessorDeclSyntax | InitializerDeclSyntax | DeinitializerDeclSyntax
  ): Seq[NewModifier] = {
    val virtualModifier = NewModifier().modifierType(ModifierTypes.VIRTUAL)
    val modifiers = node match {
      case f: FunctionDeclSyntax =>
        f.modifiers.children.flatMap(c => astForNode(c).root.map(_.asInstanceOf[NewModifier]))
      case a: AccessorDeclSyntax => a.modifier.toSeq.flatMap(c => astForNode(c).root.map(_.asInstanceOf[NewModifier]))
      case i: InitializerDeclSyntax =>
        i.modifiers.children.flatMap(c => astForNode(c).root.map(_.asInstanceOf[NewModifier]))
      case d: DeinitializerDeclSyntax =>
        d.modifiers.children.flatMap(c => astForNode(c).root.map(_.asInstanceOf[NewModifier]))
    }
    (virtualModifier +: modifiers).zipWithIndex.map { case (m, index) =>
      m.order(index)
    }
  }

  case class AstAndMethod(ast: Ast, method: NewMethod, methodBlock: Ast)

  protected def astForFunctionLike(
    node: FunctionDeclSyntax | AccessorDeclSyntax | InitializerDeclSyntax | DeinitializerDeclSyntax,
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
    }

    val modifiers                    = modifiersForFunctionLike(node)
    val (methodName, methodFullName) = calcMethodNameAndFullName(node)
    val filename                     = parserResult.filename

    val methodRefNode_ = if (!shouldCreateFunctionReference) {
      None
    } else {
      Option(methodRefNode(node, methodName, methodFullName, methodFullName))
    }

    val callAst = if (shouldCreateAssignmentCall && shouldCreateFunctionReference) {
      val idNode  = identifierNode(node, methodName)
      val idLocal = localNode(node, methodName, methodName, methodFullName).order(0)
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

    val (signature, returnType) = node match {
      case f: FunctionDeclSyntax =>
        val returnType = f.signature.returnClause.map(c => code(c.`type`)).getOrElse(Defines.Any)
        registerType(returnType)
        (s"$returnType $methodFullName ${code(f.signature.parameterClause)}", returnType)
      case a: AccessorDeclSyntax =>
        val returnType = Defines.Any
        (s"$returnType $methodFullName ${a.parameters.map(code).getOrElse("()")}", returnType)
      case i: InitializerDeclSyntax =>
        val returnType = methodAstParentStack.head.properties("FULL_NAME").toString
        (s"$returnType $methodFullName ${i.signature.parameterClause.parameters.children.map(code)}", returnType)
      case _: DeinitializerDeclSyntax =>
        val returnType = Defines.Any
        (s"$returnType $methodFullName ()", returnType)
    }

    val codeString  = code(node)
    val methodNode_ = methodNode(node, methodName, codeString, methodFullName, Some(signature), filename)
    val block       = blockNode(node, "<empty>", Defines.Any)
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
    }

    val body = node match {
      case f: FunctionDeclSyntax      => f.body
      case a: AccessorDeclSyntax      => a.body
      case i: InitializerDeclSyntax   => i.body
      case d: DeinitializerDeclSyntax => d.body
    }

    val bodyStmtAsts = body match {
      case Some(bodyNode) =>
        bodyNode.statements.children.toList match {
          case Nil => List.empty[Ast]
          case head :: Nil if head.item.isInstanceOf[ArrowExprSyntax] =>
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

    val typeDeclAst = createFunctionTypeAndTypeDeclAst(node, methodNode_, methodName, methodFullName)
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

  private def astForIfConfigDeclSyntax(node: IfConfigDeclSyntax): Ast               = notHandledYet(node)
  private def astForImportDeclSyntax(node: ImportDeclSyntax): Ast                   = notHandledYet(node)
  private def astForInitializerDeclSyntax(node: InitializerDeclSyntax): Ast         = notHandledYet(node)
  private def astForMacroDeclSyntax(node: MacroDeclSyntax): Ast                     = notHandledYet(node)
  private def astForMacroExpansionDeclSyntax(node: MacroExpansionDeclSyntax): Ast   = notHandledYet(node)
  private def astForOperatorDeclSyntax(node: OperatorDeclSyntax): Ast               = notHandledYet(node)
  private def astForPoundSourceLocationSyntax(node: PoundSourceLocationSyntax): Ast = notHandledYet(node)
  private def astForPrecedenceGroupDeclSyntax(node: PrecedenceGroupDeclSyntax): Ast = notHandledYet(node)
  private def astForSubscriptDeclSyntax(node: SubscriptDeclSyntax): Ast             = notHandledYet(node)
  private def astForTypeAliasDeclSyntax(node: TypeAliasDeclSyntax): Ast             = notHandledYet(node)

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
      val name = binding.pattern match {
        case expr: ExpressionPatternSyntax =>
          notHandledYet(expr)
          code(expr)
        case ident: IdentifierPatternSyntax =>
          code(ident.identifier)
        case isType: IsTypePatternSyntax =>
          notHandledYet(isType)
          code(isType)
        case missing: MissingPatternSyntax =>
          code(missing.placeholder)
        case tuple: TuplePatternSyntax =>
          notHandledYet(tuple)
          code(tuple)
        case valueBinding: ValueBindingPatternSyntax =>
          notHandledYet(valueBinding)
          code(valueBinding)
        case wildcard: WildcardPatternSyntax =>
          notHandledYet(wildcard)
          generateUnusedVariableName(usedVariableNames, "wildcard")
      }
      val typeFullName = binding.typeAnnotation.map(t => code(t.`type`)).getOrElse(Defines.Any)
      val nLocalNode   = localNode(binding, name, name, typeFullName).order(0)
      scope.addVariable(name, nLocalNode, scopeType)
      diffGraph.addEdge(localAstParentStack.head, nLocalNode, EdgeTypes.AST)

      val initAsts = binding.initializer.map(astForNode) ++ binding.accessorBlock.map(astForNode)
      if (initAsts.isEmpty) {
        Ast()
      } else {
        val patternAst = astForNode(binding.pattern)
        patternAst.root.collect { case i: NewIdentifier => i }.foreach(_.typeFullName(typeFullName))
        modifiers.foreach { mod =>
          patternAst.root.foreach { r => diffGraph.addEdge(r, mod, EdgeTypes.AST) }
        }
        attributeAsts.foreach { attrAst =>
          patternAst.root.foreach { r => attrAst.root.foreach { attr => diffGraph.addEdge(r, attr, EdgeTypes.AST) } }
        }
        createAssignmentCallAst(
          patternAst,
          initAsts.head,
          s"$kind ${code(binding)}",
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
