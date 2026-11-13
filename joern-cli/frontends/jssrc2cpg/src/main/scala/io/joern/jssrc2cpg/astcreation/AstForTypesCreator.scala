package io.joern.jssrc2cpg.astcreation

import io.joern.jssrc2cpg.parser.BabelNodeSyntax.*
import io.joern.jssrc2cpg.passes.EcmaBuiltins
import io.joern.x2cpg.{Ast, ValidationMode}
import io.joern.x2cpg.datastructures.Stack.*
import io.joern.x2cpg.datastructures.VariableScopeManager
import io.joern.x2cpg.frontendspecific.jssrc2cpg.Defines
import io.shiftleft.codepropertygraph.generated.nodes.{
  NewBlock,
  NewCall,
  NewMethod,
  NewModifier,
  NewNamespaceBlock,
  NewNode,
  NewTypeDecl
}
import io.shiftleft.codepropertygraph.generated.{
  DispatchTypes,
  EdgeTypes,
  EvaluationStrategies,
  ModifierTypes,
  Operators,
  PropertyDefaults,
  PropertyNames
}

import java.util
import java.util.Collections

trait AstForTypesCreator(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  /** The namespaces whose body is processed, the innermost one on top. */
  private val moduleScopes = new Stack[ModuleScope]()

  protected def astForTypeAlias(
    alias: DeclareTypeAlias | TypeAlias | TSTypeAliasDeclaration | ObjectPattern | TSTypeLiteral
  ): Ast = {
    val (aliasName, aliasFullName) = calcTypeNameAndFullName(alias)
    registerType(aliasFullName)

    val nameNode: BabelNode = alias match {
      case a: DeclareTypeAlias => a.right
      case a: TypeAlias        => a.right
      case other               => other
    }
    val nameTpe = typeFor(nameNode)
    val name    = if (nameTpe.contains("{") || nameTpe.contains("(")) {
      calcTypeNameAndFullName(nameNode)._1
    } else nameTpe

    val astParentType     = methodAstParentStack.head.label
    val astParentFullName = methodAstParentStack.head.properties(PropertyNames.FullName).toString

    val aliasTypeDeclNode =
      typeDeclNode(alias, aliasName, aliasFullName, parserResult.filename, alias.code, astParentType, astParentFullName)
    seenAliasTypes.update(aliasTypeDeclNode.name, aliasTypeDeclNode)

    if (!Defines.JsTypes.contains(name) && !seenAliasTypes.contains(name)) {
      val (typeName, typeFullName) = calcTypeNameAndFullName(alias, Option(name))
      registerType(typeFullName)

      val typeDeclNode_ = typeDeclNode(
        alias,
        typeName,
        typeFullName,
        parserResult.filename,
        alias.code,
        astParentType,
        astParentFullName,
        alias = Option(aliasFullName)
      )
      diffGraph.addEdge(methodAstParentStack.head, typeDeclNode_, EdgeTypes.AST)
    } else {
      seenAliasTypes.get(name).foreach(_.aliasTypeFullName(aliasFullName))
    }

    // adding all class methods / functions and uninitialized, non-static members
    val members: Seq[BabelNode] = alias match {
      case literal: TSTypeLiteral     => literal.members
      case pattern: ObjectPattern     => pattern.properties
      case ts: TSTypeAliasDeclaration =>
        ts.typeAnnotation match {
          case literal: TSTypeLiteral => literal.members
          case _                      => Seq.empty
        }
      case _: (DeclareTypeAlias | TypeAlias) => Seq.empty
    }
    members
      .filter(member => isClassMethodOrUninitializedMemberOrObjectProperty(member) && !isStaticMember(member))
      .map(member => astForClassMember(member, aliasTypeDeclNode, ignoreInitCalls = true))
    Ast(aliasTypeDeclNode)
  }

  private def kindOf(node: BabelNode): Option[String] = node match {
    case n: ClassMethod         => Some(n.kind)
    case n: ClassPrivateMethod  => Some(n.kind)
    case n: ObjectMethod        => Some(n.kind)
    case n: TSDeclareMethod     => Some(n.kind)
    case n: TSMethodSignature   => Some(n.kind)
    case n: TSPropertySignature => n.kind
    case _                      => None
  }

  private def isConstructor(node: BabelNode): Boolean = node match {
    case _: TSConstructSignatureDeclaration => true
    case _                                  => kindOf(node).contains("constructor")
  }

  private def staticOf(node: BabelNode): Boolean = node match {
    case n: ClassAccessorProperty => n.static
    case n: ClassMethod           => n.static
    case n: ClassPrivateMethod    => n.static
    case n: ClassPrivateProperty  => n.static
    case n: ClassProperty         => n.static
    case n: TSDeclareMethod       => n.static
    case n: TSIndexSignature      => n.static.contains(true)
    case _                        => false
  }

  private def abstractOf(node: BabelNode): Boolean = node match {
    case n: ClassAccessorProperty => n.`abstract`.contains(true)
    case n: ClassDeclaration      => n.`abstract`.contains(true)
    case n: ClassMethod           => n.`abstract`.contains(true)
    case n: ClassPrivateMethod    => n.`abstract`.contains(true)
    case n: ClassProperty         => n.`abstract`.contains(true)
    case n: TSDeclareMethod       => n.`abstract`.contains(true)
    case _                        => false
  }

  private def readonlyOf(node: BabelNode): Boolean = node match {
    case n: ClassAccessorProperty => n.readonly.contains(true)
    case n: ClassPrivateProperty  => n.readonly.contains(true)
    case n: ClassProperty         => n.readonly.contains(true)
    case n: TSIndexSignature      => n.readonly.contains(true)
    case n: TSParameterProperty   => n.readonly.contains(true)
    case n: TSPropertySignature   => n.readonly.contains(true)
    case _                        => false
  }

  private def accessibilityOf(node: BabelNode): Option[String] = node match {
    case n: ClassAccessorProperty => n.accessibility
    case n: ClassMethod           => n.accessibility
    case n: ClassPrivateMethod    => n.accessibility
    case n: ClassProperty         => n.accessibility
    case n: TSDeclareMethod       => n.accessibility
    case n: TSParameterProperty   => n.accessibility
    case _                        => None
  }

  /** The key and the value of a member that is initialized with a value, e.g. `a = 1` in a class body. */
  private def keyAndValueOf(node: BabelNode): Option[(BabelNode, BabelNode)] = node match {
    case n: ClassProperty         => n.value.map(n.key -> _)
    case n: ClassPrivateProperty  => n.value.map(n.key -> _)
    case n: ClassAccessorProperty => n.value.map(n.key -> _)
    case n: ObjectProperty        => Some(n.key -> n.value)
    case _                        => None
  }

  /** The members declared in the body of a class or an interface. */
  private def declaredMembersOf(node: BabelNode): Seq[BabelNode] = node match {
    case n: ClassDeclaration       => n.body.body
    case n: ClassExpression        => n.body.body
    case n: TSInterfaceDeclaration => n.body.body
    case _                         => Seq.empty
  }

  /** The decomposed body of a class: its parameter properties, all declared members, the members declared dynamically
    * in the constructor body, and the constructor itself (if present).
    */
  private case class ClassMembers(
    parameterProperties: Seq[BabelNode],
    allMembers: Seq[BabelNode],
    dynamicallyDeclaredMembers: Seq[BabelNode],
    constructor: Option[FunctionNode]
  ) {
    def members(withConstructor: Boolean): Seq[BabelNode] =
      if (withConstructor) {
        parameterProperties ++ allMembers ++ dynamicallyDeclaredMembers
      } else {
        parameterProperties ++ allMembers.diff(constructor.toSeq) ++ dynamicallyDeclaredMembers
      }
  }

  private def decomposeClassMembers(clazz: BabelNode): ClassMembers = {
    val allMembers      = declaredMembersOf(clazz)
    val constructor     = allMembers.collectFirst { case f: FunctionNode if isConstructor(f) => f }
    val constructorBody = constructor.toSeq.flatMap {
      case c: ClassMethod        => c.body.body
      case c: ClassPrivateMethod => c.body.body
      case _                     => Seq.empty
    }
    val constructorParameters = constructor.toSeq.flatMap {
      case c: ClassMethod        => c.params
      case c: ClassPrivateMethod => c.params
      case c: TSDeclareMethod    => c.params
      case _                     => Seq.empty
    }
    val dynamicallyDeclaredMembers = constructorBody.filter(isInitializedMember)
    val parameterProperties        = constructorParameters.filter(isParameterProperty)
    ClassMembers(parameterProperties, allMembers, dynamicallyDeclaredMembers, constructor)
  }

  private def classMembers(clazz: BabelNode, withConstructor: Boolean = true): Seq[BabelNode] =
    decomposeClassMembers(clazz).members(withConstructor)

  private def createFakeConstructor(
    code: String,
    forElem: BabelNode,
    methodBlockContent: ConstructorContent
  ): MethodAst = {
    val fakeStart = ujson.Num(forElem.start.getOrElse(-1))
    val fakeEnd   = ujson.Num(forElem.end.getOrElse(-1))
    // the constructor is located where the class is
    val fakeLoc         = forElem.json.objOpt.flatMap(_.get("loc")).filter(_ != ujson.Null).toList.map("loc" -> _)
    val fakeConstructor = ClassMethod(
      ujson.Obj.from(
        List(
          "type"  -> ujson.Str("ClassMethod"),
          "start" -> fakeStart,
          "end"   -> fakeEnd,
          "key"   -> ujson.Obj.from(
            List(
              "type"  -> ujson.Str("Identifier"),
              "name"  -> ujson.Str("constructor"),
              "start" -> fakeStart,
              "end"   -> fakeEnd
            ) ++ fakeLoc
          ),
          "kind"      -> ujson.Str("constructor"),
          "static"    -> ujson.False,
          "computed"  -> ujson.False,
          "generator" -> ujson.False,
          "async"     -> ujson.False,
          "id"        -> ujson.Null,
          "params"    -> ujson.Arr(),
          "body"      -> ujson.Obj("type" -> "BlockStatement", "body" -> ujson.Arr(), "directives" -> ujson.Arr())
        ) ++ fakeLoc
      )
    )
    val result = createMethodAstAndNode(fakeConstructor, false, false, methodBlockContent)
    result.methodNode.code(code)
    result
  }

  private def findClassConstructor(clazz: BabelNode): Option[FunctionNode] =
    decomposeClassMembers(clazz).constructor

  protected case class ConstructorContent(
    clazz: Option[BabelNode],
    constructorContent: List[BabelNode],
    typeDecl: Option[NewTypeDecl]
  )
  protected object ConstructorContent {
    def empty: ConstructorContent = ConstructorContent(None, List.empty, None)
  }

  private def createClassConstructor(
    classExpr: BabelNode,
    classConstructorNode: Option[FunctionNode],
    constructorContent: ConstructorContent
  ): MethodAst =
    classConstructorNode match {
      case Some(classConstructor: FunctionWithBody) =>
        val result = createMethodAstAndNode(classConstructor, false, false, constructorContent)
        diffGraph.addEdge(result.methodNode, NewModifier().modifierType(ModifierTypes.CONSTRUCTOR), EdgeTypes.AST)
        result
      case Some(classConstructor) =>
        val methodNode = createMethodDefinitionNode(classConstructor, constructorContent)
        diffGraph.addEdge(methodNode, NewModifier().modifierType(ModifierTypes.CONSTRUCTOR), EdgeTypes.AST)
        MethodAst(Ast(methodNode), methodNode, Ast(methodNode))
      case _ =>
        val result = createFakeConstructor("constructor() {}", classExpr, constructorContent)
        diffGraph.addEdge(result.methodNode, NewModifier().modifierType(ModifierTypes.CONSTRUCTOR), EdgeTypes.AST)
        result
    }

  private def interfaceConstructor(typeName: String, tsInterface: TSInterfaceDeclaration): NewMethod =
    findClassConstructor(tsInterface) match {
      case Some(interfaceConstructor) =>
        createMethodDefinitionNode(interfaceConstructor, ConstructorContent.empty)
      case _ => createFakeConstructor(s"new: $typeName", tsInterface, ConstructorContent.empty).methodNode
    }

  /** The class or enum a static member belongs to. Its type is the base of the member's access, e.g. `ClassA` in
    * `ClassA.b = 2`.
    */
  protected case class MemberOwner(name: String, typeFullName: String)

  private def thisAst(node: BabelNode): Ast = {
    val thisNode = identifierNode(node, "this").dynamicTypeHintFullName(typeHintForThisExpression())
    scope.addVariableReference(thisNode.name, thisNode, Defines.Any, EvaluationStrategies.BY_REFERENCE)
    Ast(thisNode)
  }

  /** The access of the member `key` of `base`: `base.key`, or `base[key]` for keys that are no plain names. */
  private def memberAccessAst(base: Ast, key: BabelNode, isComputed: Boolean, node: BabelNode): Ast = key match {
    case id: Identifier if !isComputed =>
      createFieldAccessCallAst(base, fieldIdentifierNode(id, id.name, id.name), node.lineNumber, node.columnNumber)
    case privateName: PrivateName =>
      val name = privateName.code
      createFieldAccessCallAst(base, fieldIdentifierNode(privateName, name, name), node.lineNumber, node.columnNumber)
    case _ =>
      createIndexAccessCallAst(base, astForNode(key), node.lineNumber, node.columnNumber)
  }

  /** The base of the access of a member: `this` for instance members, the type of the owner for static members. */
  private def memberBaseAst(node: BabelNode, isStatic: Boolean, owner: Option[MemberOwner]): Ast =
    owner.filter(_ => isStatic) match {
      case Some(MemberOwner(name, typeFullName)) => Ast(typeRefNode(node, name, typeFullName))
      case None                                  => thisAst(node)
    }

  private def astsForEnumMember(tsEnumMember: TSEnumMember, owner: MemberOwner): Seq[Ast] = {
    val name          = tsEnumMember.id.code
    val tpe           = typeFor(tsEnumMember)
    val possibleTypes = Seq(tpe)
    val typeFullName  = if (Defines.isBuiltinType(tpe)) tpe else Defines.Any
    val memberNode_   = memberNode(tsEnumMember, name, tsEnumMember.code, typeFullName).possibleTypes(possibleTypes)
    addModifier(memberNode_, tsEnumMember)

    tsEnumMember.initializer match {
      case Some(initializer) =>
        val lhsAst =
          memberAccessAst(memberBaseAst(tsEnumMember, true, Some(owner)), tsEnumMember.id, false, tsEnumMember)
        val rhsAst    = astForNodeWithFunctionReference(initializer)
        val callNode_ =
          callNode(tsEnumMember, tsEnumMember.code, Operators.assignment, DispatchTypes.STATIC_DISPATCH)
        val argAsts = List(lhsAst, rhsAst)
        Seq(callAst(callNode_, argAsts), Ast(memberNode_))
      case None =>
        Seq(Ast(memberNode_))
    }
  }

  /** The identifier a property is keyed by, if any. */
  private def keyIdentifierOf(node: BabelNode): Option[Identifier] = node match {
    case n: TSPropertySignature => Some(n.key).collect { case id: Identifier => id }
    case n: ObjectProperty      => Some(n.key).collect { case id: Identifier => id }
    case _                      => None
  }

  private def storeDecorators(node: BabelNode, memberNode_ : NewNode): Unit =
    astsForDecorators(node).foreach { decoratorAst =>
      Ast.storeInDiffGraph(decoratorAst, diffGraph)
      decoratorAst.root.foreach(diffGraph.addEdge(memberNode_, _, EdgeTypes.AST))
    }

  protected def astForClassMember(
    classElement: BabelNode,
    typeDeclNode: NewTypeDecl,
    ignoreInitCalls: Boolean = false,
    staticOwner: Option[MemberOwner] = None
  ): Ast = {
    val tpe           = typeFor(classElement)
    val possibleTypes = Seq(tpe)
    val typeFullName  = if (Defines.isBuiltinType(tpe)) tpe else Defines.Any
    val memberNode_   = classElement match {
      case function: (TSDeclareMethod | TSDeclareFunction) =>
        val functionNode = createMethodDefinitionNode(function, ConstructorContent.empty)
        addModifier(functionNode, function)
        memberNode(function, functionNode.name, function.code, typeFullName, Seq(functionNode.fullName))
          .possibleTypes(possibleTypes)
      case method: (ClassMethod | ClassPrivateMethod) =>
        val functionNode = createMethodAstAndNode(method, false, false, ConstructorContent.empty).methodNode
        addModifier(functionNode, method)
        memberNode(method, functionNode.name, method.code, typeFullName, Seq(functionNode.fullName))
          .possibleTypes(possibleTypes)
      case statement: ExpressionStatement if isInitializedMember(statement) =>
        val name = statement.expression match {
          case assignment: AssignmentExpression =>
            assignment.left match {
              case member: MemberExpression => member.property.code
              case other                    => other.code
            }
          case other => other.code
        }
        memberNode(statement, name, statement.code, typeFullName).possibleTypes(possibleTypes)
      case _ if keyIdentifierOf(classElement).isDefined =>
        val name        = keyIdentifierOf(classElement).get.name
        val memberNode_ = memberNode(classElement, name, classElement.code, typeFullName).possibleTypes(possibleTypes)
        storeDecorators(classElement, memberNode_)
        memberNode_
      case _ =>
        val name = classElement match {
          // The name of a private field includes the hash: `#a` is not the same member as `a`
          case property: ClassProperty         => property.key.code
          case property: ClassAccessorProperty => property.key.code
          case property: ClassPrivateProperty  => property.key.code
          case property: TSParameterProperty   =>
            property.parameter match {
              case assignment: AssignmentPattern => assignment.left.code
              case identifier: Identifier        => identifier.name
            }
          // TODO: name field most likely needs adjustment for other Babel AST types
          case _ => classElement.code
        }
        val memberNode_ = memberNode(classElement, name, classElement.code, typeFullName).possibleTypes(possibleTypes)
        storeDecorators(classElement, memberNode_)
        memberNode_
    }

    addModifier(memberNode_, classElement)
    diffGraph.addEdge(typeDeclNode, memberNode_, EdgeTypes.AST)

    if (!ignoreInitCalls) {
      keyAndValueOf(classElement) match {
        case Some((key, value)) =>
          val lhsAst = classElement match {
            case _: (ClassProperty | ClassPrivateProperty | ClassAccessorProperty) =>
              val isComputed = classElement match {
                case n: ClassProperty         => n.computed
                case n: ClassAccessorProperty => n.computed
                case _                        => false
              }
              memberAccessAst(
                memberBaseAst(classElement, staticOf(classElement), staticOwner),
                key,
                isComputed,
                classElement
              )
            case _ => astForNode(key)
          }
          val rhsAst    = astForNodeWithFunctionReference(value)
          val callNode_ =
            callNode(classElement, classElement.code, Operators.assignment, DispatchTypes.STATIC_DISPATCH)
          val argAsts = List(lhsAst, rhsAst)
          return callAst(callNode_, argAsts)
        case None =>
      }
    }

    if (!ignoreInitCalls && classElement.isInstanceOf[TSParameterProperty]) {
      val name       = memberNode_.name
      val memberNode = fieldIdentifierNode(classElement, name, name)
      val thisNode   = identifierNode(classElement, "this")
      scope.addVariableReference(thisNode.name, thisNode, Defines.Any, EvaluationStrategies.BY_REFERENCE)
      val fieldAccessAst =
        createFieldAccessCallAst(thisNode, memberNode, classElement.lineNumber, classElement.columnNumber)
      val rhsAst    = Ast(identifierNode(classElement, name, name, tpe).possibleTypes(possibleTypes))
      val callNode_ =
        callNode(classElement, s"this.$name = $name", Operators.assignment, DispatchTypes.STATIC_DISPATCH)
      val argAsts = List(fieldAccessAst, rhsAst)
      return callAst(callNode_, argAsts)
    }

    Ast()
  }

  protected def astForEnum(tsEnum: TSEnumDeclaration): Ast = {
    val (typeName, typeFullName) = calcTypeNameAndFullName(tsEnum)
    registerType(typeFullName)

    val astParentType     = methodAstParentStack.head.label
    val astParentFullName = methodAstParentStack.head.properties(PropertyNames.FullName).toString

    val typeDeclNode_ = typeDeclNode(
      tsEnum,
      typeName,
      typeFullName,
      parserResult.filename,
      s"enum $typeName",
      astParentType,
      astParentFullName
    )
    seenAliasTypes.update(typeDeclNode_.name, typeDeclNode_)

    addModifier(typeDeclNode_, tsEnum)

    val typeRefNode_ = typeRefNode(tsEnum, s"enum $typeName", typeFullName)

    methodAstParentStack.push(typeDeclNode_)
    dynamicInstanceTypeStack.push(typeFullName)
    typeRefIdStack.push(typeRefNode_)
    scope.pushNewMethodScope(typeFullName, typeName, typeDeclNode_, None)

    val (enumMembersWithInitializer, enumMembersPlain) = tsEnum.body.members.toList.partition(_.initializer.isDefined)

    val enumOwner  = MemberOwner(typeName, typeFullName)
    val memberAsts = enumMembersPlain.flatMap(astsForEnumMember(_, enumOwner))

    if (enumMembersWithInitializer.isEmpty) {
      Ast.storeInDiffGraph(Ast(typeDeclNode_).withChildren(memberAsts), diffGraph)
    } else {
      val init = staticEnumInitMethodAst(
        tsEnum,
        typeDeclNode_,
        enumMembersWithInitializer,
        s"$typeFullName:${io.joern.x2cpg.Defines.StaticInitMethodName}",
        Defines.Any,
        enumOwner
      )
      Ast.storeInDiffGraph(Ast(typeDeclNode_).withChildren(memberAsts).withChild(init), diffGraph)
    }

    methodAstParentStack.pop()
    dynamicInstanceTypeStack.pop()
    typeRefIdStack.pop()
    scope.popScope()

    diffGraph.addEdge(methodAstParentStack.head, typeDeclNode_, EdgeTypes.AST)
    Ast(typeRefNode_)
  }

  private def isStaticMember(node: BabelNode): Boolean = {
    val isMethod = node.isInstanceOf[ClassMethod] || node.isInstanceOf[ClassPrivateMethod]
    !isMethod && staticOf(node)
  }

  /** Static methods live on the class constructor function and not on its instances. */
  private def isStaticMethod(node: BabelNode): Boolean = node match {
    case _: (ClassMethod | ClassPrivateMethod | TSDeclareMethod) => staticOf(node)
    case _                                                       => false
  }

  private def isInitializedMember(node: BabelNode): Boolean = {
    val isAssignment = node match {
      case statement: ExpressionStatement =>
        statement.expression match {
          case assignment: AssignmentExpression =>
            assignment.left match {
              case member: MemberExpression => member.`object`.code == "this"
              case _                        => false
            }
          case _ => false
        }
      case _ => false
    }
    keyAndValueOf(node).isDefined || isAssignment
  }

  private def isParameterProperty(node: BabelNode): Boolean = node.isInstanceOf[TSParameterProperty]

  private def isStaticInitBlock(node: BabelNode): Boolean = node.isInstanceOf[StaticBlock]

  private def isClassMethodOrUninitializedMember(node: BabelNode): Boolean =
    !node.isInstanceOf[StaticBlock] &&
      (node.isInstanceOf[ClassMethod] || node.isInstanceOf[ClassPrivateMethod] || !isInitializedMember(node))

  private def isClassMethodOrUninitializedMemberOrObjectProperty(node: BabelNode): Boolean =
    !node.isInstanceOf[StaticBlock] &&
      (node.isInstanceOf[ObjectProperty] || node.isInstanceOf[ClassMethod] || node.isInstanceOf[ClassPrivateMethod] ||
        !isInitializedMember(node))

  private def staticClassInitMethodAst(
    node: BabelNode,
    typeDeclNode: NewTypeDecl,
    staticMemberInits: List[BabelNode],
    staticMemberBlocks: List[BabelNode],
    fullName: String,
    returnType: String,
    owner: MemberOwner
  ): Ast = {
    val modifiers =
      List(NewModifier().modifierType(ModifierTypes.STATIC), NewModifier().modifierType(ModifierTypes.CONSTRUCTOR))
    val blockNode   = NewBlock().typeFullName(Defines.Any)
    val methodNode_ = methodNode(node, io.joern.x2cpg.Defines.StaticInitMethodName, fullName, "", parserResult.filename)

    methodAstParentStack.push(methodNode_)
    scope.pushNewMethodScope(
      fullName,
      io.joern.x2cpg.Defines.StaticInitMethodName,
      blockNode,
      typeRefIdStack.headOption
    )
    localAstParentStack.push(blockNode)

    val initAsts = staticMemberInits.map(
      astForClassMember(_, typeDeclNode, staticOwner = Some(owner))
    ) ++ staticMemberBlocks.map(astForNodeWithFunctionReference)

    methodAstParentStack.pop()
    localAstParentStack.pop()
    scope.popScope()

    val methodReturn = methodReturnNode(node, returnType)
    methodAst(methodNode_, Nil, Ast(blockNode).withChildren(initAsts), methodReturn, modifiers)
  }

  private def staticEnumInitMethodAst(
    node: BabelNode,
    typeDeclNode: NewTypeDecl,
    enumMembersWithInitializer: List[TSEnumMember],
    fullName: String,
    returnType: String,
    owner: MemberOwner
  ): Ast = {
    val modifiers =
      List(NewModifier().modifierType(ModifierTypes.STATIC), NewModifier().modifierType(ModifierTypes.CONSTRUCTOR))
    val blockNode   = NewBlock().typeFullName(Defines.Any)
    val methodNode_ = methodNode(node, io.joern.x2cpg.Defines.StaticInitMethodName, fullName, "", parserResult.filename)

    methodAstParentStack.push(methodNode_)
    scope.pushNewMethodScope(
      fullName,
      io.joern.x2cpg.Defines.StaticInitMethodName,
      blockNode,
      typeRefIdStack.headOption
    )
    localAstParentStack.push(blockNode)

    val initAsts         = enumMembersWithInitializer.flatMap(astsForEnumMember(_, owner))
    val (calls, members) = initAsts.partition(_.nodes.headOption.exists(_.isInstanceOf[NewCall]))

    members.foreach(m => diffGraph.addEdge(typeDeclNode, m.root.get, EdgeTypes.AST))

    methodAstParentStack.pop()
    localAstParentStack.pop()
    scope.popScope()

    val methodReturn = methodReturnNode(node, returnType)
    methodAst(methodNode_, Nil, Ast(blockNode).withChildren(calls), methodReturn, modifiers)
  }

  /** The type declaration of the class and its members are always created in the scope where the class is declared,
    * e.g. below of the namespace for a class of a namespace. Only the target of the synthetic assignment of the class
    * is different for a class of a namespace.
    *
    * @param classPath
    *   the path of the property that the class is assigned to instead of the variable that is named like the class, see
    *   [[astForModuleBlock]]. It is only used if `shouldCreateAssignmentCall` is set.
    */
  protected def astForClass(
    clazz: ClassDeclaration | ClassExpression | DeclareClass,
    shouldCreateAssignmentCall: Boolean = false,
    classPath: Option[List[String]] = None
  ): Ast = {
    val (typeName, typeFullName) = calcTypeNameAndFullName(clazz)
    registerType(typeFullName)

    val astParentType     = methodAstParentStack.head.label
    val astParentFullName = methodAstParentStack.head.properties(PropertyNames.FullName).toString

    val (superClass, implements, mixins) = clazz match {
      case c: ClassDeclaration => (c.superClass.map(_.code).toSeq, c.implements.map(_.code), c.mixins.toSeq.map(_.code))
      case c: ClassExpression  => (c.superClass.map(_.code).toSeq, c.implements.map(_.code), c.mixins.toSeq.map(_.code))
      case c: DeclareClass     => (Seq.empty[String], c.implements.map(_.code), c.mixins.map(_.code))
    }

    val typeDeclNode_ = typeDeclNode(
      clazz,
      typeName,
      typeFullName,
      parserResult.filename,
      s"class $typeName",
      astParentType,
      astParentFullName,
      inherits = superClass ++ implements ++ mixins
    )
    seenAliasTypes.update(typeDeclNode_.name, typeDeclNode_)

    addModifier(typeDeclNode_, clazz)
    astsForDecorators(clazz).foreach { decoratorAst =>
      Ast.storeInDiffGraph(decoratorAst, diffGraph)
      decoratorAst.root.foreach(diffGraph.addEdge(typeDeclNode_, _, EdgeTypes.AST))
    }

    diffGraph.addEdge(methodAstParentStack.head, typeDeclNode_, EdgeTypes.AST)

    val typeRefNode_ = typeRefNode(clazz, s"class $typeName", typeFullName)

    methodAstParentStack.push(typeDeclNode_)
    dynamicInstanceTypeStack.push(typeFullName)
    typeRefIdStack.push(typeRefNode_)

    scope.pushNewMethodScope(typeFullName, typeName, typeDeclNode_, None)

    // Decompose the class body once and reuse it for both the member sequence and the located constructor below.
    val decomposedMembers = decomposeClassMembers(clazz)
    val allClassMembers   = decomposedMembers.members(withConstructor = false).toList

    // adding all other members and retrieving their initialization calls
    val memberInitCalls =
      allClassMembers.filter(m => !isStaticMember(m) && (isInitializedMember(m) || isParameterProperty(m)))

    val constructor =
      createClassConstructor(
        clazz,
        decomposedMembers.constructor,
        ConstructorContent(Some(clazz), memberInitCalls, Some(typeDeclNode_))
      )
    val constructorNode = constructor.methodNode

    // static members live on the constructor function and not on the instances of the class
    val constructorTypeDecl = functionTypeDecls.getOrElse(constructorNode.fullName, typeDeclNode_)

    // adding all class methods / functions and uninitialized, non-static members
    allClassMembers
      .filter(m => isClassMethodOrUninitializedMember(m) && !isStaticMember(m) && !isParameterProperty(m))
      .foreach { member =>
        astForClassMember(member, if (isStaticMethod(member)) constructorTypeDecl else typeDeclNode_)
      }

    // adding all static members and retrieving their initialization calls
    val staticMemberInits = allClassMembers.filter(isStaticMember)

    // retrieving initialization calls from the static initialization block if any
    val staticInitBlocks =
      allClassMembers.collectFirst { case block: StaticBlock => block.body.toList }.getOrElse(List.empty)

    if (staticMemberInits.nonEmpty || staticInitBlocks.nonEmpty) {
      val init = staticClassInitMethodAst(
        clazz,
        constructorTypeDecl,
        staticMemberInits,
        staticInitBlocks,
        s"${constructorNode.fullName}:${io.joern.x2cpg.Defines.StaticInitMethodName}",
        Defines.Any,
        MemberOwner(typeName, constructorNode.fullName)
      )
      Ast.storeInDiffGraph(init, diffGraph)
      // the static initializer belongs to the constructor function, just like the static members it initializes
      diffGraph.addEdge(constructorTypeDecl, init.nodes.head, EdgeTypes.AST)
    }

    methodAstParentStack.pop()
    dynamicInstanceTypeStack.pop()
    typeRefIdStack.pop()
    scope.popScope()

    // a class evaluates to its constructor function and not to an instance of the class
    typeRefNode_.typeFullName(constructorNode.fullName)

    if (shouldCreateAssignmentCall) {
      // return a synthetic assignment to enable tracing of the implicitly created identifier for
      // the class definition assigned to its constructor
      // a reference to the class, with the type hint of its constructor if requested
      def classRef(withTypeHint: Boolean): Ast = classPath match {
        case Some(path) => astForModuleMemberAccess(clazz, path)
        case None       =>
          val typeHints = if (withTypeHint) {
            Seq(constructorNode.fullName)
          } else {
            Seq.empty
          }
          Ast(identifierNode(clazz, typeName, typeName, Defines.Any, typeHints))
      }

      val assignmentAst = classPath match {
        case Some(path) =>
          val targetAst = astForModuleMemberAccess(clazz, path)
          createAssignmentCallAst(
            targetAst,
            Ast(typeRefNode_),
            s"${codeOf(targetAst.nodes.head)} = ${constructorNode.fullName}",
            clazz.lineNumber,
            clazz.columnNumber
          )
        case None =>
          val classIdNode = identifierNode(clazz, typeName, typeName, Defines.Any, Seq(constructorNode.fullName))
          val idLocal     = localNode(clazz, typeName, typeName, Defines.Any).order(0)
          diffGraph.addEdge(localAstParentStack.head, idLocal, EdgeTypes.AST)
          scope.addVariable(typeName, idLocal, Defines.Any, VariableScopeManager.ScopeType.BlockScope)
          scope.addVariableReference(typeName, classIdNode, Defines.Any, EvaluationStrategies.BY_REFERENCE)
          createAssignmentCallAst(
            classIdNode,
            typeRefNode_,
            s"$typeName = ${constructorNode.fullName}",
            clazz.lineNumber,
            clazz.columnNumber
          )
      }

      val membersWithConstructor = decomposedMembers.members(withConstructor = true)
      val classDecorationAst     = createClassDecorationAst(clazz, classRef, classPath.isEmpty)
      val propertyDecorationAsts = createPropertyDecorationAsts(membersWithConstructor, classNodeInfo = clazz, classRef)
      val methodDecorationAsts   = createMethodDecorationAsts(membersWithConstructor, classNodeInfo = clazz, classRef)
      if (classDecorationAst.root.isDefined || propertyDecorationAsts.nonEmpty || methodDecorationAsts.nonEmpty) {
        val blockNode_   = blockNode(clazz)
        val childrenAsts = List(assignmentAst, classDecorationAst) ++ propertyDecorationAsts ++ methodDecorationAsts
        Ast(blockNode_).withChildren(childrenAsts)
      } else {
        val blockNode_ = blockNode(clazz)
        Ast(blockNode_).withChildren(List(assignmentAst))
      }
    } else {
      Ast(typeRefNode_)
    }
  }

  /** @param reassign
    *   whether the decorated class replaces the class, as it does if a decorator returns another class. A class that is
    *   a property of a namespace keeps its type for the resolution of its constructor.
    */
  private def createClassDecorationAst(
    classNodeInfo: BabelNode,
    classRefHandler: Boolean => Ast,
    reassign: Boolean
  ): Ast = {
    val decoratorAsts = decoratorExpressionElements(classNodeInfo).map(astForNodeWithFunctionReference)
    if (decoratorAsts.nonEmpty) {
      val lhsAst       = Option.when(reassign)(classRefHandler(true))
      val receiverNode = identifierNode(classNodeInfo, "__decorate")
      scope.addVariableReference(receiverNode.name, receiverNode, Defines.Any, EvaluationStrategies.BY_REFERENCE)
      val thisNode = identifierNode(classNodeInfo, "this").dynamicTypeHintFullName(rootTypeDecl.map(_.fullName).toSeq)
      scope.addVariableReference(thisNode.name, thisNode, Defines.Any, EvaluationStrategies.BY_REFERENCE)

      val classRefNode     = classRefHandler(true)
      val decorateCallNode = callNode(
        classNodeInfo,
        s"__decorate([${decoratorAsts.flatMap(_.root.map(codeOf)).mkString(",")}], ${codeOf(classRefNode.nodes.head)})",
        "__decorate",
        DispatchTypes.DYNAMIC_DISPATCH
      )

      val annotationExprAst = astForDecorateArray(classNodeInfo, decoratorAsts)
      val args              = Seq(annotationExprAst, classRefNode)
      val decorateCallAst   =
        callAst(decorateCallNode, args, receiver = Option(Ast(receiverNode)), base = Option(Ast(thisNode)))

      lhsAst match {
        case Some(lhs) =>
          createAssignmentCallAst(
            lhs,
            decorateCallAst,
            s"${codeOf(lhs.nodes.head)} = ${codeOf(decorateCallAst.nodes.head)}",
            classNodeInfo.lineNumber,
            classNodeInfo.columnNumber
          )
        case None => decorateCallAst
      }
    } else Ast()
  }

  private def createMetadataCallTypeAst(tsMethodNodeInfo: BabelNode): Ast = {
    val metadataCallRecNode = identifierNode(tsMethodNodeInfo, "__metadata")
    scope.addVariableReference(
      metadataCallRecNode.name,
      metadataCallRecNode,
      Defines.Any,
      EvaluationStrategies.BY_REFERENCE
    )
    val metadataCallRecThisNode =
      identifierNode(tsMethodNodeInfo, "this").dynamicTypeHintFullName(rootTypeDecl.map(_.fullName).toSeq)
    scope.addVariableReference(
      metadataCallRecThisNode.name,
      metadataCallRecThisNode,
      Defines.Any,
      EvaluationStrategies.BY_REFERENCE
    )

    val designTypeLiteralNode = literalNode(tsMethodNodeInfo, "'design:type'", Defines.String)
    val functionLiteralNode   = literalNode(tsMethodNodeInfo, "Function", Defines.Any)
    val metadataCallType      =
      callNode(
        tsMethodNodeInfo,
        s"""__metadata("design:type", Function)""",
        "__metadata",
        DispatchTypes.DYNAMIC_DISPATCH
      )
    val metadataTypeArgAsts = Seq(Ast(designTypeLiteralNode), Ast(functionLiteralNode))
    callAst(
      metadataCallType,
      metadataTypeArgAsts,
      receiver = Option(Ast(metadataCallRecNode)),
      base = Option(Ast(metadataCallRecThisNode))
    )
  }

  private def createMetadataCallParamTypesAst(tsMethodNodeInfo: BabelNode, numParams: Int): Ast = {
    val metadataCallRecNode = identifierNode(tsMethodNodeInfo, "__metadata")
    scope.addVariableReference(
      metadataCallRecNode.name,
      metadataCallRecNode,
      Defines.Any,
      EvaluationStrategies.BY_REFERENCE
    )
    val metadataCallThisNode =
      identifierNode(tsMethodNodeInfo, "this").dynamicTypeHintFullName(rootTypeDecl.map(_.fullName).toSeq)
    scope.addVariableReference(
      metadataCallThisNode.name,
      metadataCallThisNode,
      Defines.Any,
      EvaluationStrategies.BY_REFERENCE
    )

    val designParamTypeLiteralNode = literalNode(tsMethodNodeInfo, "'design:paramtypes'", Defines.String)

    val objectLiteralNode = literalNode(tsMethodNodeInfo, "Object", Defines.Object)

    val blockNode_ = blockNode(tsMethodNodeInfo)
    scope.pushNewBlockScope(blockNode_)
    localAstParentStack.push(blockNode_)

    val tmpName      = generateUnusedVariableName(usedVariableNames, "_tmp")
    val localTmpNode = localNode(tsMethodNodeInfo, tmpName, tmpName, Defines.Any).order(0)
    val tmpArrayNode = identifierNode(tsMethodNodeInfo, tmpName)
    diffGraph.addEdge(localAstParentStack.head, localTmpNode, EdgeTypes.AST)
    scope.addVariableReference(tmpName, tmpArrayNode, Defines.Any, EvaluationStrategies.BY_REFERENCE)

    val arrayCallNode =
      callNode(
        tsMethodNodeInfo,
        s"${EcmaBuiltins.arrayFactory}()",
        EcmaBuiltins.arrayFactory,
        DispatchTypes.STATIC_DISPATCH
      )

    val lineNumber                 = tsMethodNodeInfo.lineNumber
    val columnNumber               = tsMethodNodeInfo.columnNumber
    val assignmentCode             = s"${localTmpNode.code} = ${arrayCallNode.code}"
    val assignmentTmpArrayCallNode =
      createAssignmentCallAst(tmpArrayNode, arrayCallNode, assignmentCode, lineNumber, columnNumber)

    val elementAsts = (0 until numParams).toList.map { _ =>
      val objectLiteralNode = literalNode(tsMethodNodeInfo, "Object", Defines.Object)
      val pushCallNode      = callNode(tsMethodNodeInfo, s"$tmpName.push(Object)", "", DispatchTypes.DYNAMIC_DISPATCH)

      val baseNode     = identifierNode(tsMethodNodeInfo, tmpName)
      val memberNode   = fieldIdentifierNode(tsMethodNodeInfo, "push", "push")
      val receiverNode =
        createFieldAccessCallAst(baseNode, memberNode, tsMethodNodeInfo.lineNumber, tsMethodNodeInfo.columnNumber)
      val thisPushNode = identifierNode(tsMethodNodeInfo, tmpName)
      callAst(
        pushCallNode,
        List(Ast(objectLiteralNode)),
        receiver = Option(receiverNode),
        base = Option(Ast(thisPushNode))
      )
    }

    val tmpArrayReturnNode = identifierNode(tsMethodNodeInfo, tmpName)

    scope.popScope()
    localAstParentStack.pop()

    val blockChildrenAsts = assignmentTmpArrayCallNode +: elementAsts :+ Ast(tmpArrayReturnNode)
    val blockAst_         = blockAst(blockNode_, blockChildrenAsts)

    val metadataParamTypeCall =
      callNode(
        tsMethodNodeInfo,
        s"""__metadata("design:paramtypes", [${List.fill(numParams)("Object").mkString(",")}])""",
        "__metadata",
        DispatchTypes.DYNAMIC_DISPATCH
      )
    val metadataTypeArgAsts = Seq(Ast(designParamTypeLiteralNode), blockAst_)
    callAst(
      metadataParamTypeCall,
      metadataTypeArgAsts,
      receiver = Option(Ast(metadataCallRecNode)),
      base = Option(Ast(metadataCallThisNode))
    )
  }

  private def createMetadataCallReturnTypeAst(tsMethodNodeInfo: BabelNode, tpe: String): Ast = {
    val metadataCallRecNode = identifierNode(tsMethodNodeInfo, "__metadata")
    scope.addVariableReference(
      metadataCallRecNode.name,
      metadataCallRecNode,
      Defines.Any,
      EvaluationStrategies.BY_REFERENCE
    )
    val metadataCallTypeRecThisNode =
      identifierNode(tsMethodNodeInfo, "this").dynamicTypeHintFullName(rootTypeDecl.map(_.fullName).toSeq)
    scope.addVariableReference(
      metadataCallTypeRecThisNode.name,
      metadataCallTypeRecThisNode,
      Defines.Any,
      EvaluationStrategies.BY_REFERENCE
    )

    val designTypeLiteralNode = literalNode(tsMethodNodeInfo, "'design:returntype'", Defines.String)
    val tpeLiteralNode        = literalNode(tsMethodNodeInfo, tpe, Defines.Any)
    val metadataCallType      =
      callNode(tsMethodNodeInfo, s"""__metadata("design:type", $tpe)""", "__metadata", DispatchTypes.DYNAMIC_DISPATCH)
    val metadataTypeArgAsts = Seq(Ast(designTypeLiteralNode), Ast(tpeLiteralNode))
    callAst(
      metadataCallType,
      metadataTypeArgAsts,
      receiver = Option(Ast(metadataCallRecNode)),
      base = Option(Ast(metadataCallTypeRecThisNode))
    )
  }

  private def createPropertyDecorationAsts(
    members: Seq[BabelNode],
    classNodeInfo: BabelNode,
    classRefHandler: Boolean => Ast
  ): Seq[Ast] = {
    val tsProperties = members.collect {
      case property: (ClassProperty | ClassAccessorProperty | ClassPrivateProperty) => property
    }

    tsProperties.map { tsPropertyNodeInfo =>
      val decoratorAsts = decoratorExpressionElements(tsPropertyNodeInfo).map(astForNodeWithFunctionReference)
      if (decoratorAsts.nonEmpty) {
        val name = tsPropertyNodeInfo match {
          case property: ClassProperty         => property.key.code
          case property: ClassAccessorProperty => property.key.code
          case property: ClassPrivateProperty  => property.key.code
        }
        val propertyTpe = typeFor(tsPropertyNodeInfo)

        val receiverNode = identifierNode(classNodeInfo, "__decorate")
        scope.addVariableReference(receiverNode.name, receiverNode, Defines.Any, EvaluationStrategies.BY_REFERENCE)
        val thisNode = identifierNode(classNodeInfo, "this").dynamicTypeHintFullName(rootTypeDecl.map(_.fullName).toSeq)
        scope.addVariableReference(thisNode.name, thisNode, Defines.Any, EvaluationStrategies.BY_REFERENCE)

        val classPrototypeAccessAst = createFieldAccessCallAst(
          classRefHandler(false),
          fieldIdentifierNode(classNodeInfo, "prototype", "prototype"),
          classNodeInfo.lineNumber,
          classNodeInfo.columnNumber
        )
        val propertyNameNode = literalNode(tsPropertyNodeInfo, s"'$name'", Defines.String)
        val voidCallNode_    = voidCallNode(line(tsPropertyNodeInfo), column(tsPropertyNodeInfo))

        val arg1Code         = decoratorAsts.flatMap(_.root.map(codeOf)).mkString(",")
        val decorateCallCode =
          s"__decorate([$arg1Code], ${codeOf(classPrototypeAccessAst.nodes.head)}, ${propertyNameNode.code}, ${voidCallNode_.code})"

        val decorateCallNode = callNode(classNodeInfo, decorateCallCode, "__decorate", DispatchTypes.DYNAMIC_DISPATCH)
        val subAst           = astForDecorateArray(classNodeInfo, decoratorAsts)
        callAst(
          decorateCallNode,
          subAst +: Seq(classPrototypeAccessAst, Ast(propertyNameNode), Ast(voidCallNode_)),
          receiver = Option(Ast(receiverNode)),
          base = Option(Ast(thisNode))
        )
      } else {
        Ast()
      }
    }
  }

  private def createMethodDecorationAsts(
    members: Seq[BabelNode],
    classNodeInfo: BabelNode,
    classRefHandler: Boolean => Ast
  ): Seq[Ast] = {
    val tsMethods = members.collect { case method: ClassMethod => method }

    tsMethods.map { tsMethodNodeInfo =>
      val decoratorAsts      = decoratorExpressionElements(tsMethodNodeInfo).map(astForNodeWithFunctionReference)
      val (name, fullName)   = calcMethodNameAndFullName(tsMethodNodeInfo)
      val methodTpe          = typeFor(tsMethodNodeInfo).stripPrefix("__ecma.")
      val paramNodeInfos     = paramsOf(tsMethodNodeInfo)
      val paramDecoratorAsts = paramNodeInfos.zipWithIndex.flatMap { case (paramNodeInfo, idx) =>
        val paramAsts = decoratorExpressionElements(paramNodeInfo).map(astForNodeWithFunctionReference)

        if (paramAsts.isEmpty) {
          Seq.empty
        } else {
          paramAsts.map { paramAst =>
            val receiverNode = identifierNode(paramNodeInfo, "__param")
            scope.addVariableReference(receiverNode.name, receiverNode, Defines.Any, EvaluationStrategies.BY_REFERENCE)
            val thisNode =
              identifierNode(paramNodeInfo, "this").dynamicTypeHintFullName(rootTypeDecl.map(_.fullName).toSeq)
            scope.addVariableReference(thisNode.name, thisNode, Defines.Any, EvaluationStrategies.BY_REFERENCE)

            val callNode_ = callNode(
              paramNodeInfo,
              s"__param($idx, ${codeOf(paramAst.nodes.head)})",
              "__param",
              DispatchTypes.DYNAMIC_DISPATCH
            )
            val idxNode              = literalNode(paramNodeInfo, s"$idx", Defines.Number)
            val argAsts              = Seq(Ast(idxNode), paramAst)
            val paramDecorateCallAst =
              callAst(callNode_, argAsts, receiver = Option(Ast(receiverNode)), base = Option(Ast(thisNode)))
            paramDecorateCallAst
          }
        }
      }

      if (decoratorAsts.nonEmpty || paramDecoratorAsts.nonEmpty) {
        val receiverNode = identifierNode(classNodeInfo, "__decorate")
        scope.addVariableReference(receiverNode.name, receiverNode, Defines.Any, EvaluationStrategies.BY_REFERENCE)
        val thisNode = identifierNode(classNodeInfo, "this").dynamicTypeHintFullName(rootTypeDecl.map(_.fullName).toSeq)
        scope.addVariableReference(thisNode.name, thisNode, Defines.Any, EvaluationStrategies.BY_REFERENCE)

        val classPrototypeAccessAst = createFieldAccessCallAst(
          classRefHandler(false),
          fieldIdentifierNode(classNodeInfo, "prototype", "prototype"),
          classNodeInfo.lineNumber,
          classNodeInfo.columnNumber
        )
        val functionNameNode          = literalNode(tsMethodNodeInfo, s"'$name'", Defines.String)
        val nullNode                  = literalNode(tsMethodNodeInfo, "null", Defines.Null)
        val metadataCallTypeAst       = createMetadataCallTypeAst(tsMethodNodeInfo)
        val metadataCallParamTypesAst = createMetadataCallParamTypesAst(tsMethodNodeInfo, paramNodeInfos.size)
        val metadataCallReturnTypeAst = createMetadataCallReturnTypeAst(tsMethodNodeInfo, methodTpe)

        val arg1Code         = decoratorAsts.flatMap(_.root.map(codeOf)).mkString(",")
        val arg2Code         = paramDecoratorAsts.flatMap(_.root.map(codeOf)).mkString(",")
        val arg3Code         = codeOf(metadataCallTypeAst.root.get)
        val arg4Code         = codeOf(metadataCallParamTypesAst.root.get)
        val arg5Code         = codeOf(metadataCallReturnTypeAst.root.get)
        val decorateCallCode =
          s"__decorate([$arg1Code, $arg2Code, $arg3Code, $arg4Code, $arg5Code], ${codeOf(classPrototypeAccessAst.nodes.head)}, ${functionNameNode.code}, null)"

        val decorateCallNode = callNode(classNodeInfo, decorateCallCode, "__decorate", DispatchTypes.DYNAMIC_DISPATCH)
        val subAst           = astForDecorateArray(
          classNodeInfo,
          decoratorAsts ++ paramDecoratorAsts ++ Seq(
            metadataCallTypeAst,
            metadataCallParamTypesAst,
            metadataCallReturnTypeAst
          )
        )
        callAst(
          decorateCallNode,
          subAst +: Seq(classPrototypeAccessAst, Ast(functionNameNode), Ast(nullNode)),
          receiver = Option(Ast(receiverNode)),
          base = Option(Ast(thisNode))
        )
      } else {
        Ast()
      }
    }
  }

  private def astForDecorateArray(classNodeInfo: BabelNode, decoratorAsts: List[Ast]): Ast = {
    val blockNode_ = blockNode(classNodeInfo)
    scope.pushNewBlockScope(blockNode_)
    localAstParentStack.push(blockNode_)

    val tmpName      = generateUnusedVariableName(usedVariableNames, "_tmp")
    val localTmpNode = localNode(classNodeInfo, tmpName, tmpName, Defines.Any).order(0)
    val tmpArrayNode = identifierNode(classNodeInfo, tmpName)
    diffGraph.addEdge(localAstParentStack.head, localTmpNode, EdgeTypes.AST)
    scope.addVariableReference(tmpName, tmpArrayNode, Defines.Any, EvaluationStrategies.BY_REFERENCE)

    val arrayCallNode =
      callNode(
        classNodeInfo,
        s"${EcmaBuiltins.arrayFactory}()",
        EcmaBuiltins.arrayFactory,
        DispatchTypes.STATIC_DISPATCH
      )

    val lineNumber                 = classNodeInfo.lineNumber
    val columnNumber               = classNodeInfo.columnNumber
    val assignmentCode             = s"${localTmpNode.code} = ${arrayCallNode.code}"
    val assignmentTmpArrayCallNode =
      createAssignmentCallAst(tmpArrayNode, arrayCallNode, assignmentCode, lineNumber, columnNumber)

    val elementAsts = decoratorAsts.map { ast =>
      val elementCode  = ast.root.map(codeOf).getOrElse(PropertyDefaults.Code)
      val pushCallNode = callNode(classNodeInfo, s"$tmpName.push($elementCode)", "", DispatchTypes.DYNAMIC_DISPATCH)

      val baseNode     = identifierNode(classNodeInfo, tmpName)
      val memberNode   = fieldIdentifierNode(classNodeInfo, "push", "push")
      val receiverNode =
        createFieldAccessCallAst(baseNode, memberNode, classNodeInfo.lineNumber, classNodeInfo.columnNumber)
      val thisPushNode = identifierNode(classNodeInfo, tmpName)
      callAst(pushCallNode, List(ast), receiver = Option(receiverNode), base = Option(Ast(thisPushNode)))
    }

    val tmpArrayReturnNode = identifierNode(classNodeInfo, tmpName)

    scope.popScope()
    localAstParentStack.pop()

    val blockChildrenAsts = assignmentTmpArrayCallNode +: elementAsts :+ Ast(tmpArrayReturnNode)
    blockAst(blockNode_, blockChildrenAsts)
  }

  private def hasPrivateKey(member: BabelNode): Boolean = member match {
    case _: ClassPrivateProperty  => true
    case n: ClassAccessorProperty => n.key.isInstanceOf[PrivateName]
    case _                        => false
  }

  protected def addModifier(node: NewNode, member: BabelNode): Unit = {
    modifierTypesOf(member).foreach { modifierType =>
      diffGraph.addEdge(node, NewModifier().modifierType(modifierType), EdgeTypes.AST)
    }
  }

  /** The modifiers of a member in the order in which they are added to the CPG. A member with a private key, e.g. `#x`,
    * is private and has no other accessibility.
    */
  private def modifierTypesOf(member: BabelNode): List[String] = {
    val staticModifier = Option.when(staticOf(member))(ModifierTypes.STATIC)
    if (hasPrivateKey(member)) {
      List(staticModifier, Some(ModifierTypes.PRIVATE)).flatten
    } else {
      val accessModifier = accessibilityOf(member).collect {
        case "public"    => ModifierTypes.PUBLIC
        case "private"   => ModifierTypes.PRIVATE
        case "protected" => ModifierTypes.PROTECTED
      }
      List(
        Option.when(abstractOf(member))(ModifierTypes.ABSTRACT),
        staticModifier,
        Option.when(readonlyOf(member))(ModifierTypes.READONLY),
        accessModifier
      ).flatten
    }
  }

  /** The first and the remaining identifiers of a module name: `A.B.C` is one `TSQualifiedName`, a plain name or a
    * string is a single part.
    */
  protected def moduleNameParts(id: BabelNode): (BabelNode, List[BabelNode]) = {
    id match {
      case qualified: TSQualifiedName =>
        val (first, rest) = moduleNameParts(qualified.left)
        (first, rest :+ qualified.right)
      case other => (other, Nil)
    }
  }

  /** The module declarations that do not exist at runtime: `declare module "foo"`, `declare namespace A {}`,
    * `declare global {}` and everything inside of them.
    *
    * @param enclosingAmbient
    *   whether the declaration is inside of the body of an ambient module declaration
    */
  private def isAmbientModule(tsModuleDecl: TSModuleDeclaration, enclosingAmbient: Boolean): Boolean = {
    enclosingAmbient || tsModuleDecl.declare.contains(true) || tsModuleDecl.kind == "global" ||
    tsModuleDecl.id.isInstanceOf[StringLiteral]
  }

  /** The names of the values that the declaration of an `export` statement inside of a namespace adds to the namespace.
    * Types do not exist at runtime and are therefore not part of it.
    */
  private def exportedValueNames(declaration: BabelNode): Seq[String] = {
    declaration match {
      case _: (ClassDeclaration | FunctionDeclaration | TSEnumDeclaration | VariableDeclaration) =>
        codesForNode(declaration).distinct
      case importEquals: TSImportEqualsDeclaration =>
        Seq(importEquals.id.name)
      case module: TSModuleDeclaration if isInstantiatedModule(module) =>
        Seq(moduleNameParts(module.id)._1.code)
      case _ =>
        Seq.empty
    }
  }

  private def exportedDeclaration(statement: BabelNode): Option[BabelNode] = {
    statement match {
      case exportDecl: ExportNamedDeclaration => exportDecl.declaration
      case _                                  => None
    }
  }

  /** A namespace is instantiated, i.e. it is an object at runtime that can be referenced, if it exports a value. */
  protected def isInstantiatedModule(tsModuleDecl: TSModuleDeclaration, enclosingAmbient: Boolean = false): Boolean = {
    !isAmbientModule(tsModuleDecl, enclosingAmbient) && tsModuleDecl.body.exists(
      _.body.exists(exportedDeclaration(_).exists(exportedValueNames(_).nonEmpty))
    )
  }

  /** The object of a namespace. It is the property path `rootPath` followed by the nested namespaces `B` and `C` of
    * `namespace A.B.C {}`. The root is either the variable `A`, or a property of the object of the enclosing namespace.
    * `isNew` is false if the variable `A` already exists in scope, e.g. a class or an earlier namespace that is merged
    * with this one.
    */
  private case class ModuleBinding(rootPath: List[String], nested: List[String], isNew: Boolean) {
    def objectPath: List[String] = rootPath ++ nested
  }

  /** A namespace whose body is processed and the exported members that are accessed as properties of its object. */
  private case class ModuleScope(objectPath: List[String], members: Set[String])

  /** @param enclosingObjectPath
    *   the path of the object of the enclosing namespace if this namespace is exported from it
    * @param enclosingAmbient
    *   whether this namespace is inside of the body of an ambient module declaration
    */
  protected def astForModule(
    tsModuleDecl: TSModuleDeclaration,
    enclosingObjectPath: Option[List[String]] = None,
    enclosingAmbient: Boolean = false
  ): Ast = {
    val (first, nested) = moduleNameParts(tsModuleDecl.id)
    val ambient         = isAmbientModule(tsModuleDecl, enclosingAmbient)
    val binding         = if (isInstantiatedModule(tsModuleDecl, enclosingAmbient)) {
      Option(declareModuleBinding(tsModuleDecl, first, nested, enclosingObjectPath))
    } else { None }
    astForModulePart(tsModuleDecl, tsModuleDecl, first, nested, binding, ambient)
  }

  private def declareModuleBinding(
    tsModuleDecl: TSModuleDeclaration,
    first: BabelNode,
    nested: List[BabelNode],
    enclosingObjectPath: Option[List[String]]
  ): ModuleBinding = {
    val name = first.code
    enclosingObjectPath match {
      case Some(enclosingPath) => ModuleBinding(enclosingPath :+ name, nested.map(_.code), isNew = true)
      case None                =>
        val isNew = scope.lookupVariable(name).isEmpty
        if (isNew) {
          val localNode_ = localNode(tsModuleDecl, name, name, Defines.Any).order(0)
          scope.addVariable(name, localNode_, Defines.Any, VariableScopeManager.ScopeType.MethodScope)
          diffGraph.addEdge(localAstParentStack.head, localNode_, EdgeTypes.AST)
        }
        ModuleBinding(List(name), nested.map(_.code), isNew)
    }
  }

  /** `namespace A.B.C {}` is a single declaration, but stands for the namespaces A, B and C nested in each other. The
    * namespace of a `part` spans from the start of `from` (the declaration itself for the first part, else the part) to
    * the end of the declaration.
    */
  private def astForModulePart(
    tsModuleDecl: TSModuleDeclaration,
    from: BabelNode,
    part: BabelNode,
    nested: List[BabelNode],
    binding: Option[ModuleBinding],
    ambient: Boolean
  ): Ast = {
    val (name, fullName) = calcTypeNameAndFullName(tsModuleDecl, Some(part.code))
    registerType(fullName)

    val namespaceNode = NewNamespaceBlock()
      .code(codeBetween(from, tsModuleDecl))
      .lineNumber(from.lineNumber)
      .columnNumber(from.columnNumber)
      .filename(parserResult.filename)
      .name(name)
      .fullName(fullName)

    methodAstParentStack.push(namespaceNode)
    dynamicInstanceTypeStack.push(fullName)

    scope.pushNewMethodScope(fullName, name, namespaceNode, None)

    val blockAst = nested match {
      case next :: rest => astForModulePart(tsModuleDecl, next, next, rest, binding, ambient)
      // ambient module declarations like `declare module "foo";` have no body at all
      case Nil => tsModuleDecl.body.map(astForModuleBlock(tsModuleDecl, _, binding, ambient)).getOrElse(Ast())
    }

    methodAstParentStack.pop()
    dynamicInstanceTypeStack.pop()
    scope.popScope()

    Ast(namespaceNode).withChild(blockAst)
  }

  /** The body of a namespace. Its exported members are properties of the object of the namespace, and not variables
    * that are visible outside of the namespace:
    * {{{
    * namespace A.B.C { export class Foo {} }
    * }}}
    * is lowered to
    * {{{
    * A = { B: { C: {} } };
    * A.B.C.Foo = class Foo {};
    * }}}
    * The lowering is only about how the name is bound. The declaration itself stays inside of the namespace, and
    * `class Foo {}` above stands for the type reference of the class that is created for it:
    *   - the TYPE_DECL `...:A:B:C:Foo` and its methods are created while the scopes of the namespaces and of the class
    *     are active. Their full names are unchanged, and the TYPE_DECL is attached to the NAMESPACE_BLOCK of `C`.
    *   - the TYPE_REF to the class is the value of the assignment, which is part of the body of `C` and belongs to the
    *     method that contains the namespace, e.g. `:program`.
    *   - there is no variable named `Foo`. Code inside of a method of `Foo` that reads `A` captures it as any closure.
    *
    * A reference to an exported member inside the namespace is lowered to the access of the property, e.g. `A.B.C.Foo`.
    * Exported members that are not declared inline, i.e. variables that are declared by a pattern, are declared as
    * usual and then assigned to the property. If `A` already exists, e.g. a class or an earlier namespace, its object
    * is extended instead of replaced.
    *
    * The members that are not exported are only visible inside of the namespace. They are lowered the same way, but to
    * the properties of an object that only the namespace can reference:
    * {{{
    * namespace A { function helper() {} }
    * }}}
    * is lowered to
    * {{{
    * _private_0 = {};
    * _private_0.helper = function helper() {};
    * }}}
    */
  private def astForModuleBlock(
    tsModuleDecl: TSModuleDeclaration,
    block: TSModuleBlock,
    binding: Option[ModuleBinding],
    ambient: Boolean
  ): Ast = {
    val blockNode_ = blockNode(block, block.code, Defines.Any)
    scope.pushNewBlockScope(blockNode_)
    localAstParentStack.push(blockNode_)

    val declarations = block.body.map(statement => exportedDeclaration(statement).getOrElse(statement))
    val exported     = Collections.newSetFromMap(new util.IdentityHashMap[BabelNode, java.lang.Boolean]())
    block.body.flatMap(exportedDeclaration).foreach(exported.add)

    val inline = if (binding.isDefined) {
      declarations.filter(exported.contains).flatMap(inlineMemberNames)
    } else { Nil }
    val objectAsts   = binding.toList.flatMap(astsForModuleObjectCreation(tsModuleDecl, _))
    val privateNames = if (ambient) { Nil }
    else { declarations.filterNot(exported.contains).flatMap(inlineMemberNames) }

    val privatePath =
      Option.when(privateNames.nonEmpty)(List(declarePrivateModuleObject(tsModuleDecl)))

    val privateAsts =
      privatePath.toList.map(path => astForObjectAssignment(tsModuleDecl, path, astForObject(tsModuleDecl)))

    val objectPath = binding.map(_.objectPath).getOrElse(Nil)
    moduleScopes.push(ModuleScope(objectPath, inline.toSet))
    privatePath.foreach(path => moduleScopes.push(ModuleScope(path, privateNames.toSet)))
    val statementAsts = createBlockStatementAsts(
      declarations,
      declaration =>
        if (binding.isDefined && exported.contains(declaration)) {
          astForModuleMember(declaration, objectPath)
        } else if (privatePath.isDefined && inlineMemberNames(declaration).nonEmpty) {
          astForModuleMember(declaration, privatePath.get)
        } else {
          declaration match {
            case module: TSModuleDeclaration if ambient => astForModule(module, enclosingAmbient = true)
            case _                                      => astForNodeWithFunctionReferenceAndCall(declaration)
          }
        }
    )
    privatePath.foreach(_ => moduleScopes.pop())
    moduleScopes.pop()

    localAstParentStack.pop()
    scope.popScope()
    blockAst(blockNode_, objectAsts ++ privateAsts ++ statementAsts)
  }

  /** The variable that holds the object of the members of a namespace that are not exported. */
  private def declarePrivateModuleObject(tsModuleDecl: TSModuleDeclaration): String = {
    val name       = generateUnusedVariableName(usedVariableNames, "_private")
    val localNode_ = localNode(tsModuleDecl, name, name, Defines.Any).order(0)
    scope.addVariable(name, localNode_, Defines.Any, VariableScopeManager.ScopeType.BlockScope)
    diffGraph.addEdge(localAstParentStack.head, localNode_, EdgeTypes.AST)
    name
  }

  /** The names of the declarations of a namespace that are assigned inline, i.e. the property of an object is their
    * binding. The declarations themselves, e.g. the type declaration of a class, are still created inside of the
    * namespace, see [[astForModuleBlock]].
    */
  private def inlineMemberNames(declaration: BabelNode): Seq[String] = {
    declaration match {
      case clazz: ClassDeclaration if clazz.id.isDefined          => clazz.id.map(_.name).toSeq
      case function: FunctionDeclaration if function.id.isDefined => function.id.map(_.name).toSeq
      case variable: VariableDeclaration if variable.declarations.forall(_.id.isInstanceOf[Identifier]) =>
        codesForNode(variable).distinct
      case enumeration: TSEnumDeclaration                              => Seq(enumeration.id.code)
      case importEquals: TSImportEqualsDeclaration                     => Seq(importEquals.id.name)
      case module: TSModuleDeclaration if isInstantiatedModule(module) => Seq(moduleNameParts(module.id)._1.code)
      case _                                                           => Seq.empty
    }
  }

  /** The decorators of a class, of its members and of the parameters of its methods are part of the lowering of the
    * class that is declared by an assignment.
    */
  private def hasDecorators(clazz: ClassDeclaration): Boolean = {
    decoratorsOf(clazz).nonEmpty || clazz.body.body.exists {
      case method: ClassMethod => decoratorsOf(method).nonEmpty || paramsOf(method).exists(decoratorsOf(_).nonEmpty)
      case member              => decoratorsOf(member).nonEmpty
    }
  }

  /** A declaration of a namespace that is bound to a property of the object `objectPath` instead of a variable. The
    * declaration itself, e.g. the type declaration of a class, is created as usual inside of the namespace, and its
    * type reference, method reference or value is assigned to the property. The ones that are not assigned inline are
    * declared with a variable as usual and then copied to the property.
    */
  private def astForModuleMember(declaration: BabelNode, objectPath: List[String]): Ast = {
    def memberAssignment(name: String, valueAst: Ast): Ast = {
      astForObjectAssignment(declaration, objectPath :+ name, valueAst)
    }

    declaration match {
      case clazz: ClassDeclaration if inlineMemberNames(clazz).nonEmpty && !hasDecorators(clazz) =>
        memberAssignment(clazz.id.get.name, astForClass(clazz, shouldCreateAssignmentCall = false))
      case clazz: ClassDeclaration if inlineMemberNames(clazz).nonEmpty =>
        astForClass(clazz, shouldCreateAssignmentCall = true, Option(objectPath :+ clazz.id.get.name))
      case enumeration: TSEnumDeclaration =>
        memberAssignment(enumeration.id.code, astForEnum(enumeration))
      case importEquals: TSImportEqualsDeclaration =>
        astForTSImportEqualsDeclaration(importEquals, None, Option(objectPath :+ importEquals.id.name))
      case function: FunctionDeclaration if inlineMemberNames(function).nonEmpty =>
        val functionAst = astForFunctionDeclaration(function, shouldCreateFunctionReference = true)
        memberAssignment(function.id.get.name, functionAst)
      case variable: VariableDeclaration if inlineMemberNames(variable).nonEmpty =>
        val assignmentAsts = variable.declarations.toList.flatMap { declarator =>
          declarator.init.map(init => memberAssignment(declarator.id.code, astForNodeWithFunctionReference(init)))
        }
        blockAst(blockNode(variable, variable.code, Defines.Any), assignmentAsts)
      case module: TSModuleDeclaration if inlineMemberNames(module).nonEmpty =>
        astForModule(module, Option(objectPath))
      case other =>
        val declarationAst = astForNodeWithFunctionReferenceAndCall(other)
        val memberAsts     = exportedValueNames(other).map { name =>
          val identNode_ = identifierNode(other, name)
          scope.addVariableReference(name, identNode_, Defines.Any, EvaluationStrategies.BY_REFERENCE)
          memberAssignment(name, Ast(identNode_))
        }
        blockAst(blockNode(other, other.code, Defines.Any), declarationAst +: memberAsts.toList)
    }
  }

  private def astForObjectAssignment(node: BabelNode, targetPath: List[String], valueAst: Ast): Ast = {
    val targetAst = astForModuleMemberAccess(node, targetPath)
    createAssignmentCallAst(
      targetAst,
      valueAst,
      s"${codeOf(targetAst.nodes.head)} = ${codeOf(valueAst.nodes.head)}",
      node.lineNumber,
      node.columnNumber
    )
  }

  /** `A.B.C`: the root object `A` followed by the property accesses for the rest of the path. */
  protected def astForModuleMemberAccess(node: BabelNode, objectPath: List[String]): Ast = {
    val rootNode = identifierNode(node, objectPath.head)
    scope.addVariableReference(rootNode.name, rootNode, Defines.Any, EvaluationStrategies.BY_REFERENCE)
    objectPath.tail.foldLeft(Ast(rootNode)) { (baseAst, name) =>
      createFieldAccessCallAst(baseAst, fieldIdentifierNode(node, name, name), node.lineNumber, node.columnNumber)
    }
  }

  /** The reference to an exported member of an enclosing namespace, unless it is shadowed by a variable. */
  protected def astForModuleMemberReference(ident: Identifier): Option[Ast] = {
    if (scope.lookupVariable(ident.name).isDefined) { None }
    else {
      moduleScopes.iterator.find(_.members.contains(ident.name)).map { moduleScope =>
        astForModuleMemberAccess(ident, moduleScope.objectPath :+ ident.name)
      }
    }
  }

  /** `A = { B: { C: {} } }` for `namespace A.B.C {}`. If `A` exists already, only the nested namespaces are assigned:
    * `A.B = { C: {} }`.
    */
  private def astsForModuleObjectCreation(tsModuleDecl: TSModuleDeclaration, binding: ModuleBinding): List[Ast] = {
    val (targetPath, nestedPath) = if (binding.isNew) { (binding.rootPath, binding.nested) }
    else { (binding.rootPath ++ binding.nested.take(1), binding.nested.drop(1)) }

    if (!binding.isNew && binding.nested.isEmpty) { Nil }
    else {
      val valueAst = nestedPath.reverse.foldLeft(astForObject(tsModuleDecl)) { (inner, name) =>
        astForObject(tsModuleDecl, Option(name -> inner))
      }
      List(astForObjectAssignment(tsModuleDecl, targetPath, valueAst))
    }
  }

  /** An object literal that is lowered like the one that is written in the source, see [[astForObjectExpression]]. */
  private def astForObject(node: BabelNode, member: Option[(String, Ast)] = None): Ast = {
    val code = member.map { case (name, valueAst) => s"{ $name: ${codeOf(valueAst.nodes.head)} }" }.getOrElse("{}")
    val blockNode_ = blockNode(node, code, Defines.Any)
    scope.pushNewBlockScope(blockNode_)
    localAstParentStack.push(blockNode_)

    val tmpName      = generateUnusedVariableName(usedVariableNames, "_tmp")
    val localTmpNode = localNode(node, tmpName, tmpName, Defines.Any).order(0)
    diffGraph.addEdge(localAstParentStack.head, localTmpNode, EdgeTypes.AST)

    val propertyAsts = member.toList.map { case (name, valueAst) =>
      val memberAccess = createFieldAccessCallAst(
        identifierNode(node, tmpName),
        fieldIdentifierNode(node, name, name),
        node.lineNumber,
        node.columnNumber
      )
      val assignmentCode = s"${codeOf(memberAccess.nodes.head)} = ${codeOf(valueAst.nodes.head)}"
      createAssignmentCallAst(memberAccess, valueAst, assignmentCode, node.lineNumber, node.columnNumber)
    }
    val tmpNode = identifierNode(node, tmpName)

    scope.popScope()
    localAstParentStack.pop()
    blockAst(blockNode_, propertyAsts :+ Ast(tmpNode))
  }

  protected def astForInterface(tsInterface: TSInterfaceDeclaration): Ast = {
    val (typeName, typeFullName) = calcTypeNameAndFullName(tsInterface)
    registerType(typeFullName)

    val astParentType     = methodAstParentStack.head.label
    val astParentFullName = methodAstParentStack.head.properties(PropertyNames.FullName).toString

    val extendz = tsInterface.`extends`.map(_.code)

    val typeDeclNode_ = typeDeclNode(
      tsInterface,
      typeName,
      typeFullName,
      parserResult.filename,
      s"interface $typeName",
      astParentType,
      astParentFullName,
      inherits = extendz
    )
    seenAliasTypes.update(typeDeclNode_.name, typeDeclNode_)

    addModifier(typeDeclNode_, tsInterface)

    methodAstParentStack.push(typeDeclNode_)
    dynamicInstanceTypeStack.push(typeFullName)

    scope.pushNewMethodScope(typeFullName, typeName, typeDeclNode_, None)

    val constructorNode = interfaceConstructor(typeName, tsInterface)
    diffGraph.addEdge(constructorNode, NewModifier().modifierType(ModifierTypes.CONSTRUCTOR), EdgeTypes.AST)

    val memberNode_ =
      memberNode(tsInterface, constructorNode.name, constructorNode.code, typeFullName, Seq(constructorNode.fullName))
    diffGraph.addEdge(typeDeclNode_, memberNode_, EdgeTypes.AST)

    val interfaceBodyElements = classMembers(tsInterface, withConstructor = false)

    interfaceBodyElements.foreach { classElement =>
      val tpe           = typeFor(classElement)
      val possibleTypes = Seq(tpe)
      val typeFullName  = if (Defines.isBuiltinType(tpe)) tpe else Defines.Any
      val memberNodes   = classElement match {
        case function: (TSCallSignatureDeclaration | TSMethodSignature) =>
          val functionNode = createMethodDefinitionNode(function, ConstructorContent.empty)
          addModifier(functionNode, function)
          Seq(
            memberNode(function, functionNode.name, function.code, typeFullName, Seq(functionNode.fullName))
              .possibleTypes(possibleTypes)
          )
        case _ =>
          val names = classElement match {
            case property: TSPropertySignature =>
              property.key match {
                case literal: StringLiteral => Seq(literal.value)
                case key                    => Seq(key.code)
              }
            case index: TSIndexSignature => index.parameters.map(_.name)
            // TODO: name field most likely needs adjustment for other Babel AST types
            case _ => Seq(classElement.code)
          }
          names.map { n =>
            val node = memberNode(classElement, n, classElement.code, typeFullName).possibleTypes(possibleTypes)
            addModifier(node, classElement)
            node
          }
      }
      memberNodes.foreach(diffGraph.addEdge(typeDeclNode_, _, EdgeTypes.AST))
    }

    methodAstParentStack.pop()
    dynamicInstanceTypeStack.pop()
    scope.popScope()

    Ast(typeDeclNode_)
  }

}
