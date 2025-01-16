package io.joern.javasrc2cpg.astcreation.declarations

import com.github.javaparser.ast.body.{
  AnnotationDeclaration,
  AnnotationMemberDeclaration,
  BodyDeclaration,
  ClassOrInterfaceDeclaration,
  CompactConstructorDeclaration,
  ConstructorDeclaration,
  EnumConstantDeclaration,
  EnumDeclaration,
  FieldDeclaration,
  InitializerDeclaration,
  MethodDeclaration,
  RecordDeclaration,
  TypeDeclaration,
  VariableDeclarator
}
import com.github.javaparser.ast.expr.{
  AnnotationExpr,
  ArrayInitializerExpr,
  BinaryExpr,
  BooleanLiteralExpr,
  CharLiteralExpr,
  ClassExpr,
  DoubleLiteralExpr,
  Expression,
  FieldAccessExpr,
  IntegerLiteralExpr,
  LiteralExpr,
  LongLiteralExpr,
  MarkerAnnotationExpr,
  NameExpr,
  NormalAnnotationExpr,
  NullLiteralExpr,
  SingleMemberAnnotationExpr,
  StringLiteralExpr,
  TextBlockLiteralExpr
}
import com.github.javaparser.resolution.declarations.{
  ResolvedReferenceTypeDeclaration,
  ResolvedTypeParameterDeclaration
}
import io.joern.javasrc2cpg.astcreation.{AstCreator, ExpectedType}
import io.joern.javasrc2cpg.typesolvers.TypeInfoCalculator.TypeConstants
import io.joern.javasrc2cpg.util.{BindingTable, BindingTableEntry, NameConstants, Util}
import io.joern.x2cpg.utils.NodeBuilders.*
import io.joern.x2cpg.{Ast, Defines}
import io.shiftleft.codepropertygraph.generated.nodes.{
  NewArrayInitializer,
  NewIdentifier,
  NewMethod,
  NewModifier,
  NewTypeDecl
}
import io.shiftleft.codepropertygraph.generated.{ModifierTypes, NodeTypes}
import org.slf4j.LoggerFactory

import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.util.{Success, Try}
import com.github.javaparser.ast.expr.ObjectCreationExpr
import com.github.javaparser.ast.stmt.LocalClassDeclarationStmt
import io.joern.javasrc2cpg.scope.Scope.ScopeVariable
import com.github.javaparser.ast.Node
import com.github.javaparser.resolution.types.ResolvedReferenceType
import com.github.javaparser.resolution.types.parametrization.ResolvedTypeParametersMap
import io.shiftleft.codepropertygraph.generated.nodes.NewCall
import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.joern.javasrc2cpg.scope.JavaScopeElement.PartialInit
import io.joern.javasrc2cpg.util.MultiBindingTableAdapterForJavaparser.{
  InnerClassDeclaration,
  JavaparserBindingDeclType,
  RegularClassDeclaration
}
import io.shiftleft.codepropertygraph.generated.nodes.ExpressionNew

import scala.jdk.OptionConverters.RichOptional

object AstForTypeDeclsCreator {
  case class AstWithStaticInit(ast: Seq[Ast], staticInits: Seq[Ast])
  object AstWithStaticInit {
    val empty: AstWithStaticInit = AstWithStaticInit(Seq.empty, Seq.empty)

    def apply(ast: Ast): AstWithStaticInit = {
      AstWithStaticInit(Seq(ast), staticInits = Seq.empty)
    }
  }
}

private[declarations] trait AstForTypeDeclsCreator { this: AstCreator =>
  private val logger = LoggerFactory.getLogger(this.getClass)

  private def outerClassGenericSignature: Option[String] = {
    scope.enclosingTypeDecl.map(decl => binarySignatureCalculator.variableBinarySignature(decl.typeDecl.name))
  }

  def astForAnonymousClassDecl(
    expr: ObjectCreationExpr,
    body: List[BodyDeclaration[?]],
    typeName: String,
    typeFullName: Option[String],
    baseTypeFullName: Option[String]
  ): Ast = {
    val (astParentType, astParentFullName) = getAstParentInfo()

    val genericSignature = binarySignatureCalculator.variableBinarySignature(expr.getType)
    val typeDeclRoot =
      typeDeclNode(
        expr,
        typeName,
        typeFullName.getOrElse(typeName),
        filename,
        expr.toString(),
        astParentType,
        astParentFullName,
        baseTypeFullName.getOrElse(TypeConstants.Object) :: Nil,
        genericSignature = Option(genericSignature)
      )

    typeFullName.foreach(typeFullName => scope.addInnerType(typeName, typeFullName, typeFullName))

    val declaredMethodNames = body.collect { case methodDeclaration: MethodDeclaration =>
      methodDeclaration.getNameAsString
    }.toSet

    scope.pushTypeDeclScope(
      typeDeclRoot,
      scope.isEnclosingScopeStatic,
      outerClassGenericSignature,
      declaredMethodNames,
      Nil
    )
    val memberAsts = astsForTypeDeclMembers(expr, body, isInterface = false, typeFullName)

    val localDecls    = scope.localDeclsInScope
    val lambdaMethods = scope.lambdaMethodsInScope

    val declScope = scope.popTypeDeclScope()
    scope.enclosingTypeDecl.foreach(_.registerCapturesForType(declScope.typeDecl.fullName, declScope.getUsedCaptures()))

    tryWithSafeStackOverflow(expr.getType.resolve().asReferenceType()).toOption.foreach { ancestorType =>
      val parentType = bindingTypeForReferenceType(ancestorType)
      val resolvedMethods = body
        .collect { case method: MethodDeclaration =>
          tryWithSafeStackOverflow(method.resolve())
        }
        .collect { case Success(resolvedMethod) =>
          resolvedMethod
        }
      val bindingType = InnerClassDeclaration(
        typeFullName.getOrElse(typeName),
        parentType.toList,
        resolvedMethods,
        ancestorType.typeParametersMap()
      )

      val bindingTable = getMultiBindingTable(bindingType)
      declScope.getBindingTableEntries.foreach(bindingTable.add)
      BindingTable.createBindingNodes(diffGraph, typeDeclRoot, bindingTable)
    }

    Ast(typeDeclRoot)
      .withChildren(memberAsts)
      .withChildren(localDecls)
      .withChildren(lambdaMethods)
  }

  def astForLocalClassDeclaration(localClassDecl: LocalClassDeclarationStmt): Ast = {
    val name                  = localClassDecl.getClassDeclaration.getNameAsString
    val enclosingMethodPrefix = scope.enclosingMethod.getMethodFullName.takeWhile(_ != ':')
    val fullName              = s"$enclosingMethodPrefix.$name"
    scope.addInnerType(name, fullName, fullName)
    astForTypeDeclaration(localClassDecl.getClassDeclaration, fullNameOverride = Some(fullName), isLocalClass = true)
  }

  def astForTypeDeclaration(
    typeDeclaration: TypeDeclaration[?],
    fullNameOverride: Option[String] = None,
    isLocalClass: Boolean = false
  ): Ast = {

    val isInterface = typeDeclaration match {
      case classDeclaration: ClassOrInterfaceDeclaration => classDeclaration.isInterface
      case _                                             => false
    }

    val (astParentType, astParentFullName) = getAstParentInfo()

    val typeDeclRoot =
      createTypeDeclNode(typeDeclaration, astParentType, astParentFullName, isInterface, fullNameOverride)

    // If this is a nested type (which must be true if an enclosing decl exists at this point), then the internal name
    // of the class, e.g. Foo$Bar must be added to the scope to make lookups for type Bar possible.
    scope.enclosingTypeDecl.foreach { _ =>
      if (!isLocalClass)
        scope.addInnerType(typeDeclaration.getNameAsString, typeDeclRoot.fullName, typeDeclRoot.name)
    }

    val declaredMethodNames = typeDeclaration.getMethods.asScala.map(_.getNameAsString).toSet

    val recordParameters = typeDeclaration match {
      case recordDeclaration: RecordDeclaration => recordDeclaration.getParameters.asScala.toList
      case _                                    => Nil
    }

    scope.pushTypeDeclScope(
      typeDeclRoot,
      typeDeclaration.isStatic,
      outerClassGenericSignature,
      declaredMethodNames,
      recordParameters
    )
    addTypeDeclTypeParamsToScope(typeDeclaration)

    val recordParameterAsts = typeDeclaration match {
      case recordDeclaration: RecordDeclaration => astsForRecordParameters(recordDeclaration, typeDeclRoot.fullName)
      case _                                    => Nil
    }

    val annotationAsts = typeDeclaration.getAnnotations.asScala.map(astForAnnotationExpr)
    val modifiers      = modifiersForTypeDecl(typeDeclaration, isInterface)
    val enumEntries = typeDeclaration match {
      case enumDeclaration: EnumDeclaration => enumDeclaration.getEntries.asScala.toList
      case _                                => Nil
    }

    val memberAsts =
      astsForTypeDeclMembers(
        typeDeclaration,
        enumEntries ++ typeDeclaration.getMembers.asScala.toList,
        isInterface,
        fullNameOverride
      )

    val localDecls    = scope.localDeclsInScope
    val lambdaMethods = scope.lambdaMethodsInScope

    val typeDeclAst = Ast(typeDeclRoot)
      .withChildren(recordParameterAsts)
      .withChildren(memberAsts)
      .withChildren(annotationAsts)
      .withChildren(localDecls)
      .withChildren(lambdaMethods)
      .withChildren(modifiers.map(Ast(_)))

    val declScope = scope.popTypeDeclScope()
    scope.enclosingTypeDecl.foreach(_.registerCapturesForType(declScope.typeDecl.fullName, declScope.getUsedCaptures()))

    // Annotation declarations need no binding table as objects of this
    // typ never get called from user code.
    // Furthermore the parser library throws an exception when trying to
    // access e.g. the declared methods of an annotation declaration.
    if (!typeDeclaration.isInstanceOf[AnnotationDeclaration]) {
      tryWithSafeStackOverflow(typeDeclaration.resolve()).toOption.foreach { resolvedTypeDecl =>
        val bindingTable = fullNameOverride match {
          case None =>
            getBindingTable(resolvedTypeDecl)

          case Some(fullName) =>
            val declBindingType = bindingTypeForLocalClass(fullName, resolvedTypeDecl)
            scope.addDeclBinding(resolvedTypeDecl.getName, declBindingType)
            getMultiBindingTable(declBindingType)

        }

        declScope.getBindingTableEntries.foreach(bindingTable.add)
        BindingTable.createBindingNodes(diffGraph, typeDeclRoot, bindingTable)
      }
    }

    typeDeclAst
  }

  private def astsForRecordParameters(recordDeclaration: RecordDeclaration, recordTypeFullName: String): List[Ast] = {
    val explicitMethodNames = recordDeclaration.getMethods.asScala.map(_.getNameAsString).toSet

    recordDeclaration.getParameters.asScala.toList.flatMap { parameter =>
      val parameterName = parameter.getNameAsString
      val parameterTypeFullName = tryWithSafeStackOverflow {
        val typ = parameter.getType
        scope
          .lookupScopeType(typ.asString())
          .map(_.typeFullName)
          .orElse(typeInfoCalc.fullName(typ))
          .getOrElse(defaultTypeFallback(typ))
      }.toOption.getOrElse(defaultTypeFallback())

      val genericSignature = binarySignatureCalculator.variableBinarySignature(parameter.getType)
      val parameterMember = memberNode(
        parameter,
        parameterName,
        code(parameter),
        parameterTypeFullName,
        genericSignature = Option(genericSignature)
      )
      val privateModifier = newModifierNode(ModifierTypes.PRIVATE)
      val memberAst       = Ast(parameterMember).withChild(Ast(privateModifier))

      val accessorMethodAst = Option.unless(explicitMethodNames.contains(parameterName))(
        astForRecordParameterAccessor(parameter, recordTypeFullName, parameterName, parameterTypeFullName)
      )

      memberAst :: accessorMethodAst.toList
    }
  }

  private def bindingTypeForReferenceType(typ: ResolvedReferenceType): Option[JavaparserBindingDeclType] = {
    typ.getTypeDeclaration.toScala.map(typeDecl =>
      scope.getDeclBinding(typeDecl.getName) match {
        case None => RegularClassDeclaration(typeDecl, typ.typeParametersMap())

        case Some(regularClass: RegularClassDeclaration) =>
          regularClass.copy(typeParametersMap = typ.typeParametersMap())

        case Some(innerClass: InnerClassDeclaration) => innerClass.copy(typeParametersMap = typ.typeParametersMap())

      }
    )
  }

  private def bindingTypeForLocalClass(
    fullName: String,
    typeDeclaration: ResolvedReferenceTypeDeclaration
  ): JavaparserBindingDeclType = {
    val directParents = Util.safeGetAncestors(typeDeclaration).flatMap(bindingTypeForReferenceType)

    InnerClassDeclaration(
      fullName,
      directParents.toList,
      typeDeclaration.getDeclaredMethods.asScala.toList,
      ResolvedTypeParametersMap.empty()
    )
  }

  private def getAstParentInfo(): (String, String) = {
    scope.enclosingTypeDecl
      .map { scope =>
        (NodeTypes.TYPE_DECL, scope.typeDecl.fullName)
      }
      .orElse {
        scope.enclosingNamespace.map { scope =>
          (NodeTypes.NAMESPACE_BLOCK, scope.namespace.fullName)
        }
      }
      .getOrElse((NameConstants.Unknown, NameConstants.Unknown))
  }

  private def getTypeDeclNameAndFullName(
    typeDecl: TypeDeclaration[?],
    fullNameOverride: Option[String]
  ): (String, String) = {
    val resolvedType    = tryWithSafeStackOverflow(typeDecl.resolve()).toOption
    val defaultFullName = s"${Defines.UnresolvedNamespace}.${typeDecl.getNameAsString}"
    val name            = resolvedType.flatMap(typeInfoCalc.name).getOrElse(typeDecl.getNameAsString)
    val fullName = fullNameOverride.orElse(resolvedType.flatMap(typeInfoCalc.fullName)).getOrElse(defaultFullName)

    (name, fullName)
  }

  private def astsForTypeDeclMembers(
    originNode: Node,
    members: List[BodyDeclaration[?]],
    isInterface: Boolean,
    fullNameOverride: Option[String]
  ): List[Ast] = {
    members.collect { case typeDeclaration: TypeDeclaration[_] =>
      val (name, fullName) = getTypeDeclNameAndFullName(typeDeclaration, fullNameOverride)

      scope.addInnerType(name, fullName, fullName)
    }

    val fields = members.collect { case fieldDeclaration: FieldDeclaration => fieldDeclaration }

    val fieldToFieldAsts = members.collect { case fieldDeclaration: FieldDeclaration =>
      val result =
        fieldDeclaration -> fieldDeclaration.getVariables.asScala.map { variable =>
          val fieldAst = astForFieldVariable(variable, fieldDeclaration)
          fieldAst
        }.toList
      result
    }.toMap

    val (staticFields, instanceFields) = fields.partition(_.isStatic)
    val staticFieldInitializers        = getStaticFieldInitializers(staticFields)

    val clinitAst = clinitAstFromStaticInits(staticFieldInitializers)

    val membersAstPairs: List[(Node, List[Ast])] = members.map { member =>
      val memberAsts = member match {
        case annotationMember: AnnotationMemberDeclaration =>
          // TODO: Add support for this
          Nil

        case constructor: ConstructorDeclaration =>
          // Added later to create params for captures
          Nil

        case method: MethodDeclaration =>
          astForMethod(method) :: Nil

        case compactConstructorDeclaration: CompactConstructorDeclaration =>
          // TODO: Add this when adding records
          Nil

        case enumConstantDeclaration: EnumConstantDeclaration =>
          // TODO: Create initializers
          astForEnumEntry(enumConstantDeclaration) :: Nil

        case field: FieldDeclaration => fieldToFieldAsts.getOrElse(field, Nil)

        case initialiserDeclaration: InitializerDeclaration =>
          // Handled with field initialisers
          Nil

        case typeDeclaration: TypeDeclaration[_] =>
          astForTypeDeclaration(typeDeclaration) :: Nil
      }
      (member, memberAsts)
    }

    val constructorAstMap = astsForConstructors(
      members.collect {
        case constructor: ConstructorDeclaration        => constructor
        case constructor: CompactConstructorDeclaration => constructor
      },
      instanceFields
    )

    val membersAsts = membersAstPairs.flatMap {
      case (constructor: ConstructorDeclaration, _)        => constructorAstMap.get(constructor)
      case (constructor: CompactConstructorDeclaration, _) => constructorAstMap.get(constructor)
      case (_, asts)                                       => asts
    }

    val hasCanonicalConstructor = scope.enclosingTypeDecl.get.recordParameters match {
      case Nil => members.exists(member => member.isConstructorDeclaration || member.isCompactConstructorDeclaration)

      case recordParameters =>
        members.collect {
          case compactConstructorDeclaration: CompactConstructorDeclaration => compactConstructorDeclaration

          case constructorDeclaration: ConstructorDeclaration
              if constructorDeclaration.getParameters.asScala
                .map(_.getType)
                .toList
                .equals(recordParameters.map(_.getType)) =>
            constructorDeclaration
        }.nonEmpty
    }
    val defaultConstructorAst = Option.when(!(isInterface || hasCanonicalConstructor)) {
      astForDefaultConstructor(originNode, instanceFields)
    }

    (defaultConstructorAst.toList ++ constructorAstMap.values)
      .flatMap(_.root)
      .collect { case constructorRoot: NewMethod =>
        BindingTableEntry(
          io.joern.x2cpg.Defines.ConstructorMethodName,
          constructorRoot.signature,
          constructorRoot.fullName
        )
      }
      .foreach { bindingTableEntry =>
        scope.enclosingTypeDecl.foreach(_.addBindingTableEntry(bindingTableEntry))
      }

    val capturedMembersAsts = membersForCapturedVariables(originNode, scope.enclosingTypeDecl.getUsedCaptures())

    val allMembersAsts = List(capturedMembersAsts, membersAsts, defaultConstructorAst.toList, clinitAst.toList).flatten
    val allInitCallNodes = (allMembersAsts ++ scope.enclosingTypeDecl.map(_.registeredLambdaMethods).getOrElse(Nil))
      .flatMap(_.nodes)
      .collect { case call: NewCall if call.name == "<init>" => call }
      .toSet

    addArgsToPartialInits(allInitCallNodes)

    allMembersAsts
  }

  private def addArgsToPartialInits(allInitCallNodes: Set[NewCall]): Unit = {
    scope.enclosingTypeDecl.getInitsToComplete.foreach {
      case PartialInit(typeFullName, callAst, receiverAst, args, outerClassAst) =>
        callAst.root match {
          case Some(initRoot: NewCall) if allInitCallNodes.contains(initRoot) =>
            val usedCaptures = if (scope.enclosingTypeDecl.map(_.typeDecl.fullName).contains(typeFullName)) {
              scope.enclosingTypeDecl.getUsedCaptures()
            } else {
              scope.enclosingTypeDecl.getCapturesForType(typeFullName)
            }

            receiverAst.root.foreach(receiver => diffGraph.addEdge(initRoot, receiver, EdgeTypes.RECEIVER))

            val capturesAsts =
              usedCaptures
                .filterNot(outerClassAst.isDefined && _.name == NameConstants.OuterClass)
                .zipWithIndex
                .map { (usedCapture, index) =>
                  val identifier = NewIdentifier()
                    .name(usedCapture.name)
                    .code(usedCapture.name)
                    .typeFullName(usedCapture.typeFullName)
                    .lineNumber(initRoot.lineNumber)
                    .columnNumber(initRoot.columnNumber)

                  val refsTo = Option.when(usedCapture.name != NameConstants.OuterClass)(usedCapture.node)

                  Ast(identifier).withRefEdges(identifier, refsTo.toList)
                }

            (receiverAst :: args ++ outerClassAst.toList ++ capturesAsts)
              .map { argAst =>
                storeInDiffGraph(argAst)
                argAst.root
              }
              .collect { case Some(expression: ExpressionNew) =>
                expression
              }
              .zipWithIndex
              .foreach { (argRoot, index) =>
                argRoot.argumentIndex_=(index)
                argRoot.order_=(index + 1)

                diffGraph.addEdge(initRoot, argRoot, EdgeTypes.AST)
                diffGraph.addEdge(initRoot, argRoot, EdgeTypes.ARGUMENT)
              }

          case _ => // Do nothing to avoid adding unused inits to the graph
        }
    }
  }

  private def membersForCapturedVariables(originNode: Node, captures: List[ScopeVariable]): List[Ast] = {
    captures.map { variable =>
      val node = memberNode(
        originNode,
        variable.name,
        variable.name,
        variable.typeFullName,
        genericSignature = Option(variable.genericSignature)
      )
      Ast(node)
    }
  }

  private def getStaticFieldInitializers(staticFields: List[FieldDeclaration]): List[Ast] = {
    scope.pushMethodScope(NewMethod(), ExpectedType.empty, isStatic = true)
    val fieldsAsts = staticFields.flatMap { field =>
      field.getVariables.asScala.toList.flatMap { variable =>
        scope.pushFieldDeclScope(isStatic = true, name = variable.getNameAsString)
        val assignment = astsForVariableDeclarator(variable, field)
        scope.popFieldDeclScope()
        assignment
      }
    }
    val methodScope = scope.popMethodScope()
    methodScope.getTemporaryLocals.map(Ast(_)) ++ methodScope
      .getUnaddedPatternVariableAstsAndMarkAdded() ++ fieldsAsts
  }

  private[declarations] def astForAnnotationExpr(annotationExpr: AnnotationExpr): Ast = {
    val fullName = scope
      .lookupType(annotationExpr.getNameAsString)
      .orElse(tryWithSafeStackOverflow(annotationExpr.resolve()).toOption.flatMap(typeInfoCalc.fullName))
      .getOrElse(defaultTypeFallback(annotationExpr.getNameAsString))
    typeInfoCalc.registerType(fullName)
    val code = annotationExpr.toString
    val name = annotationExpr.getName.getIdentifier
    val node = annotationNode(annotationExpr, code, name, fullName)
    annotationExpr match {
      case _: MarkerAnnotationExpr =>
        annotationAst(node, List.empty)
      case normal: NormalAnnotationExpr =>
        val assignmentAsts = normal.getPairs.asScala.toList.map { pair =>
          annotationAssignmentAst(
            pair.getName.getIdentifier,
            pair.toString,
            convertAnnotationValueExpr(pair.getValue).getOrElse(Ast())
          )
        }
        annotationAst(node, assignmentAsts)
      case single: SingleMemberAnnotationExpr =>
        val assignmentAsts = List(
          annotationAssignmentAst(
            "value",
            single.getMemberValue.toString,
            convertAnnotationValueExpr(single.getMemberValue).getOrElse(Ast())
          )
        )
        annotationAst(node, assignmentAsts)
    }
  }

  private def convertAnnotationValueExpr(expr: Expression): Option[Ast] = {
    expr match {
      case arrayInit: ArrayInitializerExpr =>
        val arrayInitNode = NewArrayInitializer()
          .code(arrayInit.toString)
        val initElementAsts = arrayInit.getValues.asScala.toList.map { value =>
          convertAnnotationValueExpr(value)
        }

        setArgumentIndices(initElementAsts.flatten)

        val returnAst = initElementAsts.foldLeft(Ast(arrayInitNode)) {
          case (ast, Some(elementAst)) =>
            ast.withChild(elementAst)
          case (ast, _) => ast
        }
        Some(returnAst)

      case annotationExpr: AnnotationExpr =>
        Some(astForAnnotationExpr(annotationExpr))

      case literalExpr: LiteralExpr =>
        Some(astForAnnotationLiteralExpr(literalExpr))

      case _: ClassExpr =>
        // TODO: Implement for known case
        None

      case _: FieldAccessExpr =>
        // TODO: Implement for known case
        None

      case _: BinaryExpr =>
        // TODO: Implement for known case
        None

      case _: NameExpr =>
        // TODO: Implement for known case
        None

      case _ =>
        logger.info(s"convertAnnotationValueExpr not yet implemented for unknown case ${expr.getClass}")
        None
    }
  }

  private def astForAnnotationLiteralExpr(literalExpr: LiteralExpr): Ast = {
    val valueNode =
      literalExpr match {
        case literal: StringLiteralExpr    => newAnnotationLiteralNode(literal.getValue)
        case literal: IntegerLiteralExpr   => newAnnotationLiteralNode(literal.getValue)
        case literal: BooleanLiteralExpr   => newAnnotationLiteralNode(java.lang.Boolean.toString(literal.getValue))
        case literal: CharLiteralExpr      => newAnnotationLiteralNode(literal.getValue)
        case literal: DoubleLiteralExpr    => newAnnotationLiteralNode(literal.getValue)
        case literal: LongLiteralExpr      => newAnnotationLiteralNode(literal.getValue)
        case _: NullLiteralExpr            => newAnnotationLiteralNode("null")
        case literal: TextBlockLiteralExpr => newAnnotationLiteralNode(literal.getValue)
      }

    Ast(valueNode)
  }

  private def modifiersForTypeDecl(typ: TypeDeclaration[?], isInterface: Boolean): List[NewModifier] = {
    val accessModifierType = if (typ.isPublic) {
      Some(ModifierTypes.PUBLIC)
    } else if (typ.isPrivate) {
      Some(ModifierTypes.PRIVATE)
    } else if (typ.isProtected) {
      Some(ModifierTypes.PROTECTED)
    } else {
      None
    }
    val accessModifier = accessModifierType.map(newModifierNode)

    val abstractModifier =
      Option.when(isInterface || typ.getMethods.asScala.exists(_.isAbstract))(newModifierNode(ModifierTypes.ABSTRACT))

    List(accessModifier, abstractModifier).flatten
  }

  private def astForFieldVariable(v: VariableDeclarator, fieldDeclaration: FieldDeclaration): Ast = {
    // TODO: Should be able to find expected type here
    val annotations = fieldDeclaration.getAnnotations

    val rawTypeName =
      tryWithSafeStackOverflow(v.getTypeAsString).map(Util.stripGenericTypes).getOrElse(NameConstants.Unknown)

    val typeFullName =
      tryWithSafeStackOverflow(v.getType).toOption
        .flatMap(typeInfoCalc.fullName)
        .orElse(scope.lookupType(rawTypeName))
        .getOrElse(s"${Defines.UnresolvedNamespace}.$rawTypeName")

    val name = v.getName.toString
    // Use type name without generics stripped in code
    val variableTypeString = tryWithSafeStackOverflow(v.getTypeAsString).getOrElse("")
    val genericSignature   = binarySignatureCalculator.variableBinarySignature(v.getType)
    val node =
      memberNode(v, name, s"$variableTypeString $name", typeFullName, genericSignature = Option(genericSignature))
    val memberAst      = Ast(node)
    val annotationAsts = annotations.asScala.map(astForAnnotationExpr)

    val fieldDeclModifiers = modifiersForFieldDeclaration(fieldDeclaration)

    scope.enclosingTypeDecl.get.addMember(node, fieldDeclaration.isStatic)

    memberAst
      .withChildren(annotationAsts)
      .withChildren(fieldDeclModifiers)
  }

  private def modifiersForFieldDeclaration(decl: FieldDeclaration): Seq[Ast] = {
    val staticModifier =
      Option.when(decl.isStatic)(newModifierNode(ModifierTypes.STATIC))

    val accessModifierType =
      if (decl.isPublic)
        Some(ModifierTypes.PUBLIC)
      else if (decl.isPrivate)
        Some(ModifierTypes.PRIVATE)
      else if (decl.isProtected)
        Some(ModifierTypes.PROTECTED)
      else
        None

    val accessModifier = accessModifierType.map(newModifierNode)

    List(staticModifier, accessModifier).flatten.map(Ast(_))
  }

  private def createTypeDeclNode(
    typ: TypeDeclaration[?],
    astParentType: String,
    astParentFullName: String,
    isInterface: Boolean,
    fullNameOverride: Option[String]
  ): NewTypeDecl = {
    val baseTypeFullNames = if (typ.isClassOrInterfaceDeclaration) {
      val decl             = typ.asClassOrInterfaceDeclaration()
      val extendedTypes    = decl.getExtendedTypes.asScala
      val implementedTypes = decl.getImplementedTypes.asScala
      val inheritsFromTypeNames =
        (extendedTypes ++ implementedTypes).flatMap { typ =>
          typeInfoCalc.fullName(typ).orElse(scope.lookupType(typ.getNameAsString))
        }
      val maybeJavaObjectType = if (extendedTypes.isEmpty) {
        typeInfoCalc.registerType(TypeConstants.Object)
        Seq(TypeConstants.Object)
      } else {
        Seq()
      }
      maybeJavaObjectType ++ inheritsFromTypeNames
    } else if (typ.isEnumDeclaration) {
      TypeConstants.Enum :: Nil
    } else if (typ.isRecordDeclaration) {
      TypeConstants.Record :: Nil
    } else {
      List.empty[String]
    }

    val (name, fullName) = getTypeDeclNameAndFullName(typ, fullNameOverride)

    val code = codeForTypeDecl(typ, isInterface)

    val genericSignature = binarySignatureCalculator.typeDeclBinarySignature(typ)
    typeDeclNode(
      typ,
      name,
      fullName,
      filename,
      code,
      astParentType,
      astParentFullName,
      baseTypeFullNames,
      genericSignature = Option(genericSignature)
    )
  }

  private def codeForTypeDecl(typ: TypeDeclaration[?], isInterface: Boolean): String = {
    val codeBuilder = new mutable.StringBuilder()
    if (typ.isPublic) {
      codeBuilder.append("public ")
    } else if (typ.isPrivate) {
      codeBuilder.append("private ")
    } else if (typ.isProtected) {
      codeBuilder.append("protected ")
    }

    if (typ.isStatic) {
      codeBuilder.append("static ")
    }

    val classPrefix =
      if (isInterface)
        "interface "
      else if (typ.isEnumDeclaration)
        "enum "
      else
        "class "
    codeBuilder.append(classPrefix)
    codeBuilder.append(typ.getNameAsString)

    codeBuilder.toString()
  }

  private def identifierForResolvedTypeParameter(typeParameter: ResolvedTypeParameterDeclaration): NewIdentifier = {
    val name = typeParameter.getName
    val typeFullName = Try(typeParameter.getUpperBound).toOption
      .flatMap(typeInfoCalc.fullName)
      .getOrElse(TypeConstants.Object)
    typeInfoCalc.registerType(typeFullName)
    newIdentifierNode(name, typeFullName)
  }

  private def addTypeDeclTypeParamsToScope(typ: TypeDeclaration[?]): Unit = {
    val typeParameters = typ match {
      case classOrInterfaceDeclaration: ClassOrInterfaceDeclaration =>
        classOrInterfaceDeclaration.getTypeParameters.asScala
      case recordDeclaration: RecordDeclaration => recordDeclaration.getTypeParameters.asScala
      case _                                    => Nil
    }

    typeParameters.foreach { case typeParam =>
      // TODO: Use typeParam.getTypeBound list to calculate this instead to allow better fallback.
      val typeFullName = tryWithSafeStackOverflow(typeParam.resolve().asTypeParameter().getUpperBound).toOption
        .flatMap(typeInfoCalc.fullName)
        .getOrElse(TypeConstants.Object)
      typeInfoCalc.registerType(typeFullName)

      scope.addTypeParameter(typeParam.getNameAsString, typeFullName)
    }
  }

  private def astForEnumEntry(entry: EnumConstantDeclaration): Ast = {
    // TODO Fix enum entries in general
    val typeFullName =
      tryWithSafeStackOverflow(entry.resolve().getType).toOption.flatMap(typeInfoCalc.fullName)

    val genericSignature = binarySignatureCalculator.enumEntryBinarySignature(entry)
    val entryNode = memberNode(
      entry,
      entry.getNameAsString,
      entry.toString,
      typeFullName.getOrElse("ANY"),
      genericSignature = Some(genericSignature)
    )

    val name = s"${typeFullName.getOrElse(Defines.UnresolvedNamespace)}.${Defines.ConstructorMethodName}"

    Ast(entryNode)
  }
}
