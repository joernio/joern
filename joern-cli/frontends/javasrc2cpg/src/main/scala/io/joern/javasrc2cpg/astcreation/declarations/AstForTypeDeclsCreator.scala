package io.joern.javasrc2cpg.astcreation.declarations

import com.github.javaparser.ast.body.{
  AnnotationDeclaration,
  BodyDeclaration,
  ClassOrInterfaceDeclaration,
  ConstructorDeclaration,
  EnumConstantDeclaration,
  FieldDeclaration,
  InitializerDeclaration,
  MethodDeclaration,
  TypeDeclaration,
  VariableDeclarator
}
import com.github.javaparser.resolution.declarations.ResolvedTypeParameterDeclaration
import io.joern.javasrc2cpg.astcreation.AstCreator
import io.joern.javasrc2cpg.astcreation.declarations.AstForTypeDeclsCreator.AstWithStaticInit
import io.joern.javasrc2cpg.typesolvers.TypeInfoCalculator.TypeConstants
import io.joern.javasrc2cpg.util.{BindingTable, BindingTableEntry}
import io.joern.x2cpg.utils.NodeBuilders.*
import io.joern.x2cpg.{Ast, Defines}
import io.shiftleft.codepropertygraph.generated.nodes.{NewIdentifier, NewMethod, NewModifier, NewTypeDecl}
import io.shiftleft.codepropertygraph.generated.{ModifierTypes, NodeTypes}
import org.slf4j.LoggerFactory

import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.util.{Success, Try}

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

  def astForTypeDecl(typ: TypeDeclaration[_], astParentType: String, astParentFullName: String): Ast = {
    val isInterface = typ match {
      case classDeclaration: ClassOrInterfaceDeclaration => classDeclaration.isInterface
      case _                                             => false
    }

    val typeDeclNode = createTypeDeclNode(typ, astParentType, astParentFullName, isInterface)

    scope.pushTypeDeclScope(typeDeclNode)
    addTypeDeclTypeParamsToScope(typ)

    val enumEntryAsts = if (typ.isEnumDeclaration) {
      typ.asEnumDeclaration().getEntries.asScala.map(astForEnumEntry).toList
    } else {
      List.empty
    }

    val staticInits: mutable.Buffer[Ast] = mutable.Buffer()
    val memberAsts = typ.getMembers.asScala.flatMap { member =>
      val astWithInits =
        astForTypeDeclMember(member, astParentFullName = NodeTypes.TYPE_DECL)
      staticInits.appendAll(astWithInits.staticInits)
      astWithInits.ast
    }

    val defaultConstructorAst = if (!isInterface && typ.getConstructors.isEmpty) {
      Some(astForDefaultConstructor())
    } else {
      None
    }

    val annotationAsts = typ.getAnnotations.asScala.map(astForAnnotationExpr)

    val clinitAst = clinitAstFromStaticInits(staticInits.toSeq)

    val localDecls    = scope.localDeclsInScope
    val lambdaMethods = scope.lambdaMethodsInScope

    val modifiers = modifiersForTypeDecl(typ, isInterface)

    val typeDeclAst = Ast(typeDeclNode)
      .withChildren(enumEntryAsts)
      .withChildren(memberAsts)
      .withChildren(defaultConstructorAst.toList)
      .withChildren(annotationAsts)
      .withChildren(clinitAst.toSeq)
      .withChildren(localDecls)
      .withChildren(lambdaMethods)
      .withChildren(modifiers.map(Ast(_)))

    val defaultConstructorBindingEntry =
      defaultConstructorAst
        .flatMap(_.root)
        .collect { case defaultConstructor: NewMethod =>
          BindingTableEntry(
            io.joern.x2cpg.Defines.ConstructorMethodName,
            defaultConstructor.signature,
            defaultConstructor.fullName
          )
        }

    // Annotation declarations need no binding table as objects of this
    // typ never get called from user code.
    // Furthermore the parser library throws an exception when trying to
    // access e.g. the declared methods of an annotation declaration.
    if (!typ.isInstanceOf[AnnotationDeclaration]) {
      tryWithSafeStackOverflow(typ.resolve()).toOption.foreach { resolvedTypeDecl =>
        val bindingTable = getBindingTable(resolvedTypeDecl)
        defaultConstructorBindingEntry.foreach(bindingTable.add)
        BindingTable.createBindingNodes(diffGraph, typeDeclNode, bindingTable)
      }
    }

    scope.popScope()

    typeDeclAst
  }

  private def modifiersForTypeDecl(typ: TypeDeclaration[_], isInterface: Boolean): List[NewModifier] = {
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

  private def astForTypeDeclMember(member: BodyDeclaration[_], astParentFullName: String): AstWithStaticInit = {
    member match {
      case constructor: ConstructorDeclaration =>
        val ast = astForConstructor(constructor)

        AstWithStaticInit(ast)

      case method: MethodDeclaration =>
        val ast = astForMethod(method)

        AstWithStaticInit(ast)

      case typeDeclaration: TypeDeclaration[_] =>
        AstWithStaticInit(astForTypeDecl(typeDeclaration, NodeTypes.TYPE_DECL, astParentFullName))

      case fieldDeclaration: FieldDeclaration =>
        val memberAsts = fieldDeclaration.getVariables.asScala.toList.map { variable =>
          astForFieldVariable(variable, fieldDeclaration)
        }

        val assignments = assignmentsForVarDecl(
          fieldDeclaration.getVariables.asScala.toList,
          line(fieldDeclaration),
          column(fieldDeclaration)
        )

        val staticInitAsts = if (fieldDeclaration.isStatic) assignments else Nil
        if (!fieldDeclaration.isStatic) scope.addMemberInitializers(assignments)

        AstWithStaticInit(memberAsts, staticInitAsts)

      case initDeclaration: InitializerDeclaration =>
        val stmts = initDeclaration.getBody.getStatements
        val asts  = stmts.asScala.flatMap(astsForStatement).toList
        AstWithStaticInit(ast = Seq.empty, staticInits = asts)

      case unhandled =>
        // AnnotationMemberDeclarations and InitializerDeclarations as children of typeDecls are the
        // expected cases.
        logger.info(s"Found unhandled typeDecl member ${unhandled.getClass} in file $filename")
        AstWithStaticInit.empty
    }
  }

  private def astForFieldVariable(v: VariableDeclarator, fieldDeclaration: FieldDeclaration): Ast = {
    // TODO: Should be able to find expected type here
    val annotations = fieldDeclaration.getAnnotations

    // variable can be declared with generic type, so we need to get rid of the <> part of it to get the package information
    // and append the <> when forming the typeFullName again
    // Ex - private Consumer<String, Integer> consumer;
    // From Consumer<String, Integer> we need to get to Consumer so splitting it by '<' and then combining with '<' to
    // form typeFullName as Consumer<String, Integer>

    val typeFullNameWithoutGenericSplit = typeInfoCalc
      .fullName(v.getType)
      .orElse(scope.lookupType(v.getTypeAsString))
      .getOrElse(s"${Defines.UnresolvedNamespace}.${v.getTypeAsString}")
    val typeFullName = {
      // Check if the typeFullName is unresolved and if it has generic information to resolve the typeFullName
      if (
        typeFullNameWithoutGenericSplit
          .contains(Defines.UnresolvedNamespace) && v.getTypeAsString.contains(Defines.LeftAngularBracket)
      ) {
        val splitByLeftAngular = v.getTypeAsString.split(Defines.LeftAngularBracket)
        scope.lookupType(splitByLeftAngular.head) match {
          case Some(fullName) =>
            fullName + splitByLeftAngular
              .slice(1, splitByLeftAngular.size)
              .mkString(Defines.LeftAngularBracket, Defines.LeftAngularBracket, "")
          case None => typeFullNameWithoutGenericSplit
        }
      } else typeFullNameWithoutGenericSplit
    }
    val name           = v.getName.toString
    val node           = memberNode(v, name, s"$typeFullName $name", typeFullName)
    val memberAst      = Ast(node)
    val annotationAsts = annotations.asScala.map(astForAnnotationExpr)

    val fieldDeclModifiers = modifiersForFieldDeclaration(fieldDeclaration)

    scope.addMember(node, fieldDeclaration.isStatic)

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
    typ: TypeDeclaration[_],
    astParentType: String,
    astParentFullName: String,
    isInterface: Boolean
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
    } else {
      List.empty[String]
    }

    val resolvedType    = tryWithSafeStackOverflow(typ.resolve()).toOption
    val defaultFullName = s"${Defines.UnresolvedNamespace}.${typ.getNameAsString}"
    val name            = resolvedType.flatMap(typeInfoCalc.name).getOrElse(typ.getNameAsString)
    val typeFullName    = resolvedType.flatMap(typeInfoCalc.fullName).getOrElse(defaultFullName)

    val code = codeForTypeDecl(typ, isInterface)

    NewTypeDecl()
      .name(name)
      .fullName(typeFullName)
      .lineNumber(line(typ))
      .columnNumber(column(typ))
      .inheritsFromTypeFullName(baseTypeFullNames)
      .filename(filename)
      .code(code)
      .astParentType(astParentType)
      .astParentFullName(astParentFullName)
  }

  private def codeForTypeDecl(typ: TypeDeclaration[_], isInterface: Boolean): String = {
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

  private def addTypeDeclTypeParamsToScope(typ: TypeDeclaration[_]): Unit = {
    tryWithSafeStackOverflow(typ.resolve()).map(_.getTypeParameters.asScala) match {
      case Success(resolvedTypeParams) =>
        resolvedTypeParams
          .map(identifierForResolvedTypeParameter)
          .foreach { typeParamIdentifier =>
            scope.addType(typeParamIdentifier.name, typeParamIdentifier.typeFullName)
          }

      case _ => // Nothing to do here
    }
  }

  private def astForEnumEntry(entry: EnumConstantDeclaration): Ast = {
    // TODO Fix enum entries in general
    val typeFullName =
      tryWithSafeStackOverflow(entry.resolve().getType).toOption.flatMap(typeInfoCalc.fullName)

    val entryNode = memberNode(entry, entry.getNameAsString, entry.toString, typeFullName.getOrElse("ANY"))

    val name = s"${typeFullName.getOrElse(Defines.UnresolvedNamespace)}.${Defines.ConstructorMethodName}"

    Ast(entryNode)
  }
}
