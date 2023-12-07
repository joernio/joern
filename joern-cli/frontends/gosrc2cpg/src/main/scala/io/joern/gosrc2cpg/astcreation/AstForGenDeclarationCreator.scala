package io.joern.gosrc2cpg.astcreation

import io.joern.gosrc2cpg.datastructures.GoGlobal
import io.joern.gosrc2cpg.parser.ParserAst.*
import io.joern.gosrc2cpg.parser.{ParserKeys, ParserNodeInfo}
import io.joern.x2cpg
import io.joern.x2cpg.{Ast, ValidationMode}
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, NodeTypes, Operators}
import ujson.Value

import scala.util.{Success, Try}

trait AstForGenDeclarationCreator(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>
  def astForGenDecl(genDecl: ParserNodeInfo, globalStatements: Boolean = false): Seq[Ast] = {
    Try(
      genDecl
        .json(ParserKeys.Specs)
        .arr
    ) match {
      case Success(specArr) =>
        specArr
          .map(createParserNodeInfo)
          .flatMap { genDeclNode =>
            genDeclNode.node match
              case ImportSpec => astForImport(genDeclNode)
              case TypeSpec   => astForTypeSpec(genDeclNode)
              case ValueSpec  => astForValueSpec(genDeclNode, globalStatements = globalStatements)
              case _          => Seq[Ast]()
          }
          .toSeq
      case _ =>
        Seq.empty
    }
  }

  private def astForImport(nodeInfo: ParserNodeInfo): Seq[Ast] = {
    val basicLit                     = createParserNodeInfo(nodeInfo.json(ParserKeys.Path))
    val (importedEntity, importedAs) = processImports(nodeInfo.json)
    val importedAsReplacement        = if (importedEntity.equals(importedAs)) "" else s"$importedAs "
    // This may be better way to add code for import node
    Seq(Ast(newImportNode(s"import $importedAsReplacement$importedEntity", importedEntity, importedAs, basicLit)))
  }

  protected def astForValueSpec(
    valueSpec: ParserNodeInfo,
    recordVar: Boolean = false,
    globalStatements: Boolean = false
  ): Seq[Ast] = {
    val typeFullName = Try(valueSpec.json(ParserKeys.Type)) match
      case Success(typeJson) =>
        val (typeFullName, _, _, _) = processTypeInfo(createParserNodeInfo(typeJson))
        Some(typeFullName)
      case _ => None

    Try(valueSpec.json(ParserKeys.Values).arr.toList) match
      case Success(_) =>
        val (assCallAsts, localAsts) =
          (valueSpec.json(ParserKeys.Names).arr.toList zip valueSpec.json(ParserKeys.Values).arr.toList)
            .map { case (lhs, rhs) => (createParserNodeInfo(lhs), createParserNodeInfo(rhs)) }
            .map { case (lhsParserNode, rhsParserNode) =>
              astForAssignmentCallNode(
                lhsParserNode,
                rhsParserNode,
                typeFullName,
                valueSpec.code,
                recordVar,
                globalStatements
              )
            }
            .unzip
        if globalStatements then Seq.empty else localAsts ++: assCallAsts
      case _ =>
        valueSpec
          .json(ParserKeys.Names)
          .arr
          .flatMap { parserNode =>
            val localParserNode = createParserNodeInfo(parserNode)
            if globalStatements then {
              astForGlobalVarAndConstants(typeFullName.getOrElse(Defines.anyTypeName), localParserNode)
              Seq.empty
            } else {
              Seq(astForLocalNode(localParserNode, typeFullName, recordVar)) ++: astForNode(localParserNode)
            }
          }
          .toSeq

  }

  protected def astForAssignmentCallNode(
    lhsParserNode: ParserNodeInfo,
    rhsParserNode: ParserNodeInfo,
    typeFullName: Option[String],
    code: String,
    recordVar: Boolean = false,
    globalStatements: Boolean = false
  ): (Ast, Ast) = {
    val rhsAst          = astForBooleanLiteral(rhsParserNode)
    val rhsTypeFullName = typeFullName.getOrElse(getTypeFullNameFromAstNode(rhsAst))
    if (globalStatements) {
      astForGlobalVarAndConstants(rhsTypeFullName, lhsParserNode, Some(rhsAst))
      (Ast(), Ast())
    } else {
      val localAst  = astForLocalNode(lhsParserNode, Some(rhsTypeFullName), recordVar)
      val lhsAst    = astForNode(lhsParserNode)
      val arguments = lhsAst ++: rhsAst
      val cNode = callNode(
        rhsParserNode,
        code,
        Operators.assignment,
        Operators.assignment,
        DispatchTypes.STATIC_DISPATCH,
        None,
        Some(rhsTypeFullName)
      )
      (callAst(cNode, arguments), localAst)
    }
  }

  private def astForGlobalVarAndConstants(
    typeFullName: String,
    lhsParserNode: ParserNodeInfo,
    rhsAst: Option[Seq[Ast]] = None
  ): Unit = {
    val name = lhsParserNode.json(ParserKeys.Name).str
    val memberAst = Ast(
      memberNode(lhsParserNode, name, lhsParserNode.code, typeFullName)
        .astParentType(NodeTypes.TYPE_DECL)
        .astParentFullName(fullyQualifiedPackage)
    )
    Ast.storeInDiffGraph(memberAst, diffGraph)
  }

  protected def astForLocalNode(
    localParserNode: ParserNodeInfo,
    typeFullName: Option[String],
    recordVar: Boolean = false
  ): Ast = {
    val name = localParserNode.json(ParserKeys.Name).str
    if name != "_" then {
      val typeFullNameStr = typeFullName.getOrElse(Defines.anyTypeName)
      val node            = localNode(localParserNode, name, localParserNode.code, typeFullNameStr)

      if recordVar then
        GoGlobal.recordStructTypeMemberType(s"$fullyQualifiedPackage${Defines.dot}$name", typeFullNameStr)
      else scope.addToScope(name, (node, typeFullNameStr))
      Ast(node)
    } else {
      Ast()
    }
  }
}
