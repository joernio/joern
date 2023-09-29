package io.joern.gosrc2cpg.astcreation

import io.joern.gosrc2cpg.parser.ParserAst.*
import io.joern.gosrc2cpg.parser.{ParserKeys, ParserNodeInfo}
import io.joern.x2cpg
import io.joern.x2cpg.{Ast, ValidationMode}
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}
import ujson.Value

import scala.util.{Success, Try}

trait AstForGenDeclarationCreator(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>
  def astForGenDecl(genDecl: ParserNodeInfo): Seq[Ast] = {
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
              case ValueSpec  => astForValueSpec(genDeclNode)
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

  private def astForValueSpec(valueSpec: ParserNodeInfo): Seq[Ast] = {
    val typeFullName = Try(valueSpec.json(ParserKeys.Type)) match
      case Success(typeJson) =>
        val typeInfoNode            = createParserNodeInfo(typeJson)
        val (typeFullName, _, _, _) = processTypeInfo(typeInfoNode)
        Some(typeFullName)
      case _ => None

    Try(valueSpec.json(ParserKeys.Values).arr.toList) match
      case Success(_) =>
        val (assCallAsts, localAsts) =
          (valueSpec.json(ParserKeys.Names).arr.toList zip valueSpec.json(ParserKeys.Values).arr.toList)
            .map { case (lhs, rhs) => (createParserNodeInfo(lhs), createParserNodeInfo(rhs)) }
            .map { case (lhsParserNode, rhsParserNode) =>
              astForAssignmentCallNode(lhsParserNode, rhsParserNode, typeFullName, valueSpec.code)
            }
            .unzip
        localAsts ++: assCallAsts
      case _ =>
        valueSpec
          .json(ParserKeys.Names)
          .arr
          .flatMap { parserNode =>
            val localParserNode = createParserNodeInfo(parserNode)
            Seq(astForLocalNode(localParserNode, typeFullName)) ++: astForNode(localParserNode)
          }
          .toSeq

  }

  protected def astForAssignmentCallNode(
    lhsParserNode: ParserNodeInfo,
    rhsParserNode: ParserNodeInfo,
    typeFullName: Option[String],
    code: String
  ): (Ast, Ast) = {
    val rhsAst          = astForBooleanLiteral(rhsParserNode)
    val rhsTypeFullName = typeFullName.getOrElse(getTypeFullNameFromAstNode(rhsAst))
    val localAst        = astForLocalNode(lhsParserNode, Some(rhsTypeFullName))
    val lhsAst          = astForNode(lhsParserNode)
    val arguments       = lhsAst ++: rhsAst
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

  protected def astForLocalNode(localParserNode: ParserNodeInfo, typeFullName: Option[String]): Ast = {
    val name = localParserNode.json(ParserKeys.Name).str
    if name != "_" then {
      val node = localNode(localParserNode, name, localParserNode.code, typeFullName.getOrElse(Defines.anyTypeName))
      scope.addToScope(name, (node, typeFullName.getOrElse(Defines.anyTypeName)))
      Ast(node)
    } else {
      Ast()
    }
  }
}
