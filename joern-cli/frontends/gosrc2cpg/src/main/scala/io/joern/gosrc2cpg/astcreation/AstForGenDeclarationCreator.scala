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
        typeFullName
      case _ => Defines.anyTypeName

    val localNodes = valueSpec.json(ParserKeys.Names).arr.flatMap { parserNode =>
      val localParserNode = createParserNodeInfo(parserNode)
      val name            = parserNode(ParserKeys.Name).str

      val node = localNode(localParserNode, name, localParserNode.code, typeFullName)
      scope.addToScope(name, (node, typeFullName))
      val identifierAst = if (valueSpec.json(ParserKeys.Values).isNull) then astForNode(localParserNode) else Seq.empty
      identifierAst ++: Seq(Ast(node))
    }

    if (!valueSpec.json(ParserKeys.Values).isNull) {
      val callNodes =
        (valueSpec.json(ParserKeys.Names).arr.toList zip valueSpec.json(ParserKeys.Values).arr.toList)
          .map { case (lhs, rhs) => (createParserNodeInfo(lhs), createParserNodeInfo(rhs)) }
          .map { case (lhsParserNode, rhsParserNode) =>
            val cNode = callNode(
              rhsParserNode,
              lhsParserNode.code + rhsParserNode.code,
              Operators.assignment,
              Operators.assignment,
              DispatchTypes.STATIC_DISPATCH
            )
            val arguments = astForNode(lhsParserNode) ++: astForNode(rhsParserNode)
            callAst(cNode, arguments)
          }
      localNodes.toList ::: callNodes
    } else
      localNodes.toList
  }
}
