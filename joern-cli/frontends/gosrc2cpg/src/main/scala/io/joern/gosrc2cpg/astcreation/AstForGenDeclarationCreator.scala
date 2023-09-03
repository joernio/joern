package io.joern.gosrc2cpg.astcreation

import io.joern.gosrc2cpg.parser.ParserAst.*
import io.joern.gosrc2cpg.parser.{ParserKeys, ParserNodeInfo}
import io.joern.x2cpg
import io.joern.x2cpg.datastructures.Stack.StackWrapper
import io.joern.x2cpg.{Ast, ValidationMode}
import io.shiftleft.codepropertygraph.generated.nodes.NewTypeDecl
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, EdgeTypes, Operators}
import ujson.Value

import scala.util.Try

trait AstForGenDeclarationCreator(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>
  def astForGenDecl(genDecl: ParserNodeInfo): Seq[Ast] = {
    genDecl
      .json(ParserKeys.Specs)
      .arr
      .map(createParserNodeInfo)
      .flatMap { genDeclNode =>
        genDeclNode.node match
          case ImportSpec => astForImport(genDeclNode)
          case TypeSpec   => astForTypeSpec(genDeclNode)
          case ValueSpec  => astForValueSpec(genDeclNode)
          case _          => Seq[Ast]()
      }
      .toSeq
  }

  private def astForImport(nodeInfo: ParserNodeInfo): Seq[Ast] = {
    val basicLit       = createParserNodeInfo(nodeInfo.json(ParserKeys.Path))
    val importedEntity = nodeInfo.json(ParserKeys.Path).obj(ParserKeys.Value).str.replaceAll("\"", "")
    val importedAs =
      Try(nodeInfo.json(ParserKeys.Name).obj(ParserKeys.Name).str).toOption
        .getOrElse(importedEntity.split("/").last)
    aliasToNameSpaceMapping.put(importedAs, importedEntity)
    val importedAsReplacement = if (importedEntity.equals(importedAs)) "" else s"$importedAs "
    // This may be better way to add code for import node
    Seq(Ast(newImportNode(s"import $importedAsReplacement$importedEntity", importedEntity, importedAs, basicLit)))
  }

  private def astForValueSpec(valueSpec: ParserNodeInfo): Seq[Ast] = {
    // If global array is initialized, type info is present in values > type
    val typeJson = Try(valueSpec.json(ParserKeys.Type))
      .getOrElse(valueSpec.json(ParserKeys.Values).arr.head)
    val typeInfoNode = createParserNodeInfo(typeJson)

    val arrayInitializerNode: Seq[Ast] = typeInfoNode.node match
      case ArrayType =>
        Seq(astForArrayInitializer(typeInfoNode))
      case _ =>
        Seq.empty

    val localNodes = valueSpec.json(ParserKeys.Names).arr.flatMap { parserNode =>
      val localParserNode                                    = createParserNodeInfo(parserNode)
      val name                                               = parserNode(ParserKeys.Name).str
      val (typeFullName, typeFullNameForcode, isVariadic, _) = processTypeInfo(typeInfoNode)
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
      localNodes.toList ::: callNodes ::: arrayInitializerNode.toList
    } else
      localNodes.toList ++ arrayInitializerNode.toList
  }
}
