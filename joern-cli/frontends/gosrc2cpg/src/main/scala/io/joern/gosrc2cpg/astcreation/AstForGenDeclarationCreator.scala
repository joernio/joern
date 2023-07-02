package io.joern.gosrc2cpg.astcreation

import io.joern.gosrc2cpg.parser.{ParserKeys, ParserNodeInfo}
import io.joern.x2cpg.Ast
import io.joern.gosrc2cpg.parser.ParserAst._

import scala.util.Try

trait AstForGenDeclarationCreator { this: AstCreator =>
  def astForGenDecl(genDecl: ParserNodeInfo): Seq[Ast] = {
    genDecl.json(ParserKeys.Tok).str match {
      case "import" => astForImport(genDecl)
      case "const"  => List[Ast]()
      case "type"   => List[Ast]()
      case "var"    => List[Ast]()
    }
  }

  def astForImport(imports: ParserNodeInfo): Seq[Ast] = {
    imports
      .json(ParserKeys.Specs)
      .arr
      .map(createParserNodeInfo)
      .map { nodeInfo =>
        nodeInfo.node match {
          case ImportSpec =>
            val basicLit       = createParserNodeInfo(nodeInfo.json(ParserKeys.Path))
            val importedEntity = nodeInfo.json(ParserKeys.Path).obj(ParserKeys.Value).str
            val importedAs =
              Try(nodeInfo.json(ParserKeys.Name).obj(ParserKeys.Name).str).toOption.getOrElse(importedEntity)
            val importedAsReplacement = if (importedEntity.equals(importedAs)) "" else s"$importedAs "
            // This may be better way to add code for import node
            Ast(newImportNode(s"import $importedAsReplacement$importedEntity", importedEntity, importedAs, basicLit))
        }
      }
      .toList
  }
}
