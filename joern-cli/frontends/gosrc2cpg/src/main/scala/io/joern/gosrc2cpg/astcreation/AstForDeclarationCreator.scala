package io.joern.gosrc2cpg.astcreation

import io.joern.gosrc2cpg.parser.{ParserKeys, ParserNodeInfo}
import io.joern.x2cpg.Ast
import io.joern.gosrc2cpg.parser.ParserAst._

import scala.util.Try

trait AstForDeclarationCreator { this: AstCreator =>
  def astForGenDecl(genDecl: ParserNodeInfo): Ast = {

    val genDeclAst = genDecl.json(ParserKeys.Specs).arr.map(createParserNodeInfo).flatMap { nodeInfo =>
      nodeInfo.node match {
        case ImportSpec =>
          val basicLit       = createParserNodeInfo(nodeInfo.json(ParserKeys.Path))
          val importedEntity = nodeInfo.json(ParserKeys.Path).obj(ParserKeys.Value).str
          val importedAs =
            Try(nodeInfo.json(ParserKeys.Name).obj(ParserKeys.Name).str).toOption.getOrElse(importedEntity)
          val importedAsReplacement = if (importedEntity.equals(importedAs)) "" else s"$importedAs "
          // This may be better way to add code for import node
          val importNode =
            newImportNode(s"import $importedAsReplacement$importedEntity", importedEntity, importedAs, basicLit)

          // Adding import node directly because it is not a Ast Node
          diffGraph.addNode(importNode)
          None
        case _ => None
      }
    }
    if (genDeclAst.isEmpty)
      Ast()
    else if (genDeclAst.size == 1)
      genDeclAst.head
    else
      blockAst(blockNode(genDecl), genDeclAst.toList)
  }

}
