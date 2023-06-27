package io.joern.gosrc2cpg.astcreation

import io.joern.gosrc2cpg.Config
import io.joern.gosrc2cpg.parser.GoAstJsonParser.ParserResult
import io.joern.x2cpg.AstCreatorBase
import overflowdb.BatchedUpdate
import overflowdb.BatchedUpdate.DiffGraphBuilder

class AstCreator(config: Config, parserResult: ParserResult) extends AstCreatorBase(parserResult.filename) {
  override def createAst(): DiffGraphBuilder = {
    diffGraph
  }
}
