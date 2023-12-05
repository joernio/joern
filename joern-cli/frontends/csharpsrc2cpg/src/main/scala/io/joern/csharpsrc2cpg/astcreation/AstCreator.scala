package io.joern.csharpsrc2cpg.astcreation

import io.joern.csharpsrc2cpg.parser.DotNetNodeInfo
import io.joern.x2cpg.astgen.{AstGenNodeBuilder, ParserResult}
import io.joern.x2cpg.{AstCreatorBase, ValidationMode}
import overflowdb.BatchedUpdate.DiffGraphBuilder
class AstCreator(val relativeFileName: String, val parserResult: ParserResult)(implicit
  withSchemaValidation: ValidationMode
) extends AstCreatorBase(relativeFileName)
    with AstGenNodeBuilder[AstCreator] {

  override def createAst(): DiffGraphBuilder = {
    // TODO: Generate AST
    diffGraph
  }

}
