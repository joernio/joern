package io.joern.rubysrc2cpg.astcreation

import io.joern.x2cpg.{AstCreatorBase, ValidationMode}
import io.joern.x2cpg.astgen.{AstGenNodeBuilder, ParserResult}
import io.shiftleft.codepropertygraph.generated.DiffGraphBuilder

class AstGenCreator(val fileName: String, val parserResult: ParserResult)(implicit withSchemaValidation: ValidationMode)
    extends AstCreatorBase(fileName) {
  override def createAst(): DiffGraphBuilder = diffGraph

}
