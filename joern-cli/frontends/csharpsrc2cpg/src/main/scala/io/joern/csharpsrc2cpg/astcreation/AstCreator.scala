package io.joern.csharpsrc2cpg.astcreation

import io.joern.csharpsrc2cpg.parser.DotNetNodeInfo
import io.joern.x2cpg.astgen.{AstGenNodeBuilder, ParserResult}
import io.joern.x2cpg.{Ast, AstCreatorBase, ValidationMode}
import io.shiftleft.codepropertygraph.generated.nodes.NewFile
import io.shiftleft.utils.IOUtils
import overflowdb.BatchedUpdate.DiffGraphBuilder

import java.nio.file.Paths

class AstCreator(val relativeFileName: String, val parserResult: ParserResult)(implicit
  withSchemaValidation: ValidationMode
) extends AstCreatorBase(relativeFileName)
    with AstGenNodeBuilder[AstCreator] {

  override def createAst(): DiffGraphBuilder = {
    val fileNode = NewFile().name(relativeFileName).content(parserResult.fileContent).order(1)
    val ast      = Ast(fileNode)
    Ast.storeInDiffGraph(ast, diffGraph)
    diffGraph
  }

}
