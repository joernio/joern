package io.joern.gosrc2cpg.astcreation

import io.joern.gosrc2cpg.datastructures.PackageMemberAst
import io.joern.gosrc2cpg.parser.ParserAst.Unknown
import io.joern.gosrc2cpg.parser.ParserNodeInfo
import io.joern.x2cpg.astgen.AstGenNodeBuilder
import io.joern.x2cpg.{Ast, AstCreatorBase, ValidationMode, Defines as XDefines}
import io.shiftleft.codepropertygraph.generated.NodeTypes
import org.apache.commons.lang3.StringUtils
import io.shiftleft.codepropertygraph.generated.DiffGraphBuilder
import ujson.Value

import scala.collection.immutable.Set

class AstForPackageConstructorCreator(val pacakgePath: String, statements: Set[PackageMemberAst])(implicit
  withSchemaValidation: ValidationMode
) extends AstCreatorBase(pacakgePath)
    with AstGenNodeBuilder[AstForPackageConstructorCreator] {

  override def createAst(): DiffGraphBuilder = {
    val name       = StringUtils.normalizeSpace(s"$pacakgePath${XDefines.StaticInitMethodName}")
    val node       = ParserNodeInfo(Unknown, Value("{}"), name, Some(0), Some(0), Some(0), Some(0))
    val ctorMethod = methodNode(node, name, name, name, None, pacakgePath, Some(NodeTypes.TYPE_DECL), Some(pacakgePath))
    val blockNode_ = blockNode(node, Defines.empty, Defines.voidTypeName)
    val declsAsts  = statements.map(_.ast).toList
    setArgumentIndices(declsAsts)
    val methodReturn = methodReturnNode(node, Defines.anyTypeName)
    val ctorAst      = methodAst(ctorMethod, Seq.empty, blockAst(blockNode_, declsAsts), methodReturn)
    Ast.storeInDiffGraph(ctorAst, diffGraph)
    diffGraph
  }

}
